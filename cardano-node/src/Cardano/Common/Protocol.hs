{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Common.Protocol
  ( Protocol(..)
  , ProtocolExceptions (..)
  , SomeProtocol(..)
  , fromProtocol
  ) where



import           Cardano.Prelude hiding (show)
import           Prelude (error, show)
import           Test.Cardano.Prelude (canonicalDecodePretty)

import           Codec.CBOR.Read (deserialiseFromBytes, DeserialiseFailure)
import qualified Data.ByteString.Lazy as LB
import           Data.Text (pack, unpack)
import           Data.Maybe (maybe)

import           Cardano.Binary (Raw)
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Crypto.Signing as Signing
import           Ouroboros.Consensus.Block (Header(..))
import           Ouroboros.Consensus.Demo
                   ( defaultDemoPBftParams, defaultDemoPraosParams
                   , defaultSecurityParam)
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Mempool.API
                   ( ApplyTxErr, GenTx, GenTxId)
import           Ouroboros.Consensus.Node.ProtocolInfo
                   ( PBftLeaderCredentials, PBftLeaderCredentialsError
                   , PBftSignatureThreshold(..), mkPBftLeaderCredentials)
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util (Dict(..))
import           Ouroboros.Consensus.Util.Condense (Condense)
import           Ouroboros.Network.Block (ChainHash, HeaderHash)

import           Cardano.Common.LocalSocket
import           Cardano.Common.Orphans ()
import           Cardano.Config.Types
                   ( CardanoConfiguration (..), Core (..)
                   , RequireNetworkMagic (..))
import           Cardano.Tracing.Tracers (TraceConstraints)

-------------------------------------------------------------------------------
-- Untyped/typed protocol boundary
-------------------------------------------------------------------------------

data Protocol =
    ByronLegacy
  | BFT
  | Praos
  | MockPBFT
  | RealPBFT
  deriving Show


data ProtocolExceptions = AbstractHashDecodeFailure Text
                        | CanonicalDecodeFailure Text
                        | PbftLeaderCredentialsFailure PBftLeaderCredentialsError
                        | SigningKeyDeserializeFailure DeserialiseFailure

instance Exception ProtocolExceptions

instance Show ProtocolExceptions where
  show (AbstractHashDecodeFailure err) = concat
    [ "Cardano.Common.Protocol.AbstractHashDecodeFailure: "
    , unpack err
    ]
  show (CanonicalDecodeFailure err) = concat
    [ "Cardano.Common.Protocol.CanonicalDecodeFailure: "
    , show err
    ]
  show (PbftLeaderCredentialsFailure err) = concat
    [ "Cardano.Common.Protocol.PbftLeaderCredentialsFailure: "
    , show err
    ]
  show (SigningKeyDeserializeFailure dsF) = concat
    [ "Cardano.Common.Protocol.SigningKeyDeserializeFailure: "
    , show dsF
    ]


data SomeProtocol where
  SomeProtocol :: (RunDemo blk, TraceConstraints blk)
               => Consensus.Protocol blk -> SomeProtocol

-- | Check if a protocol exists.
checkProtocol
  :: forall blk.
  ( Condense blk, Condense (HeaderHash blk)
  , Condense [blk], Condense (ChainHash blk), Condense (Header blk)
  , Condense (GenTx blk),RunDemo blk, Show (ApplyTxErr blk), Show blk
  , Show (GenTx blk), Show (GenTxId blk), Show (Header blk))
  => Consensus.Protocol blk -> SomeProtocol
checkProtocol protocol =
  if Consensus.runProtocol protocol == Dict
    then SomeProtocol protocol
    else panic . pack $ "Cardano.Common.Protocol.checkProtocol:\
                        \This should be impossible"

fromProtocol :: CardanoConfiguration -> Protocol -> IO SomeProtocol
fromProtocol _ ByronLegacy = error "Byron Legacy protocol is not implemented."
fromProtocol _ BFT = pure . checkProtocol $ Consensus.ProtocolMockBFT defaultSecurityParam
fromProtocol _ Praos = pure . checkProtocol $ Consensus.ProtocolMockPraos defaultDemoPraosParams
fromProtocol _ MockPBFT = pure . checkProtocol $ Consensus.ProtocolMockPBFT defaultDemoPBftParams
fromProtocol cc RealPBFT = do
    -- Genesis hash
    genHash <- eitherThrow
                 AbstractHashDecodeFailure
                 (Crypto.decodeAbstractHash (coGenesisHash core) :: Either Text (Crypto.Hash Raw))

    gConfigEither <- runExceptT $ Genesis.mkConfigFromFile (cvtRNM rnm) gFp genHash

    -- GenesisConfig
    gc <- either throwIO return gConfigEither
    eCred <- readLeaderCredentials gc core

    optionalLeaderCredentials <- eitherThrow PbftLeaderCredentialsFailure eCred
    let

        -- TODO: The plumbing here to make the PBFT options from the
        -- CardanoConfiguration is subtle, it should have its own function
        -- to do this, along with other config conversion plumbing:
        p = Consensus.ProtocolRealPBFT
              gc
              (fmap PBftSignatureThreshold (coPBftSigThd core))
              defProtoVer
              defSoftVer
              (Just optionalLeaderCredentials)

    case Consensus.runProtocol p of
      Dict -> pure $ SomeProtocol p
 where
  core :: Core
  core = ccCore cc
  -- Genesis filepath
  gFp :: FilePath
  gFp = coGenesisFile core
  rnm :: RequireNetworkMagic
  rnm = coRequiresNetworkMagic core
  cvtRNM :: RequireNetworkMagic -> Crypto.RequiresNetworkMagic
  cvtRNM NoRequireNetworkMagic = Crypto.RequiresNoMagic
  cvtRNM RequireNetworkMagic   = Crypto.RequiresMagic
  -- TODO:  make configurable via CLI (requires cardano-shell changes)
  -- These defaults are good for mainnet.
  defSoftVer :: Update.SoftwareVersion
  defSoftVer  = Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1
  defProtoVer :: Update.ProtocolVersion
  defProtoVer = Update.ProtocolVersion 0 2 0

readLeaderCredentials
  :: Genesis.Config -> Core
  -> IO (Either PBftLeaderCredentialsError PBftLeaderCredentials)
readLeaderCredentials gc core = do
    signingKeyFileBytes <- LB.readFile signingKeyFile
    delegCertFileBytes  <- LB.readFile delegCertFile

    signingKey <- eitherThrow SigningKeyDeserializeFailure $
                    deserialiseSigningKey signingKeyFileBytes

    delegCert <- eitherThrow CanonicalDecodeFailure $
                    canonicalDecodePretty delegCertFileBytes

    pure $ mkPBftLeaderCredentials gc signingKey delegCert
  where
    signingKeyFile :: FilePath
    signingKeyFile =
      maybe
        (panic "Cardano.Common.Protocol.signingKeyFile: No path given")
        identity
        $ coStaticKeySigningKeyFile core
    delegCertFile :: FilePath
    delegCertFile =
      maybe
        (panic "Cardano.Common.Protocol.delegCertFile: No path given")
        identity
        $ coStaticKeyDlgCertFile core
    deserialiseSigningKey :: LB.ByteString
                          -> Either DeserialiseFailure Signing.SigningKey
    deserialiseSigningKey key =
      fmap (Signing.SigningKey . snd)
      $ deserialiseFromBytes Signing.fromCBORXPrv key
