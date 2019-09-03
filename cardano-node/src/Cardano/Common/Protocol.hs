{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}

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
import           Data.Text (unpack)

import           Cardano.Binary (Raw)
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import           Cardano.Crypto (Hash, RequiresNetworkMagic (..), decodeAbstractHash)
import qualified Cardano.Crypto.Signing as Signing
import           Ouroboros.Consensus.Demo (defaultDemoPBftParams, defaultDemoPraosParams, defaultSecurityParam)
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Node.ProtocolInfo ( PBftLeaderCredentials
                                                       , PBftLeaderCredentialsError
                                                       , PBftSignatureThreshold(..)
                                                       , mkPBftLeaderCredentials
                                                       )
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util (Dict(..))

import           Cardano.Common.Orphans ()
import           Cardano.Config.Types
                   ( CardanoConfiguration (..), Core (..)
                   , RequireNetworkMagic (..) )
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

fromProtocol :: CardanoConfiguration -> Protocol -> IO SomeProtocol
fromProtocol _ ByronLegacy =
  error "Byron Legacy protocol is not implemented."
fromProtocol _ BFT =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = Consensus.ProtocolMockBFT defaultSecurityParam
fromProtocol _ Praos =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = Consensus.ProtocolMockPraos defaultDemoPraosParams
fromProtocol _ MockPBFT =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = Consensus.ProtocolMockPBFT defaultDemoPBftParams
fromProtocol CardanoConfiguration{ccCore} RealPBFT = do
    let Core{ coGenesisFile
            , coGenesisHash
            , coPBftSigThd
            } = ccCore
        genHashE = decodeAbstractHash coGenesisHash :: Either Text (Hash Raw)
        cvtRNM :: RequireNetworkMagic -> RequiresNetworkMagic
        cvtRNM NoRequireNetworkMagic = RequiresNoMagic
        cvtRNM RequireNetworkMagic   = RequiresMagic

    genHash <- case genHashE of
                 Left err -> throwIO $ AbstractHashDecodeFailure err
                 Right g -> pure g
    gcE <- runExceptT (Genesis.mkConfigFromFile (cvtRNM $ coRequiresNetworkMagic ccCore) coGenesisFile genHash)
    gc <- case gcE of
          Left err -> throwIO err
          Right x -> pure x

    optionalLeaderCredentials <- readLeaderCredentials gc ccCore

    let
        -- TODO:  make configurable via CLI (requires cardano-shell changes)
        -- These defaults are good for mainnet.
        defSoftVer  = Update.SoftwareVersion (Update.ApplicationName "cardano-sl") 1
        defProtoVer = Update.ProtocolVersion 0 2 0
        -- TODO: The plumbing here to make the PBFT options from the
        -- CardanoConfiguration is subtle, it should have its own function
        -- to do this, along with other config conversion plumbing:
        p = Consensus.ProtocolRealPBFT
              gc
              (fmap PBftSignatureThreshold coPBftSigThd)
              defProtoVer
              defSoftVer
              optionalLeaderCredentials

    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p

readLeaderCredentials :: Genesis.Config -> Core -> IO (Maybe PBftLeaderCredentials)
readLeaderCredentials gc Core {
                           coStaticKeySigningKeyFile = Just signingKeyFile
                         , coStaticKeyDlgCertFile    = Just delegCertFile
                         } = do
    signingKeyFileBytes <- LB.readFile signingKeyFile
    delegCertFileBytes  <- LB.readFile delegCertFile

    signingKey <- either (throwIO . SigningKeyDeserializeFailure) return $
                    deserialiseSigningKey signingKeyFileBytes

    delegCert <- either (throwIO . CanonicalDecodeFailure) return $
                    canonicalDecodePretty delegCertFileBytes

    either (throwIO . PbftLeaderCredentialsFailure) (return . Just)
           (mkPBftLeaderCredentials gc signingKey delegCert)
  where
    deserialiseSigningKey :: LB.ByteString
                          -> Either DeserialiseFailure Signing.SigningKey
    deserialiseSigningKey =
        fmap (Signing.SigningKey . snd)
      . deserialiseFromBytes Signing.fromCBORXPrv

--TODO: fail noisily if only one file is specified without the other
-- since that's obviously a user error.
readLeaderCredentials _gc _ = return Nothing
