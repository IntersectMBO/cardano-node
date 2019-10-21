{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes       #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Protocol
  ( Protocol(..)
  , SomeProtocol(..)
  , withRealPBFT
  , fromProtocol
  ) where



import           Cardano.Prelude
import           Prelude (error, fail)
import           Test.Cardano.Prelude (canonicalDecodePretty)

import           Codec.CBOR.Read (deserialiseFromBytes, DeserialiseFailure)
import           Control.Exception hiding (throwIO)
import qualified Data.ByteString.Lazy as LB
import           Data.Text (unpack)

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import           Cardano.Crypto (RequiresNetworkMagic (..), decodeAbstractHash)
import qualified Cardano.Crypto.Signing as Signing
import           Cardano.Shell.Lib (GeneralException (..))
import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Demo (defaultDemoPBftParams, defaultDemoPraosParams, defaultSecurityParam)
import qualified Ouroboros.Consensus.Demo.Run as Demo
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId)
import           Ouroboros.Consensus.Node.ProtocolInfo (PBftLeaderCredentials,
                                                        PBftSignatureThreshold(..),
                                                         mkPBftLeaderCredentials)
import           Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)
import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB)
import           Ouroboros.Consensus.Node.ProtocolInfo (PBftLeaderCredentials, PBftSignatureThreshold(..), mkPBftLeaderCredentials)
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util (Dict(..))
import           Ouroboros.Network.Block

import           Cardano.Config.Types
                   ( CardanoConfiguration (..), Core (..)
                   , Protocol (..),  RequireNetworkMagic (..) )

-- TODO: consider not throwing this, or wrap it in a local error type here
-- that has proper error messages.
instance Exception Genesis.ConfigurationError

type TraceConstraints blk =
    ( Condense blk
    , Condense [blk]
    , Condense (ChainHash blk)
    , Condense (Header blk)
    , Condense (HeaderHash blk)
    , Condense (GenTx blk)
    , Show (ApplyTxErr blk)
    , Show (GenTx blk)
    , Show (GenTxId blk)
    , Show blk
    , Show (Header blk)
    )

{-------------------------------------------------------------------------------
  Untyped/typed protocol boundary
-------------------------------------------------------------------------------}

-- | Perform an action that expects ProtocolInfo for Byron/PBFT,
--   with attendant configuration.
withRealPBFT
  :: CardanoConfiguration
  -> (Demo.RunDemo (ByronBlockOrEBB ByronConfig)
      => Consensus.Protocol (ByronBlockOrEBB ByronConfig)
      -> IO a)
  -> IO a
withRealPBFT cc action = do
  SomeProtocol p <- fromProtocol cc RealPBFT
  case p of
    p'@Consensus.ProtocolRealPBFT{} -> action p'
    _ -> error "Impossible, as long as sanity prevails in these halls."

data SomeProtocol where
  SomeProtocol :: (Demo.RunDemo blk, TraceConstraints blk)
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

fromProtocol cc RealPBFT = do
    let Core{ coGenesisFile
            , coGenesisHash
            , coPBftSigThd
            } = ccCore cc
        genHash = either (throw . ConfigurationError) identity $
                  decodeAbstractHash coGenesisHash
        cvtRNM :: RequireNetworkMagic -> RequiresNetworkMagic
        cvtRNM NoRequireNetworkMagic = RequiresNoMagic
        cvtRNM RequireNetworkMagic   = RequiresMagic

    gcE <- runExceptT (Genesis.mkConfigFromFile
                       (cvtRNM $ coRequiresNetworkMagic $ ccCore cc)
                       coGenesisFile genHash)
    let gc = case gcE of
          Left err -> throw err -- TODO: no no no!
          Right x -> x

    optionalLeaderCredentials <- readLeaderCredentials gc (ccCore cc)

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

readLeaderCredentials :: Genesis.Config
                      -> Core
                      -> IO (Maybe PBftLeaderCredentials)
readLeaderCredentials gc Core {
                           coStaticKeySigningKeyFile = Just signingKeyFile
                         , coStaticKeyDlgCertFile    = Just delegCertFile
                         } = do
    signingKeyFileBytes <- LB.readFile signingKeyFile
    delegCertFileBytes  <- LB.readFile delegCertFile

    --TODO: review the style of reporting for input validation failures
    -- If we use throwIO, we should use a local exception type that
    -- wraps the other structured failures and reports them appropriatly
    signingKey <- either throwIO return $
                    deserialiseSigningKey signingKeyFileBytes

    delegCert  <- either (fail . unpack) return $
                    canonicalDecodePretty delegCertFileBytes

    either throwIO (return . Just)
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
