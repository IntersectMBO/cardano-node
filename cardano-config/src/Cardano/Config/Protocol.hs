{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Protocol
  ( Protocol(..)
  , SomeProtocol(..)
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
import           Cardano.Crypto (decodeAbstractHash)
import qualified Cardano.Crypto.Signing as Signing
import           Cardano.Shell.Lib (GeneralException (..))

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..),
                                                        PBftLeaderCredentials,
                                                        PBftSignatureThreshold(..),
                                                         mkPBftLeaderCredentials)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Protocol (SecurityParam (..),
                                               PraosParams (..),
                                               PBftParams (..))
import qualified Ouroboros.Consensus.Protocol as Consensus
import qualified Ouroboros.Consensus.Ledger.Byron        as Consensus
import           Ouroboros.Consensus.Util (Dict(..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block

import           Cardano.Config.Types
                   (DelegationCertFile (..), GenesisFile (..),
                    MiscellaneousFilepaths (..), NodeCLI (..),
                    NodeConfiguration (..), LastKnownBlockVersion (..),
                    Update (..), Protocol (..), SigningKeyFile (..))

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


mockSecurityParam :: SecurityParam
mockSecurityParam = SecurityParam 5

-- | Helper for creating a 'SomeProtocol' for a mock protocol that needs the
-- 'CoreNodeId' and NumCoreNodes'. If one of them is missing from the
-- 'CardanoConfiguration', a 'MissingNodeInfo' exception is thrown.
mockSomeProtocol
  :: (RunNode blk, TraceConstraints blk)
  => NodeConfiguration
  -> (CoreNodeId -> NumCoreNodes -> Consensus.Protocol blk)
  -> IO SomeProtocol
mockSomeProtocol nc mkConsensusProtocol =  do
    (cid, numCoreNodes) <- either throwIO return $ extractNodeInfo nc
    let p = mkConsensusProtocol cid numCoreNodes
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p



data SomeProtocol where
  SomeProtocol :: (RunNode blk, TraceConstraints blk)
               => Consensus.Protocol blk -> SomeProtocol

fromProtocol :: NodeConfiguration
             -> NodeCLI
             -> IO SomeProtocol
fromProtocol nc nCli = case ncProtocol nc of
  ByronLegacy ->
    error "Byron Legacy protocol is not implemented."

  BFT         -> mockSomeProtocol nc $ \cid numCoreNodes ->
    Consensus.ProtocolMockBFT numCoreNodes cid mockSecurityParam

  Praos       -> mockSomeProtocol nc $ \cid numCoreNodes ->
    Consensus.ProtocolMockPraos numCoreNodes cid PraosParams {
        praosSecurityParam = mockSecurityParam
      , praosSlotsPerEpoch = 3
      , praosLeaderF       = 0.5
      , praosLifetimeKES   = 1000000
      }

  MockPBFT    -> mockSomeProtocol nc $ \cid numCoreNodes@(NumCoreNodes numNodes) ->
    Consensus.ProtocolMockPBFT numCoreNodes cid PBftParams {
        pbftSecurityParam      = mockSecurityParam
      , pbftNumNodes           = fromIntegral numNodes
      , pbftSignatureThreshold = (1.0 / fromIntegral numNodes) + 0.1
      }

  RealPBFT    -> do
    let NodeConfiguration { ncGenesisHash } = nc
        genHash = either (throw . ConfigurationError) identity $
                  decodeAbstractHash ncGenesisHash

    gcE <- runExceptT (Genesis.mkConfigFromFile
                       (ncReqNetworkMagic nc)
                       (unGenesisFile . genesisFile $ mscFp nCli)
                       genHash
                      )
    let gc = case gcE of
          Left err -> throw err -- TODO: no no no!
          Right x -> x

    optionalLeaderCredentials <- readLeaderCredentials gc nCli

    let p = protocolConfigRealPbft nc gc optionalLeaderCredentials

    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p


-- | The plumbing to select and convert the appropriate configuration subset
-- for the 'RealPBFT' protocol.
--
protocolConfigRealPbft :: NodeConfiguration
                       -> Genesis.Config
                       -> Maybe PBftLeaderCredentials
                       -> Consensus.Protocol Consensus.ByronBlock
protocolConfigRealPbft NodeConfiguration {
                         ncPbftSignatureThresh,
                         ncUpdate = Update {
                           upApplicationName,
                           upApplicationVersion,
                           upLastKnownBlockVersion
                         }
                       }
                       genesis leaderCredentials =
    Consensus.ProtocolRealPBFT
      genesis
      (PBftSignatureThreshold <$> ncPbftSignatureThresh)
      (convertProtocolVersion upLastKnownBlockVersion)
      (Update.SoftwareVersion (Update.ApplicationName upApplicationName)
                              (toEnum upApplicationVersion))
      leaderCredentials
  where
    convertProtocolVersion
      LastKnownBlockVersion {lkbvMajor, lkbvMinor, lkbvAlt} =
      Update.ProtocolVersion (toEnum lkbvMajor)
                             (toEnum lkbvMinor)
                             (toEnum lkbvAlt)


readLeaderCredentials :: Genesis.Config
                      -> NodeCLI
                      -> IO (Maybe PBftLeaderCredentials)
readLeaderCredentials gc nCli = do
  let mDelCertFp = delegCertFile $ mscFp nCli
  let mSKeyFp = signKeyFile $ mscFp nCli
  case (mDelCertFp, mSKeyFp) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> panic "Signing key filepath not specified"
    (Nothing, Just _) -> panic "Delegation certificate filepath not specified"
    (Just delegCertFile, Just signingKeyFile) -> do
         signingKeyFileBytes <- LB.readFile $ unSigningKey signingKeyFile
         delegCertFileBytes <- LB.readFile $ unDelegationCert delegCertFile

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

-- | Info missing from the config needed to run a protocol
data MissingNodeInfo
  = MissingCoreNodeId
  | MissingNumCoreNodes
  deriving (Show, Exception)

extractNodeInfo
  :: NodeConfiguration
  -> Either MissingNodeInfo (CoreNodeId, NumCoreNodes)
extractNodeInfo NodeConfiguration { ncNodeId, ncNumCoreNodes } = do

    coreNodeId   <- case ncNodeId of
      CoreId coreNodeId -> pure coreNodeId
      _                 -> Left MissingCoreNodeId
    numCoreNodes <- maybe (Left MissingNumCoreNodes) Right ncNumCoreNodes
    return (CoreNodeId coreNodeId , NumCoreNodes numCoreNodes)
