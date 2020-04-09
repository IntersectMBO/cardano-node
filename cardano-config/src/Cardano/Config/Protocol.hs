{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Config.Protocol
  ( Protocol(..)
  , ProtocolInstantiationError(..)
  , SomeProtocol(..)
  , TraceConstraints
  , fromProtocol
  , renderProtocolInstantiationError
  ) where

import           Cardano.Prelude

import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (bimapExceptT, firstExceptT,
                                                   hoistEither, left)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import           Data.Aeson (ToJSON)

import           Cardano.BM.Tracing (ToObject)
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import           Cardano.Crypto (RequiresNetworkMagic)
import qualified Cardano.Crypto.Signing as Signing
import           Cardano.Tracing.ToObjectOrphans ()

import           Ouroboros.Consensus.Block (Header, BlockProtocol)
import           Ouroboros.Consensus.BlockchainTime (SlotLength, slotLengthFromSec)
import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId,
                                                  HasTxId, HasTxs, TxId)
import           Ouroboros.Consensus.Mock.Ledger.Block (defaultSimpleBlockConfig)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..), ValidationErr(..))
import           Ouroboros.Consensus.Util (Dict (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block (HeaderHash)

import           Cardano.Config.Types (DelegationCertFile (..),
                                       GenesisFile (..),
                                       LastKnownBlockVersion (..),
                                       Protocol (..), SigningKeyFile (..),
                                       Update (..))

-- TODO: consider not throwing this, or wrap it in a local error type here
-- that has proper error messages.
instance Exception Genesis.ConfigurationError

-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing.
type TraceConstraints blk =
    ( Condense blk
    , Condense [blk]
    , Condense (Header blk)
    , Condense (HeaderHash blk)
    , Condense (GenTx blk)
    , Condense (TxId (GenTx blk))
    , HasTxs blk
    , HasTxId (GenTx blk)
    , Show (ApplyTxErr blk)
    , Show (GenTx blk)
    , Show (GenTxId blk)
    , Show blk
    , Show (Header blk)
    , Show (TxId (GenTx blk))
    , ToJSON   (TxId (GenTx blk))
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    )

{-------------------------------------------------------------------------------
  Untyped/typed protocol boundary
-------------------------------------------------------------------------------}


mockSecurityParam :: SecurityParam
mockSecurityParam = SecurityParam 5

mockSlotLength :: SlotLength
mockSlotLength = slotLengthFromSec 20

-- | Helper for creating a 'SomeProtocol' for a mock protocol that needs the
-- 'CoreNodeId' and NumCoreNodes'. If one of them is missing from the
-- 'CardanoConfiguration', a 'MissingNodeInfo' exception is thrown.
mockSomeProtocol
  :: (RunNode blk, TraceConstraints blk)
  => Maybe NodeId
  -> Maybe Word64
  -- ^ Number of core nodes
  -> (CoreNodeId -> NumCoreNodes -> Consensus.Protocol blk (BlockProtocol blk))
  -> Either ProtocolInstantiationError SomeProtocol
mockSomeProtocol nId mNumCoreNodes mkConsensusProtocol =  do
    (cid, numCoreNodes) <- extractNodeInfo nId mNumCoreNodes
    let p = mkConsensusProtocol cid numCoreNodes
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p

data SomeProtocol where
  SomeProtocol :: (RunNode blk, TraceConstraints blk)
               => Consensus.Protocol blk (BlockProtocol blk) -> SomeProtocol

data ProtocolInstantiationError =
    ByronLegacyProtocolNotImplemented
  | CanonicalDecodeFailure !FilePath !Text
  | DelegationCertificateFilepathNotSpecified
  | GenesisConfigurationError !FilePath !Genesis.ConfigurationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | MissingCoreNodeId
  | MissingNumCoreNodes
  | PbftError !PBftLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath !DeserialiseFailure
  | SigningKeyFilepathNotSpecified
  deriving Show

fromProtocol
  :: Maybe NodeId
  -> Maybe Word64
  -- ^ Number of core nodes
  -> Maybe GenesisFile
  -> RequiresNetworkMagic
  -> Maybe Double
  -> Maybe DelegationCertFile
  -> Maybe SigningKeyFile
  -> Update
  -> Protocol
  -> ExceptT ProtocolInstantiationError IO SomeProtocol
fromProtocol _ _ _ _ _ _ _ _ ByronLegacy =
  left ByronLegacyProtocolNotImplemented
fromProtocol nId mNumCoreNodes _ _ _ _ _ _ BFT =
  hoistEither $ mockSomeProtocol nId mNumCoreNodes $ \cid numCoreNodes ->
    Consensus.ProtocolMockBFT numCoreNodes cid mockSecurityParam
      (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)
fromProtocol nId mNumCoreNodes _ _ _ _ _ _ Praos =
  hoistEither $ mockSomeProtocol nId mNumCoreNodes $ \cid numCoreNodes ->
    Consensus.ProtocolMockPraos
      numCoreNodes
      cid
      PraosParams {
          praosSecurityParam = mockSecurityParam
        , praosSlotsPerEpoch = 3
        , praosLeaderF       = 0.5
        , praosLifetimeKES   = 1000000
        }
      (defaultSimpleBlockConfig mockSecurityParam (slotLengthFromSec 2))
fromProtocol nId mNumCoreNodes _ _ _ _ _ _ MockPBFT =
  hoistEither $ mockSomeProtocol nId mNumCoreNodes $ \cid numCoreNodes@(NumCoreNodes numNodes) ->
    Consensus.ProtocolMockPBFT
      PBftParams { pbftSecurityParam      = mockSecurityParam
                 , pbftNumNodes           = numCoreNodes
                 , pbftSignatureThreshold = (1.0 / fromIntegral numNodes) + 0.1

                 }
      (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)
      cid
fromProtocol _ _ mGenFile nMagic sigThresh delCertFp sKeyFp update RealPBFT = do
    let genFile@(GenesisFile gFp) = fromMaybe ( panic $ "Cardano.Config.Protocol.fromProtocol: "
                                                      <> "Genesis file not specified"
                                              ) mGenFile

    (_, genHash) <- readGenesis genFile

    gc <- firstExceptT (GenesisConfigurationError gFp) $ Genesis.mkConfigFromFile
             nMagic
             gFp
             (Genesis.unGenesisHash genHash)

    optionalLeaderCredentials <- readLeaderCredentials
                                   gc
                                   delCertFp
                                   sKeyFp

    let p = protocolConfigRealPbft update sigThresh gc optionalLeaderCredentials

    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p


-- | The plumbing to select and convert the appropriate configuration subset
-- for the 'RealPBFT' protocol.
--
protocolConfigRealPbft :: Update
                       -> Maybe Double
                       -> Genesis.Config
                       -> Maybe PBftLeaderCredentials
                       -> Consensus.Protocol ByronBlock ProtocolRealPBFT
protocolConfigRealPbft (Update appName appVer lastKnownBlockVersion)
                       pbftSignatureThresh
                       genesis leaderCredentials =
    Consensus.ProtocolRealPBFT
      genesis
      (PBftSignatureThreshold <$> pbftSignatureThresh)
      (convertProtocolVersion lastKnownBlockVersion)
      (Update.SoftwareVersion appName appVer)
      leaderCredentials
  where
    convertProtocolVersion
      LastKnownBlockVersion {lkbvMajor, lkbvMinor, lkbvAlt} =
      Update.ProtocolVersion lkbvMajor lkbvMinor lkbvAlt

readGenesis
  :: GenesisFile
  -> ExceptT ProtocolInstantiationError IO (Genesis.GenesisData, Genesis.GenesisHash)
readGenesis (GenesisFile fp) = firstExceptT (GenesisReadError fp) $ Genesis.readGenesisData fp

readLeaderCredentials :: Genesis.Config
                      -> Maybe DelegationCertFile
                      -> Maybe SigningKeyFile
                      -> ExceptT ProtocolInstantiationError IO (Maybe PBftLeaderCredentials)
readLeaderCredentials gc mDelCertFp mSKeyFp =
  case (mDelCertFp, mSKeyFp) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> left SigningKeyFilepathNotSpecified
    (Nothing, Just _) -> left DelegationCertificateFilepathNotSpecified
    (Just delegCertFile, Just signingKeyFile) -> do

         let delegCertFp = unDelegationCert delegCertFile
         let signKeyFp = unSigningKey signingKeyFile

         signingKeyFileBytes <- liftIO $ LB.readFile signKeyFp
         delegCertFileBytes <- liftIO $ LB.readFile delegCertFp
         signingKey <- firstExceptT (SigningKeyDeserialiseFailure signKeyFp)
                         . hoistEither
                         $ deserialiseSigningKey signingKeyFileBytes
         delegCert  <- firstExceptT (CanonicalDecodeFailure delegCertFp)
                         . hoistEither
                         $ canonicalDecodePretty delegCertFileBytes

         bimapExceptT PbftError Just
           . hoistEither
           $ mkPBftLeaderCredentials gc signingKey delegCert

  where
    deserialiseSigningKey :: LB.ByteString
                          -> Either DeserialiseFailure Signing.SigningKey
    deserialiseSigningKey =
        fmap (Signing.SigningKey . snd)
      . deserialiseFromBytes Signing.fromCBORXPrv

renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
    ByronLegacyProtocolNotImplemented -> "ByronLegacyProtocolNotImplemented"
    CanonicalDecodeFailure fp failure -> "Canonical decode failure in " <> toS fp
                                         <> " Canonical failure: " <> failure
    DelegationCertificateFilepathNotSpecified -> "Delegation certificate filepath not specified"
    --TODO: Implement configuration error render function in cardano-ledger
    GenesisConfigurationError fp genesisConfigError -> "Genesis configuration error in: " <> toS fp
                                                       <> " Error: " <> (T.pack $ show genesisConfigError)
    GenesisReadError fp err ->  "There was an error parsing the genesis file: " <> toS fp
                                <> " Error: " <> (T.pack $ show err)
    MissingCoreNodeId -> "Missing core node id"
    MissingNumCoreNodes -> "NumCoreNodes: not specified in configuration yaml file."
    -- TODO: Implement PBftLeaderCredentialsError render function in ouroboros-network
    PbftError pbftLeaderCredentialsError -> "PBFT leader credentials error: " <> (T.pack $ show pbftLeaderCredentialsError)
    SigningKeyDeserialiseFailure fp deserialiseFailure -> "Signing key deserialisation error in: " <> toS fp
                                                           <> " Error: " <> (T.pack $ show deserialiseFailure)
    SigningKeyFilepathNotSpecified -> "Signing key filepath not specified"

extractNodeInfo
  :: Maybe NodeId
  -> Maybe Word64
  -> Either ProtocolInstantiationError (CoreNodeId, NumCoreNodes)
extractNodeInfo mNodeId ncNumCoreNodes  = do

    coreNodeId   <- case mNodeId of
                      Just (CoreId coreNodeId) -> pure coreNodeId
                      _                        -> Left MissingCoreNodeId
    numCoreNodes <- maybe (Left MissingNumCoreNodes) Right ncNumCoreNodes
    return (coreNodeId , NumCoreNodes numCoreNodes)
