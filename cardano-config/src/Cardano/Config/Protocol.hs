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

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import           Cardano.Crypto (RequiresNetworkMagic)
import qualified Cardano.Crypto.Signing as Signing
import           Cardano.Tracing.ToObjectOrphans ()

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.BlockchainTime (SlotLength, slotLengthFromSec)
import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Mock.Ledger.Block (defaultSimpleBlockConfig)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Cardano.Config.Types (DelegationCertFile (..),
                                       GenesisFile (..),
                                       LastKnownBlockVersion (..),
                                       Protocol (..), SigningKeyFile (..),
                                       Update (..))
import           Cardano.Tracing.Constraints (TraceConstraints)


{-------------------------------------------------------------------------------
  Untyped/typed protocol boundary
-------------------------------------------------------------------------------}

data SomeProtocol where
  SomeProtocol :: (RunNode blk, TraceConstraints blk)
               => Consensus.Protocol blk (BlockProtocol blk) -> SomeProtocol

fromProtocol
  :: Protocol
  -> Maybe NodeId
  -> Maybe Word64
  -- ^ Number of core nodes
  -> Maybe GenesisFile
  -> RequiresNetworkMagic
  -> Maybe Double
  -> Maybe DelegationCertFile
  -> Maybe SigningKeyFile
  -> Update
  -> ExceptT ProtocolInstantiationError IO SomeProtocol
fromProtocol BFT         = mkConsensusProtocolBFT
fromProtocol Praos       = mkConsensusProtocolPraos
fromProtocol MockPBFT    = mkConsensusProtocolMockPBFT
fromProtocol RealPBFT    = mkConsensusProtocolRealPBFT


mkConsensusProtocolBFT,
 mkConsensusProtocolPraos,
 mkConsensusProtocolMockPBFT,
 mkConsensusProtocolRealPBFT
  :: Maybe NodeId
  -> Maybe Word64
  -- ^ Number of core nodes
  -> Maybe GenesisFile
  -> RequiresNetworkMagic
  -> Maybe Double
  -> Maybe DelegationCertFile
  -> Maybe SigningKeyFile
  -> Update
  -> ExceptT ProtocolInstantiationError IO SomeProtocol


------------------------------------------------------------------------------
-- Mock/testing protocols
--

mkConsensusProtocolBFT nId mNumCoreNodes _ _ _ _ _ _ =
  hoistEither $ mockSomeProtocol nId mNumCoreNodes $ \cid numCoreNodes ->
    Consensus.ProtocolMockBFT numCoreNodes cid mockSecurityParam
      (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)

mkConsensusProtocolPraos nId mNumCoreNodes _ _ _ _ _ _ =
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

mkConsensusProtocolMockPBFT nId mNumCoreNodes _ _ _ _ _ _ =
  hoistEither $ mockSomeProtocol nId mNumCoreNodes $ \cid numCoreNodes@(NumCoreNodes numNodes) ->
    Consensus.ProtocolMockPBFT
      PBftParams { pbftSecurityParam      = mockSecurityParam
                 , pbftNumNodes           = numCoreNodes
                 , pbftSignatureThreshold = (1.0 / fromIntegral numNodes) + 0.1

                 }
      (defaultSimpleBlockConfig mockSecurityParam mockSlotLength)
      cid

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
    return $ SomeProtocol p

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


------------------------------------------------------------------------------
-- Real protocols
--

mkConsensusProtocolRealPBFT _ _ mGenFile nMagic sigThresh delCertFp sKeyFp update = do
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

    return $ SomeProtocol p


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


------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    CanonicalDecodeFailure !FilePath !Text
  | DelegationCertificateFilepathNotSpecified
  | GenesisConfigurationError !FilePath !Genesis.ConfigurationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | MissingCoreNodeId
  | MissingNumCoreNodes
  | PbftError !PBftLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath !DeserialiseFailure
  | SigningKeyFilepathNotSpecified
  deriving Show


renderProtocolInstantiationError :: ProtocolInstantiationError -> Text
renderProtocolInstantiationError pie =
  case pie of
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

