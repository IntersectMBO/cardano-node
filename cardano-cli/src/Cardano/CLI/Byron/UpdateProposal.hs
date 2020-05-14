{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.UpdateProposal
  ( ByronUpdateProposalError(..)
  , ParametersToUpdate(..)
  , runProposalCreation
  , createUpdateProposal
  , deserialiseByronUpdateProposal
  , readByronUpdateProposal
  , submitByronUpdateProposal
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M

import qualified Cardano.Binary as Binary
import           Cardano.Chain.Common (LovelacePortion, TxFeePolicy(..))
import           Cardano.Chain.Genesis (GenesisData(..))
import           Cardano.Chain.Slotting (EpochNumber(..), SlotNumber(..))
import           Cardano.Chain.Update
                   (AProposal(..), ProtocolParametersUpdate(..),
                    InstallerHash(..), Proposal, ProposalBody(..), ProtocolVersion(..),
                    SoftforkRule(..), SoftwareVersion(..), SystemTag(..), recoverUpId,
                    signProposal)
import           Cardano.Config.Types
import           Cardano.Config.Protocol (CardanoEra(..), RealPBFTError)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Cardano.Crypto.Signing (SigningKey, noPassSafeSigner)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.Api (Network)
import           Cardano.CLI.Byron.Key (ByronKeyFailure, readEraSigningKey)
import           Cardano.CLI.Byron.Genesis (ByronGenesisError, readGenesis)
import           Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import           Cardano.CLI.Helpers (HelpersError, ensureNewFileLBS)

data ByronUpdateProposalError
  = ByronReadUpdateProposalFileFailure !FilePath !Text
  | ByronUpdateProposalHelperError !HelpersError
  | ByronUpdateProposalGenesisError !ByronGenesisError
  | ByronUpdateProposalTxError !ByronTxError
  | KeyFailure !ByronKeyFailure
  | UpdateProposalDecodingError !Binary.DecoderError
  | UpdateProposalSubmissionError !RealPBFTError
  deriving Show

runProposalCreation
  :: ConfigYamlFilePath
  -> SigningKeyFile
  -> ProtocolVersion
  -> SoftwareVersion
  -> SystemTag
  -> InstallerHash
  -> FilePath
  -> [ParametersToUpdate]
  -> ExceptT ByronUpdateProposalError IO ()
runProposalCreation configFp sKey pVer sVer sysTag insHash outputFp params = do
  sK <- firstExceptT KeyFailure $ readEraSigningKey ByronEra sKey
  proposal <- createUpdateProposal configFp sK pVer sVer sysTag insHash params
  firstExceptT ByronUpdateProposalHelperError $ ensureNewFileLBS outputFp (serialiseByronUpdateProposal proposal)


data ParametersToUpdate =
    ScriptVersion Word16
  | SlotDuration Natural
  | MaxBlockSize Natural
  | MaxHeaderSize Natural
  | MaxTxSize Natural
  | MaxProposalSize Natural
  | MpcThd LovelacePortion
  | HeavyDelThd LovelacePortion
  | UpdateVoteThd LovelacePortion
  -- ^ UpdateVoteThd: This represents the minimum percentage of the total number of genesis
  -- keys that have to endorse a protocol version to be able to become adopted.
  | UpdateProposalThd LovelacePortion
  -- ^ UpdateProposalTTL: If after the number of slots specified the proposal
  -- does not reach majority of approvals, the proposal is simply discarded.
  | UpdateProposalTTL SlotNumber
  | SoftforkRuleParam SoftforkRule
  | TxFeePolicy TxFeePolicy
  | UnlockStakeEpoch EpochNumber
  deriving Show

createProtocolParametersUpdate
  :: ProtocolParametersUpdate
  -> [ParametersToUpdate]
  -> ProtocolParametersUpdate
createProtocolParametersUpdate init paramsToUpdate = go init paramsToUpdate
 where go i [] = i
       go i (paramToUpdate : rest) =
         case paramToUpdate of
           ScriptVersion val -> go i{ppuScriptVersion = Just val} rest
           SlotDuration val -> go i{ppuSlotDuration = Just val} rest
           MaxBlockSize val -> go i{ppuMaxBlockSize = Just val} rest
           MaxHeaderSize val -> go i{ppuMaxHeaderSize = Just val} rest
           MaxTxSize val -> go i{ppuMaxTxSize = Just val} rest
           MaxProposalSize val -> go i{ppuMaxProposalSize = Just val} rest
           MpcThd val -> go i{ppuMpcThd = Just val} rest
           HeavyDelThd val -> go i{ppuHeavyDelThd = Just val} rest
           UpdateVoteThd val -> go i{ppuUpdateVoteThd = Just val} rest
           UpdateProposalThd val -> go i{ppuUpdateProposalThd = Just val} rest
           UpdateProposalTTL val -> go i{ppuUpdateProposalTTL = Just val} rest
           SoftforkRuleParam val -> go i{ppuSoftforkRule = Just val} rest
           TxFeePolicy val -> go i{ppuTxFeePolicy = Just val} rest
           UnlockStakeEpoch val -> go i{ppuUnlockStakeEpoch = Just val} rest

convertProposalToGenTx :: AProposal ByteString -> Mempool.GenTx ByronBlock
convertProposalToGenTx prop = Mempool.ByronUpdateProposal (recoverUpId prop) prop

createUpdateProposal
  :: ConfigYamlFilePath
  -> SigningKey
  -> ProtocolVersion
  -> SoftwareVersion
  -> SystemTag
  -> InstallerHash
  -> [ParametersToUpdate]
  -> ExceptT ByronUpdateProposalError IO Proposal
createUpdateProposal yamlConfigFile sKey pVer sVer sysTag inshash paramsToUpdate = do

  nc <- liftIO $ parseNodeConfigurationFP yamlConfigFile
  (genData, _) <- firstExceptT ByronUpdateProposalGenesisError . readGenesis $ ncGenesisFile nc

  let metaData :: M.Map SystemTag InstallerHash
      metaData = M.singleton sysTag inshash
      noPassSigningKey = noPassSafeSigner sKey
      pmId = gdProtocolMagicId genData
      protocolParamsUpdate = createProtocolParametersUpdate
                               emptyProtocolParametersUpdate paramsToUpdate


  let proposalBody = ProposalBody pVer protocolParamsUpdate sVer metaData

  let proposal = signProposal pmId proposalBody noPassSigningKey

  pure proposal

emptyProtocolParametersUpdate :: ProtocolParametersUpdate
emptyProtocolParametersUpdate =
  ProtocolParametersUpdate
    { ppuScriptVersion = Nothing
    , ppuSlotDuration = Nothing
    , ppuMaxBlockSize = Nothing
    , ppuMaxHeaderSize = Nothing
    , ppuMaxTxSize = Nothing
    , ppuMaxProposalSize = Nothing
    , ppuMpcThd = Nothing
    , ppuHeavyDelThd = Nothing
    , ppuUpdateVoteThd = Nothing
    , ppuUpdateProposalThd = Nothing
    , ppuUpdateProposalTTL = Nothing
    , ppuSoftforkRule = Nothing
    , ppuTxFeePolicy = Nothing
    , ppuUnlockStakeEpoch = Nothing
    }

serialiseByronUpdateProposal :: Proposal -> LByteString
serialiseByronUpdateProposal = Binary.serialize

deserialiseByronUpdateProposal :: LByteString
                               -> Either ByronUpdateProposalError (AProposal ByteString)
deserialiseByronUpdateProposal bs =
  case Binary.decodeFull bs of
    Left deserFail -> Left $ UpdateProposalDecodingError deserFail
    Right proposal -> Right $ annotateProposal proposal
 where
  annotateProposal :: AProposal Binary.ByteSpan -> AProposal ByteString
  annotateProposal proposal = Binary.annotationBytes bs proposal

readByronUpdateProposal :: FilePath -> ExceptT ByronUpdateProposalError IO LByteString
readByronUpdateProposal fp =
  handleIOExceptT (ByronReadUpdateProposalFileFailure fp . toS . displayException)
                  (LB.readFile fp)


submitByronUpdateProposal
  :: IOManager
  -> Network
  -> FilePath
  -> ExceptT ByronUpdateProposalError IO ()
submitByronUpdateProposal iomgr network proposalFp = do
    proposalBs <- readByronUpdateProposal proposalFp
    aProposal <- hoistEither $ deserialiseByronUpdateProposal proposalBs
    let genTx = convertProposalToGenTx aProposal
    traceWith stdoutTracer $
      "Update proposal TxId: " ++ condense (Mempool.txId genTx)
    firstExceptT ByronUpdateProposalTxError $ nodeSubmitTx iomgr network genTx
