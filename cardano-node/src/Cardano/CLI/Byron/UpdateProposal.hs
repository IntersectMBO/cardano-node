{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.UpdateProposal
  ( ParametersToUpdate(..)
  , convertProposalToGenTx
  , createUpdateProposal
  , deserialiseByronUpdateProposal
  , serialiseByronUpdateProposal
  , submitByronUpdateProposal
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import           Control.Tracer (nullTracer, stdoutTracer, traceWith)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M

import qualified Cardano.Binary as Binary
import           Cardano.Chain.Common (LovelacePortion, TxFeePolicy(..))
import           Cardano.Chain.Genesis (GenesisData(..))
import           Cardano.Chain.Slotting (EpochNumber(..), SlotNumber(..))
import           Cardano.Chain.Update
                   (AProposal(..), ProtocolParametersUpdate(..), InstallerHash(..),
                    Proposal, ProposalBody(..), ProtocolVersion(..), SoftforkRule(..),
                    SoftwareVersion(..), SystemTag(..), recoverUpId, signProposal)
import           Cardano.Config.Types
import           Cardano.Crypto.Signing (SigningKey, noPassSafeSigner)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.NodeToClient (IOManager)

import           Cardano.CLI.Ops (CliError(..), readGenesis, withRealPBFT)
import           Cardano.Common.LocalSocket
import           Cardano.Node.Submission (submitGeneralTx)


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
  -> ExceptT CliError IO Proposal
createUpdateProposal configFile sKey pVer sVer sysTag inshash paramsToUpdate = do

  nc <- liftIO $ parseNodeConfigurationFP configFile
  (genData, _) <- readGenesis $ ncGenesisFile nc

  let metaData :: M.Map SystemTag InstallerHash
      metaData = M.singleton sysTag inshash
      noPassSigningKey = noPassSafeSigner sKey
      pmId = gdProtocolMagicId genData
      protocolParamsUpdate = createProtocolParametersUpdate emptyProtocolParametersUpdate paramsToUpdate


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

deserialiseByronUpdateProposal :: LByteString -> Either CliError (AProposal ByteString)
deserialiseByronUpdateProposal bs =
  case Binary.decodeFull bs :: Either Binary.DecoderError (AProposal Binary.ByteSpan) of
    Left deserFail -> Left $ UpdateProposalDecodingError deserFail
    Right proposal -> Right $ annotateProposal proposal
 where
  annotateProposal :: AProposal Binary.ByteSpan -> AProposal ByteString
  annotateProposal proposal = Binary.annotationBytes bs proposal

submitByronUpdateProposal
  :: IOManager
  -> ConfigYamlFilePath
  -> FilePath
  -> Maybe CLISocketPath
  -> ExceptT CliError IO ()
submitByronUpdateProposal iocp config proposalFp mSocket = do
    nc <- liftIO $ parseNodeConfigurationFP config

    let genFile = ncGenesisFile nc
        ptcl = ncProtocol nc
        sigThresh = ncPbftSignatureThresh nc
        nMagic = ncReqNetworkMagic nc

    proposalBs <- liftIO $ LB.readFile proposalFp
    aProposal <- hoistEither $ deserialiseByronUpdateProposal proposalBs
    let genTx = convertProposalToGenTx aProposal

    let proposalBody = Binary.unAnnotated $ aBody aProposal
        (ProtocolVersion major minor alt) = protocolVersion proposalBody
        (SoftwareVersion appName sNumber) = softwareVersion proposalBody


    let lastKnownBlockVersion = LastKnownBlockVersion {lkbvMajor = major, lkbvMinor = minor, lkbvAlt = alt}
        update = Update appName sNumber $ lastKnownBlockVersion
        skt = chooseSocketPath (ncSocketPath nc) mSocket

    firstExceptT UpdateProposalSubmissionError $ withRealPBFT genFile nMagic sigThresh Nothing Nothing update ptcl $
                \p@Consensus.ProtocolRealPBFT{} -> liftIO $ do
                   traceWith stdoutTracer ("Update proposal TxId: " ++ condense (Mempool.txId genTx))
                   submitGeneralTx iocp skt
                                   (pInfoConfig (Consensus.protocolInfo p))
                                   genTx
                                   nullTracer -- stdoutTracer
