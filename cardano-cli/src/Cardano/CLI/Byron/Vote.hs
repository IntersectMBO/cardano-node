{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.Vote
  ( runVoteCreation
  , submitByronVote
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import qualified Data.ByteString.Lazy as LB


import qualified Cardano.Binary as Binary
import           Cardano.Config.Types
import           Cardano.Chain.Genesis (GenesisData(..))
import           Cardano.Chain.Update
                   (AVote(..), UpId, Vote, mkVote, recoverUpId, recoverVoteId)
import           Cardano.CLI.Byron.UpdateProposal
                   (deserialiseByronUpdateProposal, readByronUpdateProposal)
import           Cardano.CLI.Byron.Key (readEraSigningKey)
import           Cardano.CLI.Ops (CardanoEra(..), ensureNewFileLBS)
import           Cardano.Crypto.Signing (SigningKey)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.IOManager (IOManager)

import           Cardano.Api (Network)
import           Cardano.CLI.Errors (CliError(..))
import           Cardano.CLI.Ops (readGenesis)
import           Cardano.CLI.Byron.Tx (nodeSubmitTx)




runVoteCreation
  :: ConfigYamlFilePath
  -> SigningKeyFile
  -> FilePath
  -> Bool
  -> FilePath
  -> ExceptT CliError IO ()
runVoteCreation configFp sKey upPropFp voteBool outputFp = do
  sK <- readEraSigningKey ByronEra sKey
  upProp <- readByronUpdateProposal upPropFp
  proposal <- hoistEither $ deserialiseByronUpdateProposal upProp
  let updatePropId = recoverUpId proposal
  vote <- createByronVote configFp sK updatePropId voteBool
  ensureNewFileLBS outputFp (serialiseByronVote vote)

convertVoteToGenTx :: AVote ByteString -> Mempool.GenTx ByronBlock
convertVoteToGenTx vote = Mempool.ByronUpdateVote (recoverVoteId vote) vote

createByronVote
  :: ConfigYamlFilePath
  -> SigningKey
  -> UpId
  -> Bool
  -> ExceptT CliError IO Vote
createByronVote config sKey upId voteChoice = do
  nc <- liftIO $ parseNodeConfigurationFP config
  (genData, _) <- readGenesis $ ncGenesisFile nc
  let pmId = gdProtocolMagicId genData
  --TODO: this reads the config file just to get the networkMagic
  pure $ mkVote pmId sKey upId voteChoice

deserialiseByronVote :: LByteString -> Either CliError (AVote ByteString)
deserialiseByronVote bs =
  case Binary.decodeFull bs of
    Left deserFail -> Left $ ByronVoteDecodingError deserFail
    Right vote -> Right $ annotateVote vote
 where
  annotateVote :: AVote Binary.ByteSpan -> AVote ByteString
  annotateVote vote = Binary.annotationBytes bs vote


serialiseByronVote :: Vote -> LByteString
serialiseByronVote = Binary.serialize

submitByronVote
  :: IOManager
  -> Network
  -> FilePath
  -> ExceptT CliError IO ()
submitByronVote iomgr network voteFp = do
    voteBs <- liftIO $ LB.readFile voteFp
    vote <- hoistEither $ deserialiseByronVote voteBs
    let genTx = convertVoteToGenTx vote
    traceWith stdoutTracer ("Vote TxId: " ++ condense (Mempool.txId genTx))
    nodeSubmitTx iomgr network genTx

