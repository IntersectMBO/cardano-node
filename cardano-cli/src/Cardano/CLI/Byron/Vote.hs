{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.Vote
  ( ByronVoteError(..)
  , renderByronVoteError
  , runVoteCreation
  , submitByronVote
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text


import qualified Cardano.Binary as Binary
import           Cardano.Config.Types
import           Cardano.Chain.Update
                   (AVote(..), Vote, mkVote, recoverUpId, recoverVoteId)
import           Cardano.CLI.Byron.UpdateProposal
                   (ByronUpdateProposalError, deserialiseByronUpdateProposal, readByronUpdateProposal)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Mempool (GenTx(..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Cardano.Api.Typed (NetworkId, toByronProtocolMagicId)
import           Cardano.CLI.Byron.Genesis (ByronGenesisError)
import           Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import           Cardano.CLI.Byron.Key (CardanoEra(..), ByronKeyFailure, readEraSigningKey)
import           Cardano.CLI.Helpers (HelpersError, ensureNewFileLBS)



data ByronVoteError
  = ByronVoteDecodingError !Binary.DecoderError
  | ByronVoteGenesisReadError !ByronGenesisError
  | ByronVoteKeyReadFailure !ByronKeyFailure
  | ByronVoteReadFileFailure !FilePath !Text
  | ByronVoteTxSubmissionError !ByronTxError
  | ByronVoteUpdateProposalFailure !ByronUpdateProposalError
  | ByronVoteUpdateHelperError !HelpersError
  deriving Show

renderByronVoteError :: ByronVoteError -> Text
renderByronVoteError bVerr =
  case bVerr of
    ByronVoteDecodingError decoderErr -> "Error decoding Byron vote: " <> (Text.pack $ show decoderErr)
    ByronVoteGenesisReadError genErr -> "Error reading the genesis file:" <> (Text.pack $ show genErr)
    ByronVoteReadFileFailure fp err -> "Error reading Byron vote at " <> Text.pack fp <> " Error: " <> err
    ByronVoteTxSubmissionError txErr -> "Error submitting the transaction: " <> (Text.pack $ show txErr)
    ByronVoteUpdateProposalFailure err -> "Error reading the update proposal: " <> (Text.pack $ show err)
    ByronVoteUpdateHelperError err ->"Error creating the vote: " <> (Text.pack $ show err)
    ByronVoteKeyReadFailure err -> "Error reading the signing key: " <> (Text.pack $ show err)


runVoteCreation
  :: NetworkId
  -> SigningKeyFile
  -> FilePath
  -> Bool
  -> FilePath
  -> ExceptT ByronVoteError IO ()
runVoteCreation nw sKey upPropFp voteBool outputFp = do
  sK <- firstExceptT ByronVoteKeyReadFailure $ readEraSigningKey ByronEra sKey
  -- TODO: readByronUpdateProposal & deserialiseByronUpdateProposal should be one function
  upProp <- firstExceptT ByronVoteUpdateProposalFailure $ readByronUpdateProposal upPropFp
  proposal <- hoistEither . first ByronVoteUpdateProposalFailure $ deserialiseByronUpdateProposal upProp
  let updatePropId = recoverUpId proposal
      vote = mkVote (toByronProtocolMagicId nw) sK updatePropId voteBool
  firstExceptT ByronVoteUpdateHelperError $ ensureNewFileLBS outputFp (serialiseByronVote vote)

convertVoteToGenTx :: AVote ByteString -> GenTx ByronBlock
convertVoteToGenTx vote = ByronUpdateVote (recoverVoteId vote) vote

deserialiseByronVote :: LByteString -> Either ByronVoteError (AVote ByteString)
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
  :: NetworkId
  -> FilePath
  -> ExceptT ByronVoteError IO ()
submitByronVote network voteFp = do
    voteBs <- liftIO $ LB.readFile voteFp
    vote <- hoistEither $ deserialiseByronVote voteBs
    let genTx = convertVoteToGenTx vote
    traceWith stdoutTracer ("Vote TxId: " ++ condense (txId genTx))
    firstExceptT ByronVoteTxSubmissionError $ nodeSubmitTx network genTx
