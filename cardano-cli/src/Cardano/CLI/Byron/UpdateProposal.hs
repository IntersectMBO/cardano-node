{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.UpdateProposal
  ( ByronUpdateProposalError(..)
  , runProposalCreation
  , readByronUpdateProposal
  , renderByronUpdateProposalError
  , submitByronUpdateProposal
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB

import           Cardano.Chain.Update (InstallerHash (..), ProtocolVersion (..),
                     SoftwareVersion (..), SystemTag (..))
import           Cardano.CLI.Helpers (HelpersError, ensureNewFileLBS, renderHelpersError, textShow)

import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Cardano.Api (NetworkId, SerialiseAsRawBytes (..))

import           Cardano.Api.Byron (AsType (AsByronUpdateProposal), ByronProtocolParametersUpdate,
                     ByronUpdateProposal, SigningKey (..), makeByronUpdateProposal,
                     toByronLedgerUpdateProposal)
import           Cardano.CLI.Byron.Genesis (ByronGenesisError)
import           Cardano.CLI.Byron.Key (ByronKeyFailure, readEraSigningKey)
import           Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import           Cardano.CLI.Shelley.Commands (ByronKeyFormat (..))
import           Cardano.CLI.Types

data ByronUpdateProposalError
  = ByronReadUpdateProposalFileFailure !FilePath !Text
  | ByronUpdateProposalWriteError !HelpersError
  | ByronUpdateProposalGenesisReadError !FilePath !ByronGenesisError
  | ByronUpdateProposalTxError !ByronTxError
  | ReadSigningKeyFailure !FilePath !ByronKeyFailure
  | UpdateProposalDecodingError !FilePath
  deriving Show

renderByronUpdateProposalError :: ByronUpdateProposalError -> Text
renderByronUpdateProposalError err =
  case err of
    ByronReadUpdateProposalFileFailure fp rErr ->
      "Error reading update proposal at " <> textShow fp <> " Error: " <> textShow rErr
    ByronUpdateProposalWriteError hErr ->
      "Error writing update proposal: " <> renderHelpersError hErr
    ByronUpdateProposalGenesisReadError fp rErr ->
      "Error reading update proposal at: " <> textShow fp <> " Error: " <> textShow rErr
    ByronUpdateProposalTxError txErr ->
      "Error submitting update proposal: " <> textShow txErr
    ReadSigningKeyFailure fp rErr ->
      "Error reading signing key at: " <> textShow fp <> " Error: " <> textShow rErr
    UpdateProposalDecodingError fp ->
      "Error decoding update proposal at: " <> textShow fp

runProposalCreation
  :: NetworkId
  -> SigningKeyFile
  -> ProtocolVersion
  -> SoftwareVersion
  -> SystemTag
  -> InstallerHash
  -> FilePath
  -> ByronProtocolParametersUpdate
  -> ExceptT ByronUpdateProposalError IO ()
runProposalCreation nw sKey@(SigningKeyFile sKeyfp) pVer sVer
                    sysTag insHash outputFp params = do
  sK <- firstExceptT (ReadSigningKeyFailure sKeyfp) $ readEraSigningKey NonLegacyByronKeyFormat sKey
  let proposal = makeByronUpdateProposal nw pVer sVer sysTag insHash (ByronSigningKey sK) params
  firstExceptT ByronUpdateProposalWriteError $
    ensureNewFileLBS outputFp . LB.fromStrict $ serialiseToRawBytes proposal

readByronUpdateProposal :: FilePath -> ExceptT ByronUpdateProposalError IO ByronUpdateProposal
readByronUpdateProposal fp = do
  proposalBs <- handleIOExceptT (ByronReadUpdateProposalFileFailure fp . toS . displayException)
                  $ BS.readFile fp
  let mProposal = deserialiseFromRawBytes AsByronUpdateProposal proposalBs
  hoistEither $ maybe (Left $ UpdateProposalDecodingError fp) Right mProposal

submitByronUpdateProposal
  :: NetworkId
  -> FilePath
  -> ExceptT ByronUpdateProposalError IO ()
submitByronUpdateProposal network proposalFp = do
    proposal  <- readByronUpdateProposal proposalFp
    let genTx = toByronLedgerUpdateProposal proposal
    traceWith stdoutTracer $
      "Update proposal TxId: " ++ condense (txId genTx)
    firstExceptT ByronUpdateProposalTxError $ nodeSubmitTx network genTx

