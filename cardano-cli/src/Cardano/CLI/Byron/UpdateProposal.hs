{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.UpdateProposal
  ( ByronUpdateProposalError(..)
  , runProposalCreation
  , readByronUpdateProposal
  , renderByronUpdateProposalError
  , submitByronUpdateProposal
  ) where

import           Cardano.Prelude (ConvertText (..))

import           Control.Exception (Exception (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Prettyprinter as PP

import           Cardano.Chain.Update (InstallerHash (..), ProtocolVersion (..),
                   SoftwareVersion (..), SystemTag (..))

import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Cardano.Api (NetworkId, SerialiseAsRawBytes (..), SocketPath)
import           Cardano.Api.Byron (AsType (AsByronUpdateProposal), ByronProtocolParametersUpdate,
                   ByronUpdateProposal, makeByronUpdateProposal, toByronLedgerUpdateProposal)
import           Cardano.Api.Pretty

import           Cardano.CLI.Byron.Genesis (ByronGenesisError)
import           Cardano.CLI.Byron.Key (ByronKeyFailure, readByronSigningKey)
import           Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import           Cardano.CLI.Helpers (HelpersError, ensureNewFileLBS, renderHelpersError)
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

renderByronUpdateProposalError :: ByronUpdateProposalError -> Doc Ann
renderByronUpdateProposalError err =
  case err of
    ByronReadUpdateProposalFileFailure fp rErr ->
      PP.vsep
      [ reflow $ "Error reading update proposal at" <+> pretty fp <> ", error:"
      , PP.indent 2 $ pretty rErr
      ]
    ByronUpdateProposalWriteError hErr ->
      PP.vsep
      [ reflow "Error writing update proposal:"
      , PP.indent 2 $ renderHelpersError hErr
      ]
    ByronUpdateProposalGenesisReadError fp rErr ->
      PP.vsep
      [ reflow $ "Error reading update proposal at:" <+> pretty fp <> ", with error:"
      , PP.indent 2 $ pretty (show rErr)
      ]
    ByronUpdateProposalTxError txErr ->
      PP.vsep
      [ reflow "Error submitting update proposal:"
      , PP.indent 2 $ pretty (show txErr)
      ]
    ReadSigningKeyFailure fp rErr ->
      PP.vsep
      [ reflow $ "Error reading signing key at:" <+> pretty fp <> ", with error: "
      , PP.indent 2 $ pretty (show rErr)
      ]
    UpdateProposalDecodingError fp ->
      reflow $ "Error decoding update proposal at:" <+> pretty fp

runProposalCreation
  :: NetworkId
  -> SigningKeyFile In
  -> ProtocolVersion
  -> SoftwareVersion
  -> SystemTag
  -> InstallerHash
  -> FilePath
  -> ByronProtocolParametersUpdate
  -> ExceptT ByronUpdateProposalError IO ()
runProposalCreation nw sKey@(File sKeyfp) pVer sVer
                    sysTag insHash outputFp params = do
  sK <- firstExceptT (ReadSigningKeyFailure sKeyfp) $ readByronSigningKey NonLegacyByronKeyFormat sKey
  let proposal = makeByronUpdateProposal nw pVer sVer sysTag insHash sK params
  firstExceptT ByronUpdateProposalWriteError $
    ensureNewFileLBS outputFp $ serialiseToRawBytes proposal

readByronUpdateProposal :: FilePath -> ExceptT ByronUpdateProposalError IO ByronUpdateProposal
readByronUpdateProposal fp = do
  proposalBs <- handleIOExceptT (ByronReadUpdateProposalFileFailure fp . toS . displayException)
                  $ BS.readFile fp
  let proposalResult = deserialiseFromRawBytes AsByronUpdateProposal proposalBs
  hoistEither $ first (const (UpdateProposalDecodingError fp)) proposalResult

submitByronUpdateProposal
  :: SocketPath
  -> NetworkId
  -> FilePath
  -> ExceptT ByronUpdateProposalError IO ()
submitByronUpdateProposal nodeSocketPath network proposalFp = do
  proposal <- readByronUpdateProposal proposalFp
  let genTx = toByronLedgerUpdateProposal proposal
  traceWith stdoutTracer $ "Update proposal TxId: " ++ condense (txId genTx)
  firstExceptT ByronUpdateProposalTxError $ nodeSubmitTx nodeSocketPath network genTx
