module Cardano.CLI.Byron.Run
  ( runNodeCmd
  ) where

import           Cardano.Prelude

import           Cardano.CLI.Byron.Parsers
import           Cardano.CLI.Byron.UpdateProposal
import           Cardano.CLI.Byron.Vote (runVoteCreation, submitByronVote)
import           Cardano.CLI.Ops

runNodeCmd :: NodeCmd -> ExceptT CliError IO ()
runNodeCmd (CreateVote configFp sKey upPropFp voteBool outputFp) =
  runVoteCreation configFp sKey upPropFp voteBool outputFp

runNodeCmd (SubmitUpdateProposal configFp proposalFp mSocket) =
  withIOManagerE $ \iocp -> submitByronUpdateProposal iocp configFp proposalFp mSocket

runNodeCmd (SubmitVote configFp voteFp mSocket) =
  withIOManagerE $ \iocp -> submitByronVote iocp configFp voteFp mSocket

runNodeCmd (UpdateProposal configFp sKey pVer sVer sysTag insHash outputFp params) =
  runProposalCreation configFp sKey pVer sVer sysTag insHash outputFp params
