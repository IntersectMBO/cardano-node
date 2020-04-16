module Cardano.CLI.Byron.Run
  ( runByronClientCommand
  ) where

import           Cardano.Prelude

import           Cardano.CLI.Byron.Parsers
import           Cardano.CLI.Byron.UpdateProposal
import           Cardano.CLI.Key
import           Cardano.CLI.Ops

runByronClientCommand :: ByronCommand -> ExceptT CliError IO ()
runByronClientCommand bcc =
  case bcc of
    UpdateProposal configFp sKey pVer sVer sysTag insHash outputFp params -> do
        sK <- readSigningKey ByronEra sKey
        proposal <- createUpdateProposal configFp sK pVer sVer sysTag insHash params
        ensureNewFileLBS outputFp (serialiseByronUpdateProposal proposal)

    SubmitUpdateProposal configFp proposalFp mSocket ->
        withIOManagerE $ \iocp -> submitByronUpdateProposal iocp configFp proposalFp mSocket
