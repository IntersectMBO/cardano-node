module Cardano.CLI.Byron.Run
  ( runByronClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (hoistEither)
import qualified Data.ByteString.Lazy as LB

import           Cardano.Chain.Update (recoverUpId)

import           Cardano.CLI.Byron.Parsers
import           Cardano.CLI.Byron.UpdateProposal
import           Cardano.CLI.Byron.Vote
import           Cardano.CLI.Key
import           Cardano.CLI.Ops

runByronClientCommand :: ByronCommand -> ExceptT CliError IO ()
runByronClientCommand bcc =
  case bcc of
    CreateVote configFp sKey upPropFp voteBool outputFp -> do
        sK <- readSigningKey ByronEra sKey
        upProp <- readByronUpdateProposal upPropFp
        proposal <- hoistEither $ deserialiseByronUpdateProposal upProp
        let updatePropId = recoverUpId proposal
        vote <- createByronVote configFp sK updatePropId voteBool
        ensureNewFileLBS outputFp (serialiseByronVote vote)

    SubmitUpdateProposal configFp proposalFp mSocket ->
        withIOManagerE $ \iocp -> submitByronUpdateProposal iocp configFp proposalFp mSocket

    SubmitVote configFp voteFp mSocket ->
        withIOManagerE $ \iocp -> submitByronVote iocp configFp voteFp mSocket

    UpdateProposal configFp sKey pVer sVer sysTag insHash outputFp params -> do
        sK <- readSigningKey ByronEra sKey
        proposal <- createUpdateProposal configFp sK pVer sVer sysTag insHash params
        ensureNewFileLBS outputFp (serialiseByronUpdateProposal proposal)


