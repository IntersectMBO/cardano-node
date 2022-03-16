module Properties.Cli.KesPeriodInfo where

import           Prelude

import           Control.Monad.IO.Class
import           Data.List (isInfixOf)
import qualified GHC.Stack as GHC

import           Cardano.CLI.Shelley.Output
import           Cardano.CLI.Shelley.Run.Query
import           Cardano.CLI.Types

import           Hedgehog (success)
import           Hedgehog.Extras.Test.Base (Integration, failMessage)

-- | This property checks that a given operational certificate has a valid specified KES starting period.
prop_op_cert_valid_kes_period :: GHC.HasCallStack => FilePath -> QueryKesPeriodInfoOutput -> Integration ()
prop_op_cert_valid_kes_period opCertFp output =
  GHC.withFrozenCallStack $
    case qKesOpCertIntervalInformation output of
      OpCertWithinInterval{} -> success
      info@OpCertStartingKesPeriodIsInTheFuture{} ->
        failMessage GHC.callStack
          $ "Expected OpCertWithinInterval but got: OpCertStartingKesPeriodIsInTheFuture " <> "\n"
          <> renderOpCertIntervalInformation opCertFp info
      info@OpCertExpired{} ->
        failMessage GHC.callStack
          $ "Expected OpCertWithinInterval but got: OpCertExpired " <> "\n"
          <> renderOpCertIntervalInformation opCertFp info
      info@OpCertSomeOtherError{} ->
        failMessage GHC.callStack
          $ "Expected OpCertWithinInterval but got: OpCertSomeOtherError " <> "\n"
          <> renderOpCertIntervalInformation opCertFp info


-- | This property parses the node's logs for the output "TraceForgedBlock" to confirm that
-- the node has minted blocks at any time in the past.
prop_node_minted_block :: GHC.HasCallStack => FilePath -> Integration ()
prop_node_minted_block nodeLogFp  = do
  logs <- liftIO $ readFile nodeLogFp
  -- TODO: Ideally we would parse the node's json logging file via the FromJSON LogObject
  -- instance. This will require all properties that depend on parsing the node's logs to
  -- only parse the JSON logs and not the plain text logs.
  let allMintingLogs = filter (isInfixOf "TraceForgedBlock") $ lines logs
  if null allMintingLogs
  then failMessage GHC.callStack $ "Log file: " <> nodeLogFp <> " had no logs indicating the relevant node has minted blocks."
  else success



