import qualified Paths_tx_generator as Paths (getDataFileName)
import qualified Cardano.Benchmarking.Script.Types as C (Action (..), SubmitMode (DiscardTX))
import qualified Cardano.Benchmarking.Script.Selftest as C (runSelftest, testScript)
import qualified MonadicGen.Cardano.Benchmarking.Script.Types as M (Action (..), SubmitMode (DiscardTX))
import qualified MonadicGen.Cardano.Benchmarking.Script.Selftest as M (runSelftest, testScript)
import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)
import           System.Exit (die, exitSuccess)
import qualified System.Posix.Process as Posix (forkProcess, getProcessStatus, ProcessStatus)
import qualified System.Posix.Types as Posix (ProcessID)

main :: IO ()
main = do
  putStrLn "Monadic Cleanup Testcase"
  protocolFile <-  Paths.getDataFileName "data/protocol-parameters.json"
  let currentActions = C.testScript protocolFile C.DiscardTX
      monadicActions = M.testScript protocolFile M.DiscardTX
  putStrLn "About to run current selftest"
  currentID <- Posix.forkProcess $ runSelftest' C.runSelftest
  Just status <- Posix.getProcessStatus True False currentID
  putStrLn $ "current selftest status: " ++ show status
  putStrLn "About to print currentActions"
  print currentActions
  putStrLn $ "currentActions: len " ++ show (length currentActions)
  putStrLn "About to run monadic selftest"
  monadicID <- Posix.forkProcess $ runSelftest' M.runSelftest
  Just status <- Posix.getProcessStatus True False monadicID
  putStrLn $ "monadic selftest status: " ++ show status
  putStrLn "About to print monadicActions"
  print monadicActions
  putStrLn $ "currentActions: len " ++ show (length monadicActions)

runSelftest' :: Show a => (IOManager -> Maybe FilePath -> IO (Either a ())) -> IO ()
runSelftest' rst = do
  withIOManager $ \iocp -> rst iocp Nothing
                               >>= either (die . show) (const exitSuccess)
