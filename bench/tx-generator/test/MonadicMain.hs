import qualified Cardano.Benchmarking.Script.Selftest as C (runSelftest)
import qualified MonadicGen.Cardano.Benchmarking.Script.Selftest as M (runSelftest)
import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)
import           System.Exit (die, exitSuccess)

main :: IO ()
main = runSelftest' C.runSelftest >> runSelftest' M.runSelftest

runSelftest' :: Show a => (IOManager -> Maybe FilePath -> IO (Either a ())) -> IO ()
runSelftest' rst = do
  withIOManager $ \iocp -> rst iocp Nothing
                               >>= either (die . show) (const exitSuccess)
