import           Ouroboros.Network.NodeToClient (withIOManager)
import           MonadicGen.Cardano.Benchmarking.Script.Selftest (runSelftest)
import           System.Exit (die, exitSuccess)

main :: IO ()
main = withIOManager
     $ \iocp -> runSelftest iocp Nothing
                     >>= either (die . show) (const exitSuccess)
