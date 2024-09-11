import GHC.Debug.Client
import GHC.Debug.Snapshot

main :: IO ()
main = withDebuggeeConnect "/tmp/ghc-debug" $ \d ->
    makeSnapshot d "/tmp/cardano-node-snapshot.dat"

