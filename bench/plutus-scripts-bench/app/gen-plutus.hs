import Cardano.Benchmarking.PlutusScripts (findPlutusScript, encodePlutusScript)
import qualified Data.ByteString.Lazy as LBS (putStr)
import System.Environment (getArgs)

main :: IO ()
main =
  do
    a:_ <- getArgs
    let Just s = findPlutusScript a
    LBS.putStr $ encodePlutusScript s
