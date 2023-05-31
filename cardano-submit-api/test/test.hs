import qualified System.IO as IO

import qualified Cardano.Crypto.Init as Crypto

main :: IO ()
main = do
  Crypto.cryptoInit

  IO.putStrLn "cardano-tx-submit test"
