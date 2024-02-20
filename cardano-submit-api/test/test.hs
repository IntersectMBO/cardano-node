import qualified Cardano.Crypto.Init as Crypto

import qualified System.IO as IO

main :: IO ()
main = do
  Crypto.cryptoInit

  IO.putStrLn "cardano-tx-submit test"
