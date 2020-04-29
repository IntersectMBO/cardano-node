
import           Cardano.Api

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  keyPair <- byronGenKeyPair
  BS.putStrLn $ renderByronKeyPairView keyPair

  keyPair' <- byronGenKeyPair
  -- Could also be 'Testnet x'.
  let pubKey = mkByronPublicKey keyPair' Mainnet
  BS.putStrLn $ renderByronPublicKeyView pubKey

  let addr = byronPubKeyAddress pubKey
  BS.putStrLn $ renderByronAddressView addr
