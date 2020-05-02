
import           Cardano.Api

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  keyPair <- byronGenKeyPair
  BS.putStrLn $ renderKeyPairView keyPair

  -- Could also be 'Testnet x'.
  let vk = mkVerificationKey keyPair
  BS.putStrLn $ renderVerificationKeyView vk

  let addr = byronVerificationKeyAddress vk Mainnet
  BS.putStrLn $ renderAddressView addr
