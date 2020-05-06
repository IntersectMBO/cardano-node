
import           Cardano.Api

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  sk <- byronGenSigningKey
  BS.putStrLn $ renderSigningKeyView sk

  -- Could also be 'Testnet x'.
  let vk = getVerificationKey sk
  BS.putStrLn $ renderVerificationKeyView vk

  let addr = byronVerificationKeyAddress vk Mainnet
  BS.putStrLn $ renderAddressView addr
