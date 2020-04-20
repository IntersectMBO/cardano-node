module Cardano.Config.Shelley.VRF
  ( VRFError(..)
  , genVRFKeyPair
  , readVRFSigningKey
  , readVRFVerKey
  , writeVRFSigningKey
  , writeVRFVerKey
  ) where

import           Cardano.Prelude

import qualified Cardano.Binary as CBOR
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither)
import qualified Data.ByteString.Lazy.Char8 as LB

import           Cardano.Crypto.VRF.Class
                   (SignKeyVRF, VerKeyVRF, deriveVerKeyVRF, genKeyVRF)
import           Cardano.Crypto.VRF.Simple (SimpleVRF)


data VRFError = ReadVRFSigningKeyError !FilePath !IOException
              | ReadVRFVerKeyError !FilePath !IOException
              | DecodeVRFSigningKeyError !FilePath !CBOR.DecoderError
              | DecodeVRFVerKeyError !FilePath !CBOR.DecoderError
              | WriteVRFSigningKeyError !FilePath !IOException
              | WriteVRFVerKeyError !FilePath !IOException

genVRFKeyPair :: IO (SignKeyVRF SimpleVRF, VerKeyVRF SimpleVRF)
genVRFKeyPair = do sKeyVRF <- genKeyVRF
                   pure (sKeyVRF, deriveVerKeyVRF sKeyVRF)

readVRFSigningKey :: FilePath -> ExceptT VRFError IO (SignKeyVRF SimpleVRF)
readVRFSigningKey fp = do
  bs <- handleIOExceptT (ReadVRFSigningKeyError fp) $ LB.readFile fp
  firstExceptT (DecodeVRFSigningKeyError fp) . hoistEither $ CBOR.decodeFull bs

writeVRFSigningKey :: FilePath -> SignKeyVRF SimpleVRF -> ExceptT VRFError IO ()
writeVRFSigningKey fp vKeyVRF =
  handleIOExceptT (WriteVRFSigningKeyError fp) $ LB.writeFile fp (CBOR.serialize vKeyVRF)

readVRFVerKey :: FilePath ->  ExceptT VRFError IO (VerKeyVRF SimpleVRF)
readVRFVerKey fp = do
  bs <- handleIOExceptT (ReadVRFVerKeyError fp) $ LB.readFile fp
  firstExceptT (DecodeVRFVerKeyError fp) . hoistEither $ CBOR.decodeFull bs

writeVRFVerKey :: FilePath -> VerKeyVRF SimpleVRF -> ExceptT VRFError IO ()
writeVRFVerKey fp vKeyVRF =
  handleIOExceptT (WriteVRFVerKeyError fp) $ LB.writeFile fp (CBOR.serialize vKeyVRF)
