{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}

module Codec.CBOR.Schema where

import Prelude

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Foreign.Marshal.Alloc (free)
import Foreign.C.String (CString, peekCString, withCStringLen)
import System.IO.Unsafe (unsafePerformIO)

data ValidationError = ValidationError
  { cbor :: String -- | The erroneous CBOR as a base16-encoded text string
  , hint :: String -- | An (hopefully) helpful error message
  } deriving (Eq, Show)

validate
  :: Text
    -- ^ A CDDL specification
  -> ByteString
    -- ^ Some CBOR value
  -> Either ValidationError ()
validate cddl cbor =
  case result of
    [] ->
      Right ()
    hint ->
      Left $ ValidationError
        { hint
        , cbor = B8.unpack (Base16.encode cbor)
        }
 where
  result = unsafePerformIO $ do
    withCStringLen (Text.unpack cddl) $ \(cddl_ptr, cddl_len) ->
      BS.useAsCStringLen cbor $ \(cbor_ptr, cbor_len) -> do
        cstr <- validate_cbor
          cddl_ptr (fromIntegral cddl_len)
          cbor_ptr (fromIntegral cbor_len)
        peekCString cstr <* free cstr

foreign import ccall "cbits/libcddl.h validate_cbor"
  validate_cbor
    :: CString
    -> Word
    -> CString
    -> Word
    -> IO CString
