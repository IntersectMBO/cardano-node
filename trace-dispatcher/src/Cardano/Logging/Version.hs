{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Logging.Version
  ( ForwardingVersion (..)
  , ForwardingVersionData (..)
  , forwardingVersionCodec
  , forwardingCodecCBORTerm
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import qualified Codec.CBOR.Term as CBOR

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Protocol.Handshake.Version (Accept (..), Acceptable (..))

data ForwardingVersion
  = ForwardingV_1
  | ForwardingV_2
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

forwardingVersionCodec :: CodecCBORTerm (Text, Maybe Int) ForwardingVersion
forwardingVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
 where
  encodeTerm ForwardingV_1 = CBOR.TInt 1
  encodeTerm ForwardingV_2 = CBOR.TInt 2

  decodeTerm (CBOR.TInt 1) = Right ForwardingV_1
  decodeTerm (CBOR.TInt 2) = Right ForwardingV_2
  decodeTerm (CBOR.TInt n) = Left ( T.pack "decode ForwardingVersion: unknown tag: " <> T.pack (show n)
                                  , Just n
                                  )
  decodeTerm _             = Left ( T.pack "decode ForwardingVersion: unexpected term"
                                  , Nothing
                                  )

newtype ForwardingVersionData = ForwardingVersionData
  { networkMagic :: NetworkMagic
  } deriving (Eq, Show, Typeable)

instance Acceptable ForwardingVersionData where
  acceptableVersion local remote
    | local == remote = Accept local
    | otherwise       = Refuse $ T.pack $ "ForwardingVersionData mismatch: "
                                 ++ show local
                                 ++ " /= " ++ show remote

forwardingCodecCBORTerm :: ForwardingVersion -> CodecCBORTerm Text ForwardingVersionData
forwardingCodecCBORTerm _ = CodecCBORTerm { encodeTerm, decodeTerm }
 where
  encodeTerm :: ForwardingVersionData -> CBOR.Term
  encodeTerm ForwardingVersionData { networkMagic } =
    CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

  decodeTerm :: CBOR.Term -> Either Text ForwardingVersionData
  decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (ForwardingVersionData $ NetworkMagic $ fromIntegral x)
                           | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
  decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t
