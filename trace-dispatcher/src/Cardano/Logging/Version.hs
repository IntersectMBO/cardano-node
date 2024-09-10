{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Logging.Version
  ( ForwardingVersion (..)
  , ForwardingVersionData (..)
  , ForwardingTraceSelector (..)
  , forwardingVersionCodec
  , forwardingCodecCBORTerm
  ) where

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Protocol.Handshake.Version (Accept (..), Acceptable (..),
                   Queryable (..))

import qualified Codec.CBOR.Term as CBOR
import           Data.Bits
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

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

data ForwardingVersionData = ForwardingVersionData
  { networkMagic      :: NetworkMagic
  , selectForward     :: ForwardingTraceSelector
  } deriving (Eq, Show, Typeable)

-- | This value is part of forwarding protocol negotiation, as the forwarder ("node") is
--   unaware of the subscriber's ("cardano-tracer") config - and what it will eventually need
data ForwardingTraceSelector
  = TraceSelectAll            -- ^ forward all trace message renderings
  | TraceSelectMachine        -- ^ forward machine-readable only: LogFormatting(forMachine)
  | TraceSelectHuman          -- ^ forward human-readable only: LogFormatting(forHuman)
  | TraceSelectNone           -- ^ do not forward trace messages; the forwarding protocol can still transport metrics and datapoints
  deriving (Eq, Show, Typeable)

-- To avoid bumping the protocol version and still be downwards compatible,
-- negotiating required trace message renderings for forwarding works such that:
-- * above the Word32 networkMagic, bit 32 is set to block forMachine forwarding, bit 33 is set to block forHuman forwarding
-- * the previous ForwardingVersionData CBOR term keeps its semantics of "forward everything" (no high bits set)
-- * the node should always start negotiation with "TraceSelectNone", and let one or both renderings be unblocked during handshake
-- * the tracer will negotiate the required renderings based on its config

encodeAsInt :: ForwardingVersionData -> Int
encodeAsInt ForwardingVersionData{..} =
  shiftL 32 bitmask .|. fromIntegral (unNetworkMagic networkMagic)
  where
    -- this could be done with an Enum instance - but I want the code to be _very_ explicit
    bitmask = case selectForward of
      TraceSelectAll      -> 0b00
      TraceSelectMachine  -> 0b10
      TraceSelectHuman    -> 0b01
      TraceSelectNone     -> 0b11

decodeFromInt :: Int -> ForwardingVersionData
decodeFromInt i = ForwardingVersionData
  { networkMagic  = NetworkMagic $ fromIntegral $ i .&. 0xffffffff
  , selectForward = selector $ 0b11 .&. shiftR 32 i
  }
  where
    -- this could be done with an Enum instance - but I want the code to be _very_ explicit
    selector :: Int -> ForwardingTraceSelector
    selector 0b00 = TraceSelectAll
    selector 0b10 = TraceSelectMachine
    selector 0b01 = TraceSelectHuman
    selector 0b11 = TraceSelectNone
    selector _    = error "decodeFromInt: impossible"

instance Acceptable ForwardingVersionData where
  acceptableVersion local remote
    | notSameNetwork  = Refuse "ForwardingVersionData: networkMagic mismatch"
    | otherwise       = Accept negotiatedSelection
    where
      notSameNetwork      = networkMagic local /= networkMagic remote
      negotiatedSelection = decodeFromInt $ encodeAsInt local .&. encodeAsInt remote

instance Queryable ForwardingVersionData where
    queryVersion _ = False

forwardingCodecCBORTerm :: ForwardingVersion -> CodecCBORTerm Text ForwardingVersionData
forwardingCodecCBORTerm _ = CodecCBORTerm { encodeTerm, decodeTerm }
 where
  encodeTerm :: ForwardingVersionData -> CBOR.Term
  encodeTerm = CBOR.TInt . encodeAsInt

  decodeTerm :: CBOR.Term -> Either Text ForwardingVersionData
  decodeTerm (CBOR.TInt x) = Right $ decodeFromInt x
  decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t
