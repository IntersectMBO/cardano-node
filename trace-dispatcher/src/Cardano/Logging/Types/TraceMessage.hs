
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Logging.Types.TraceMessage
  ( TraceMessage (..)
  ) where

import           Cardano.Logging.Types

import           Codec.CBOR.JSON
import           Codec.Serialise (Serialise (..))
import           Data.Aeson as AE hiding (decode, encode)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)


-- | base for a machine readable trace message (JSON or CBOR), with metadata, and enclosed payload data from the trace itself.
data TraceMessage = TraceMessage
  { tmsgAt      :: !UTCTime
  , tmsgNS      :: !Text
  , tmsgData    :: !AE.Object
  , tmsgSev     :: !SeverityS
  , tmsgThread  :: !Text
  , tmsgHost    :: !Text
  }
  deriving Show

instance Serialise AE.Object where
  encode = encodeValue . Object
  decode = decodeValue True >>= \case
    Object o -> pure o
    x        -> fail $ "decode(TraceMessage): expected JSON object, got: " ++ show x


-- Serialisations are hand-rolled for higher degree of stability, and making them transparent.
instance Serialise TraceMessage where
  encode TraceMessage{..} =
        encode tmsgAt
    <>  encode tmsgNS
    <>  encode tmsgSev
    <>  encode tmsgData
    <>  encode tmsgThread
    <>  encode tmsgHost

  decode = do
    tmsgAt      <- decode
    tmsgNS      <- decode
    tmsgSev     <- decode
    tmsgData    <- decode
    tmsgThread  <- decode
    tmsgHost    <- decode
    pure TraceMessage{..}


instance ToJSON TraceMessage where
  toJSON TraceMessage{..} = AE.object
    [ "at"      .= tmsgAt
    , "ns"      .= tmsgNS
    , "data"    .= tmsgData
    , "sev"     .= tmsgSev
    , "thread"  .= tmsgThread
    , "host"    .= tmsgHost
    ]
  toEncoding TraceMessage{..} = AE.pairs $
       "at"     .= tmsgAt
    <> "ns"     .= tmsgNS
    <> "data"   .= tmsgData
    <> "sev"    .= tmsgSev
    <> "thread" .= tmsgThread
    <> "host"   .= tmsgHost

instance FromJSON TraceMessage where
  parseJSON = AE.withObject "TraceMessage" $ \v -> do
    tmsgAt      <- v .: "at"
    tmsgNS      <- v .: "ns"
    tmsgData    <- v .: "data"
    tmsgSev     <- v .: "sev"
    tmsgThread  <- v .: "thread"
    tmsgHost    <- v .: "host"
    pure TraceMessage{..}
