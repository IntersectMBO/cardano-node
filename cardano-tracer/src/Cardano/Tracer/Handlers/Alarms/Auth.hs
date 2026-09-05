{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal bearer-token authentication for the alarm HTTP surface. There is
-- no existing auth pattern anywhere in @cardano-tracer@ to follow (none of
-- the existing Prometheus\/EKG\/Timeseries servers check any credential at
-- all), so this is deliberately simple: static token tables, read once from
-- config-supplied files. No hashing, no constant-time comparison, no
-- rotation, no rate limiting.
module Cardano.Tracer.Handlers.Alarms.Auth
  ( ProducerCredential (..)
  , ReaderCredential (..)
  , AuthTables (..)
  , loadCredentials
  , bearerToken
  , lookupProducer
  , lookupReader
  ) where

import           Cardano.Tracer.Configuration (AlarmsAuthConfig (..), ProducerCredentialConfig (..),
                   ReaderCredentialConfig (..))
import           Cardano.Tracer.Handlers.Alarms.Types

import qualified Data.ByteString.Char8 as BSC
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Network.HTTP.Types (hAuthorization)
import           Network.Wai (Request, requestHeaders)

data ProducerCredential = ProducerCredential
  { pcrSource :: !AlarmSource
  }
  deriving stock (Eq, Show)

data ReaderCredential = ReaderCredential
  { rcrName         :: !Text
  , rcrAllowHistory :: !Bool
  , rcrFilter       :: !AlarmFilter
  }
  deriving stock (Eq, Show)

data AuthTables = AuthTables
  { atProducers :: !(Map Text ProducerCredential)
  , atReaders   :: !(Map Text ReaderCredential)
  }

-- | Reads every configured token file once, at registry-construction time
--   (per the design doc's Security section: "Secrets should be read from
--   protected files"). Not part of 'wellFormed', since that's pure and this
--   needs 'IO'.
loadCredentials :: AlarmsAuthConfig -> IO AuthTables
loadCredentials AlarmsAuthConfig{aacProducers, aacReaders} = do
  producers <- traverse loadProducer aacProducers
  readers   <- traverse loadReader aacReaders
  pure AuthTables
    { atProducers = Map.fromList producers
    , atReaders   = Map.fromList readers
    }
 where
  loadProducer ProducerCredentialConfig{pcTokenFile, pcSource} = do
    token <- readTokenFile pcTokenFile
    pure (token, ProducerCredential (AlarmSource pcSource))

  loadReader ReaderCredentialConfig{rcName, rcTokenFile, rcAllowHistory, rcFilter} = do
    token <- readTokenFile rcTokenFile
    pure ( token
         , ReaderCredential
             { rcrName         = rcName
             , rcrAllowHistory = fromMaybe False rcAllowHistory
             , rcrFilter       = maybe emptyAlarmFilter filterFromConfig rcFilter
             }
         )

readTokenFile :: FilePath -> IO Text
readTokenFile path = Text.strip . Text.pack <$> readFile path

-- | Extract the bearer token from a request's @Authorization@ header, if
--   present and well-formed.
bearerToken :: Request -> Maybe Text
bearerToken req = lookup hAuthorization (requestHeaders req) >>= extractBearer
 where
  extractBearer bs
    | BSC.isPrefixOf "Bearer " bs =
        Just (Text.strip (TE.decodeUtf8With TE.lenientDecode (BSC.drop 7 bs)))
    | otherwise = Nothing

lookupProducer :: AuthTables -> Text -> Maybe ProducerCredential
lookupProducer tables token = Map.lookup token (atProducers tables)

lookupReader :: AuthTables -> Text -> Maybe ReaderCredential
lookupReader tables token = Map.lookup token (atReaders tables)
