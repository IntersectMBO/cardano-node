{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.FrequencyLimiter where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.Text (Text)
import           Data.Time (UTCTime, diffUTCTime, getCurrentTime,
                     nominalDiffTimeToSeconds)
import           System.Random

import           Cardano.Logging.Trace
import           Cardano.Logging.Types


type FrequencyLimited a = (Maybe a, Maybe LimitingMessage)

data LimitingMessage =
    StartLimiting Text Double
  | ContinueLimiting Text Double  -- Just for debugging
  | StopLimiting Text

data FrequencyStructure a = FrequencyStructure {
    fsMessage  :: FrequencyLimited a
  , fsLastTime :: UTCTime
  , fsMsgCount :: Int
  , fsActive   :: Maybe Double
}

-- | Limits the frequency of messages to msgPer10Seconds
-- If the limiter detects more messages, it traces a StartLimiting message
-- with the current percentage given as a floating point number between 1.0 and 0.0
-- Then it randomly selects messages with the given percentage
-- until the frequency falls under the treshold.
-- Then it sends a StopLimiting message and traces all messages again.
-- Inbetween you can receive ContinueLimiting messages, with the current
-- percentage given as a floating point number between 1.0 and 0.0
limitFrequency
  :: forall a acc m . MonadIO m
  => Int
  -> Text
  -> Trace m a
  -> Trace m LimitingMessage
  -> m (Trace m a)
limitFrequency limiting limiterName vtracer ltracer = undefined



-- | Limits the frequency of messages to msgPer10Seconds
-- If the limiter detects more messages, it traces a StartLimiting messages
-- and then randomly selects messages until the frequency falls under the
-- treshold. Then it sends a StopLimiting message and traces all messages again.
-- Inbetween you can receive continue limiting messages, with the current
-- percentage given as a floating point number between 1.0 and 0.0
limitFrequency'
  :: forall a acc m . MonadIO m
  => Int
  -> Text
  -> Trace m (FrequencyLimited a)
  -> m (Trace m a)
limitFrequency' limiting limiterName tr = do
    timeNow            <- liftIO getCurrentTime
    let initialMessage = (Nothing, Nothing)
    let initial        = FrequencyStructure initialMessage timeNow 0 Nothing
    let tr'            = T.contramap prepare tr
    foldMTraceM cata initial tr'
  where
    prepare ::
         (LoggingContext, Either TraceControl (Folding a (FrequencyStructure a)))
      -> (LoggingContext, Either TraceControl (FrequencyLimited a))
    prepare (lc, Left c)                                  = (lc, Left c)
    prepare (lc, Right (Folding FrequencyStructure {..})) = (lc, Right fsMessage)

    cata :: FrequencyStructure a -> a -> m (FrequencyStructure a)
    cata fs@FrequencyStructure {..} message = do
      timeNow <- liftIO getCurrentTime
      let timeDiffPico = nominalDiffTimeToSeconds (diffUTCTime timeNow fsLastTime)
      let timeDiffSec  = timeDiffPico * 1000000000000.00
      case fsActive of
        Nothing ->  -- not active
          if timeDiffSec > 1
            then -- more then a second has passed
              if fsMsgCount > limiting
                then -- start limiting
                  pure $
                    let limitingFactor = fromIntegral limiting / fromIntegral fsMsgCount
                    in fs  { fsMessage   = (Just message,
                                            Just (StartLimiting limiterName limitingFactor))
                           , fsLastTime  = timeNow
                           , fsMsgCount  = 0
                           , fsActive    = Just limitingFactor}
                else -- continue new second without limiting
                  pure $ fs { fsMessage   = (Just message, Nothing)
                            , fsLastTime  = timeNow
                            , fsMsgCount  = 0}
            -- Not active, not at second boundary, jsut pass and count
            else pure $ fs  { fsMessage   = (Just message, Nothing)
                            , fsMsgCount  = fsMsgCount + 1}
        Just percentage ->
          if timeDiffSec > 1
            then
              if fsMsgCount > limiting
                then -- Continue Limiting
                  pure $
                    let limitingFactor = fromIntegral limiting
                                          / fromIntegral fsMsgCount
                    in  fs { fsMessage   = (Just message,
                                        Just (ContinueLimiting limiterName limitingFactor))
                           , fsLastTime  = timeNow
                           , fsMsgCount  = 0
                           , fsActive    = Just limitingFactor}
                else -- stop limiting
                  pure $
                   fs { fsMessage       = (Just message, Just (StopLimiting limiterName))
                          , fsLastTime  = timeNow
                          , fsMsgCount  = 0
                          , fsActive    = Nothing}
            else do -- with active limiting
              rnd :: Double <- liftIO randomIO
              if percentage > rnd
                then -- sending the message
                  pure $ fs  { fsMessage   = (Just message, Nothing)
                             , fsMsgCount  = fsMsgCount + 1}
                else -- suppress the message
                  pure  $ fs { fsMessage   = (Nothing, Nothing)
                             , fsMsgCount  = fsMsgCount + 1}
