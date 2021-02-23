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

data LimitingMessage =
    StartLimiting Text Double
  | ContinueLimiting Text Double  -- Should be shown only for debugging
  | StopLimiting Text

data FrequencyRec a = FrequencyRec {
    frMessage  :: Maybe a
  , frLastTime :: UTCTime
  , frMsgCount :: Int
  , frTicks    :: Int
  , frActive   :: Maybe Double
}

-- | Limits the frequency of messages to nMsg which is given per minute.

-- If the limiter detects more messages, it traces randomly selected
-- messages with the given percentage
-- on the vtracer until the frequency falls under the treshold.

-- Before this the ltracer gets a StartLimiting message with the
-- current percentage given as a floating point number between 1.0 and 0.0.
-- Inbetween you can receive ContinueLimiting messages on the ltracer,
-- with the current percentage.
-- Finally it sends a StopLimiting message on the ltracer and traces all
-- messages on the vtracer again.
limitFrequency
  :: forall a acc m . MonadIO m
  => Int
  -> Text
  -> Trace m a
  -> Trace m LimitingMessage
  -> m (Trace m a)
limitFrequency nMsg limiterName vtracer ltracer =
    let ticks = max 1 (round (5.0 * (60.0 / fromIntegral nMsg)))
        treshold = (fromIntegral nMsg / 60.0) * fromIntegral ticks
    in do
      timeNow <- liftIO getCurrentTime
      foldMTraceM
        (cata ticks treshold)
        (FrequencyRec Nothing timeNow 0 0 Nothing)
        (T.contramap prepare (filterTraceMaybe vtracer))
  where
    prepare ::
         (LoggingContext, Either TraceControl (Folding a (FrequencyRec a)))
      -> (LoggingContext, Either TraceControl (Maybe a))
    prepare (lc, Left c)                            = (lc, Left c)
    prepare (lc, Right (Folding FrequencyRec {..})) = (lc, Right frMessage)

    cata :: Int -> Double -> FrequencyRec a -> a -> m (FrequencyRec a)
    cata ticks treshold fs@FrequencyRec {..} message = do
      timeNow <- liftIO getCurrentTime
      let timeDiffPico = nominalDiffTimeToSeconds (diffUTCTime timeNow frLastTime)
      let timeDiffSec  = timeDiffPico * 1000000000000.00
      case frActive of
        Nothing ->  -- not active
          if timeDiffSec > 1.0
            then -- ticking
              if frTicks + 1 >= ticks
                then -- in a check cycle
                  if fromIntegral frMsgCount > treshold
                    then do -- start limiting
                      let limitingFactor = treshold / fromIntegral frMsgCount
                      traceWith
                        (setSeverity Info ltracer)
                        (StartLimiting limiterName limitingFactor)
                      pure fs  { frMessage   = Just message
                               , frLastTime  = timeNow
                               , frMsgCount  = 0
                               , frTicks     = 0
                               , frActive    = Just limitingFactor}
                    else -- in a check cycle, but stay inactive
                      pure fs  { frMessage   = Just message
                               , frLastTime  = timeNow
                               , frMsgCount  = 0
                               , frTicks     = 0
                               , frActive    = Nothing}
                else -- ticking but not in a check cycle
                  if fromIntegral frMsgCount > treshold
                    then do -- start limiting inbetween
                      let preFactor = fromIntegral ticks / fromIntegral (frTicks + 1)
                      let limitingFactor = (treshold * preFactor) / fromIntegral frMsgCount
                      traceWith
                        (setSeverity Info ltracer)
                        (StartLimiting limiterName limitingFactor)
                      pure fs  { frMessage   = Just message
                               , frLastTime  = timeNow
                               , frMsgCount  = 0
                               , frTicks     = 0
                               , frActive    = Just limitingFactor}
                    else
                      pure fs  { frMessage   = Just message
                               , frLastTime  = timeNow
                               , frMsgCount  = 0
                               , frTicks     = frTicks + 1}
            -- Not active, not at second boundary, just pass and count
            else pure $ fs  { frMessage   = Just message
                            , frMsgCount  = frMsgCount + 1}
        Just percentage -> -- Active
          if (timeDiffSec > 1.0) && (frTicks + 1 >= ticks)
            then -- active, second and ticking
              (if fromIntegral frMsgCount > treshold
                then do -- continue
                  let limitingFactor = treshold / fromIntegral frMsgCount
                  traceWith
                   (setSeverity Debug ltracer)
                   (ContinueLimiting limiterName limitingFactor)
                  pure fs
                     {frMessage = Just message, frLastTime = timeNow, frMsgCount = 0,
                      frTicks = 0, frActive = Just limitingFactor}
                else do -- stop
                  traceWith (setSeverity Info ltracer) (StopLimiting limiterName)
                  pure fs
                    {frMessage = Just message, frLastTime = timeNow, frMsgCount = 0,
                     frTicks = 0, frActive = Nothing})
            else do
              rnd :: Double <- liftIO randomIO
              let newTicks = if timeDiffSec > 1.0 then frTicks + 1 else frTicks
              if percentage > rnd
                then -- sending the message
                  pure $ fs  { frMessage   = Just message
                             , frTicks     = newTicks
                             , frMsgCount  = frMsgCount + 1}
                else -- suppress the message
                  pure  $ fs { frMessage   = Nothing
                             , frTicks     = newTicks
                             , frMsgCount  = frMsgCount + 1}
