{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.FrequencyLimiter where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Text (Text, unpack)
import           Data.Time (UTCTime, diffUTCTime, getCurrentTime,
                     nominalDiffTimeToSeconds)
import           Debug.Trace
import           GHC.Generics
import           System.Random

import           Cardano.Logging.Trace
import           Cardano.Logging.Types

data LimitingMessage =
    StartLimiting {name :: Text, factor :: Double}
  | ContinueLimiting {name :: Text, factor :: Double}  -- Should be shown only for debugging
  | StopLimiting {name :: Text, factor :: Double}
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON LimitingMessage where
    toEncoding = A.genericToEncoding A.defaultOptions

data FrequencyRec a = FrequencyRec {
    frMessage  :: Maybe a
  , frLastTime :: UTCTime
  , frMsgCount :: Int
  , frTicks    :: Int
  , frActive   :: Maybe Double}
  deriving (Show)

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
  => Int   -- messages per minute
  -> Text  -- name of this limiter
  -> Trace m a -- the limited trace
  -> Trace m LimitingMessage -- the limiters messages
  -> m (Trace m a) -- the original trace
limitFrequency nMsg limiterName vtracer ltracer =
    let ticks = max 1 (round (5.0 * (60.0 / fromIntegral nMsg)))
        treshold = (fromIntegral nMsg / 60.0) * fromIntegral ticks
    in do
      timeNow <- trace ("name : "++ unpack limiterName ++ " ticks: " ++ show ticks ++ " treshold: " ++ show treshold) $
        liftIO getCurrentTime
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
      let timeDiffSec = nominalDiffTimeToSeconds (diffUTCTime timeNow frLastTime)
      case frActive of
        Nothing ->  trace ("inactive timeDiffSec: " ++ show timeDiffSec) $ -- not active
          if timeDiffSec > 1.0
            then trace "ticking passive" $ -- ticking
              if frTicks >= ticks
                then trace ("checking frMsgCount: " ++ show frMsgCount ++ " treshold: " ++ show treshold) $ -- in a check cycle
                  if fromIntegral frMsgCount > treshold
                    then trace "start limiting" $ do -- start limiting
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
                    then trace ("start limiting 2 ticks: "  ++ show ticks ++ " frTicks: " ++ show frTicks
                                ++ " frMsgCount: " ++ show frMsgCount ++ " treshold: " ++ show treshold) $ do  -- start limiting inbetween
                      let preFactor = fromIntegral ticks / fromIntegral (frTicks + 1)
                      let limitingFactor = treshold / (fromIntegral frMsgCount * preFactor)
                      traceWith
                        (setSeverity Info ltracer)
                        (StartLimiting limiterName limitingFactor)
                      pure fs  { frMessage   = Just message
                               , frLastTime  = timeNow
                               , frTicks     = 0
                               , frMsgCount  = 0
                               , frActive    = Just limitingFactor}
                    else
                      pure fs  { frMessage   = Just message
                               , frLastTime  = timeNow
                               , frMsgCount  = frMsgCount + 1
                               , frTicks     = frTicks + 1}
            -- Not active, not at second boundary, just pass and count
            else pure $ fs  { frMessage   = Just message
                            , frMsgCount  = frMsgCount + 1}
        Just percentage ->  -- Active
          if timeDiffSec > 1.0
            then trace ("ticking active " ++ unpack limiterName) $
              if frTicks >= ticks
                then trace ("checking frMsgCount: " ++ show frMsgCount ++ " treshold: " ++ show treshold)  $ -- ticking-- active, second and ticking
                  if fromIntegral frMsgCount > treshold
                    then trace "stay active " $ do -- continue
                      let limitingFactor = treshold / fromIntegral frMsgCount
                      traceWith
                       (setSeverity Debug ltracer)
                       (ContinueLimiting limiterName limitingFactor)
                      pure fs
                         {frMessage = Just message,
                          frLastTime = timeNow,
                          frMsgCount = 0,
                          frTicks = 0,
                          frActive = Just limitingFactor}
                    else trace "stop active " $ do -- stop
                      traceWith (setSeverity Info ltracer) (StopLimiting limiterName 1.0)
                      pure fs
                         {frMessage = Just message,
                          frLastTime = timeNow,
                          frMsgCount = 0,
                          frTicks = 0,
                          frActive = Nothing}
                else do -- ticking
                  rnd :: Double <- liftIO randomIO
                  if percentage > rnd
                    then -- sending the message
                      pure $ fs  { frMessage   = Just message
                                 , frLastTime  = timeNow
                                 , frTicks     = frTicks + 1
                                 , frMsgCount  = frMsgCount + 1}
                    else -- suppress the message
                      pure  $ fs { frMessage   = Nothing
                                 , frLastTime  = timeNow
                                 , frTicks     = frTicks + 1
                                 , frMsgCount  = frMsgCount + 1}
            else do
              rnd :: Double <- liftIO randomIO
              if percentage > rnd
                then -- sending the message
                  pure $ fs  { frMessage   = Just message
                             , frMsgCount  = frMsgCount + 1}
                else -- suppress the message
                  pure  $ fs { frMessage   = Nothing
                             , frMsgCount  = frMsgCount + 1}
