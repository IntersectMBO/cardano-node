{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.FrequencyLimiter (
  limitFrequency,
  LimitingMessage(..)
)where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift
import qualified Control.Tracer as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Functor.Contravariant
import           Data.Text (Text, unpack)
import           Data.Time.Clock.System
import           Debug.Trace
import           GHC.Generics

import           Cardano.Logging.Trace
import           Cardano.Logging.Types

data LimitingMessage =
    StartLimiting Text
    -- ^ This message indicates the start of frequency limiting
  | StopLimiting Text Int
    -- ^ This message indicates the stop of frequency limiting,
    -- and gives the number of messages that has been suppressed
  deriving (Eq, Ord, Show, Generic)

-- TODO Needs handwritten instance
instance A.ToJSON LimitingMessage where
    toEncoding = A.genericToEncoding A.defaultOptions

data FrequencyRec a = FrequencyRec {
    frMessage  :: Maybe a   -- ^ The message to pass
  , frLastTime :: Double    -- ^ The time since the last message did arrive in seconds
  , frBudget   :: Double    -- ^ A budget which is used to decide when to start limiting
                              --   and stop limiting. When messages arrive in shorter frquency then
                              --   by the given thresholdFrequency budget is spend, and if they
                              --   arrive in a longer period budget is earned.
                              --   A value between 1.0 and -1.0. If -1.0 is reached start limiting,
                              --   and if 1.0 is reached stop limiting.
  , frActive   :: Maybe (Int, Double)
                              -- ^ Just is active and carries the number
                              --   of suppressed messages and the time of last send message
} deriving (Show)

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
  :: forall a acc m . (MonadIO m, MonadUnliftIO m)
  => Double   -- messages per second
  -> Text     -- name of this limiter
  -> Trace m a -- the limited trace
  -> Trace m LimitingMessage -- the limiters messages
  -> m (Trace m a) -- the original trace
limitFrequency thresholdFrequency limiterName vtracer ltracer = do
    timeNow <- systemTimeToSeconds <$> liftIO getSystemTime
    foldMTraceM
      (cata (1.0 / thresholdFrequency))
      (FrequencyRec Nothing  timeNow 0.0 Nothing)
      (Trace $ T.contramap prepare (unpackTrace (filterTraceMaybe vtracer)))
  where
    prepare ::
         (LoggingContext, Maybe TraceControl, Folding a (FrequencyRec a))
      -> (LoggingContext, Maybe TraceControl, Maybe a)
    prepare (lc, mbC, Folding FrequencyRec {..}) = (lc, mbC, frMessage)

    cata :: Double -> FrequencyRec a -> a -> m (FrequencyRec a)
    cata thresholdPeriod fs@FrequencyRec {..} message = do
      timeNow <- liftIO $ systemTimeToSeconds <$> getSystemTime
      let elapsedTime      = timeNow - frLastTime
      let rawSpendReward   = elapsedTime - thresholdPeriod
                                    -- negative if shorter, positive if longer
      let normaSpendReward = rawSpendReward * thresholdFrequency -- TODO not really normalized
      let spendReward      = min 0.5 (max (-0.5) normaSpendReward)
      let newBudget        = min 1.0 (max (-1.0) (spendReward + frBudget))
      -- trace ("elapsedTime " ++ show elapsedTime
      --        ++ " thresholdPeriod " ++ show thresholdPeriod
      --        ++ " rawSpendReward " ++ show rawSpendReward
      --        ++ " normaSpendReward "  ++ show normaSpendReward
      --        ++ " spendReward "       ++ show spendReward
      --        ++ " newBudget "       ++ show newBudget $
      case frActive of
        Nothing -> -- not active
          if spendReward + frBudget <= -1.0
            then do  -- start limiting
              traceWith
                (setSeverity Info ltracer)
                (StartLimiting limiterName)
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frBudget      = newBudget
                       , frActive      = Just (0, timeNow)
                       }
            else  -- continue without limiting
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frBudget      = newBudget
                       }
        Just (nSuppressed, lastTimeSend) -> -- is active
          if spendReward + frBudget >= 1.0
            then do -- stop limiting
              traceWith
                (setSeverity Info ltracer)
                (StopLimiting limiterName nSuppressed)
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frBudget      = newBudget
                       , frActive      = Nothing
                       }
            else
              let lastPeriod = timeNow - lastTimeSend
              in
              -- trace ("lastPeriod " ++ show lastPeriod
              --        ++ " thresholdPeriod " ++ show thresholdPeriod) $
                  if lastPeriod > thresholdPeriod
                    then -- send
                      pure fs  { frMessage     = Just message
                               , frLastTime    = timeNow
                               , frBudget      = newBudget
                               , frActive      = Just (nSuppressed, timeNow)
                               }
                    else  -- suppress
                      pure fs  { frMessage     = Nothing
                               , frLastTime    = timeNow
                               , frBudget      = newBudget
                               , frActive      = Just (nSuppressed + 1, lastTimeSend)
                               }

systemTimeToSeconds :: SystemTime -> Double
systemTimeToSeconds MkSystemTime {..} =
  fromIntegral systemSeconds + fromIntegral systemNanoseconds * 1.0E-9
