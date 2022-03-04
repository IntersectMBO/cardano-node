{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.FrequencyLimiter (
    limitFrequency
  , LimitingMessage(..)
  , LimiterSpec (..)
)where

import           Control.Monad.IO.Unlift
import qualified Control.Tracer as T
import           Data.Aeson (Value (..), (.=))
import           Data.Text (Text, pack)
import           Data.Time.Clock.System
import           GHC.Generics

import           Cardano.Logging.Trace
import           Cardano.Logging.Types

-- | Threshold for starting and stopping of the limiter
budgetLimit :: Double
budgetLimit = 30.0

-- | After how many seconds a reminder message is send
reminderPeriod :: Double
reminderPeriod = 10.0

data LimiterSpec = LimiterSpec {
    lsNs        :: [Text]
  , lsName      :: Text
  , lsFrequency :: Double
}

data LimitingMessage =
    StartLimiting Text
    -- ^ This message indicates the start of frequency limiting
  | StopLimiting Text Int
    -- ^ This message indicates the stop of frequency limiting,
    -- and gives the number of messages that has been suppressed
  | RememberLimiting Text Int
    -- ^ This message remembers of ongoing frequency limiting,
    -- and gives the number of messages that has been suppressed

  deriving (Eq, Ord, Show, Generic)

instance LogFormatting LimitingMessage where
  forHuman (StartLimiting txt) = "Start of frequency limiting for " <> txt
  forHuman (StopLimiting txt num) = "Stop of frequency limiting for " <> txt <>
    ". Suppressed " <> pack (show num) <> " messages."
  forHuman (RememberLimiting txt num) = "Frequency limiting still active for " <> txt <>
    ". Suppressed so far " <> pack (show num) <> " messages."
  forMachine _dtl (StartLimiting txt) = mkObject
        [ "kind" .= String "StartLimiting"
        , "name" .= String txt
        ]
  forMachine _dtl (StopLimiting txt num) = mkObject
        [ "kind" .= String "StopLimiting"
        , "name" .= String txt
        , "numSuppressed" .= Number (fromIntegral num)
        ]
  forMachine _dtl (RememberLimiting txt num) = mkObject
        [ "kind" .= String "RememberLimiting"
        , "name" .= String txt
        , "numSuppressed" .= Number (fromIntegral num)
        ]
  asMetrics (StartLimiting _txt)          = []
  asMetrics (StopLimiting txt num)        = [IntM
                                              ("SuppressedMessages " <> txt)
                                              (fromIntegral num)]
  asMetrics (RememberLimiting _txt _num)  = []

data FrequencyRec a = FrequencyRec {
    frMessage  :: Maybe a   -- ^ The message to pass
  , frLastTime :: Double    -- ^ The time since the last message did arrive in seconds
  , frLastRem  :: Double    -- ^ The time since the last limiting remainder was send
  , frBudget   :: Double    -- ^ A budget which is used to decide when to start limiting
                              --   and stop limiting. When messages arrive in shorter frequency then
                              --   by the given thresholdFrequency budget is earned, and if they
                              --   arrive in a longer period budget is spend.
  , frActive   :: Maybe (Int, Double)
                              -- ^ Just is active and carries the number
                              --   of suppressed messages and the time of last send message
} deriving (Show)

-- | Limits the frequency of messages to nMsg which is given per minute.
--
-- If the limiter detects more messages, it traces randomly selected
-- messages with the given frequency on the 'vtracer' until the
-- frequency falls under the threshold long enough.(see below)
--
-- Before this the 'ltracer' gets a 'StartLimiting' message.
-- In-between you receive 'ContinueLimiting' messages on the 'ltracer'
-- every 'reminderPeriod' seconds, with the number of suppressed messages.
-- Finally it sends a 'StopLimiting' message on the 'ltracer' and traces all
-- messages on the 'vtracer' again.
--
-- A budget is used to decide when to start limiting and stop limiting,
-- so that the limiter does not get activated if few messages are send in
-- high frequency, and doesn't get deactivated if their are only few messages
-- which come with low frequency.  When messages arrive in shorter frequency then
-- by the given 'thresholdFrequency' budget is earned, and if they
-- arrive in a longer period budget is spend. If budget is gets higher
-- then 'budgetLimit', the limiter starts, and if it falls below minus 'budgetLimit'
-- the limiter stops.

-- The budget is calculated by 'thresholdPeriod' / 'elapsedTime', which says how
-- many times too quick the message arrives. A value less then 1.0 means the message is
-- arriving slower then threshold. This value gets then normalized, so that
-- (0.0-10.0) means message arrive quicker then threshold and (0.0..-10.0)
-- means that messages arrive slower then threshold.


limitFrequency
  :: forall a m . (MonadIO m, MonadUnliftIO m)
  => Double   -- messages per second
  -> Text     -- name of this limiter
  -> Trace m a -- the limited trace
  -> Trace m LimitingMessage -- the limiters messages
  -> m (Trace m a) -- the original trace
limitFrequency thresholdFrequency limiterName vtracer ltracer = do
    timeNow <- systemTimeToSeconds <$> liftIO getSystemTime
    foldMTraceM
      (checkLimiting (1.0 / thresholdFrequency))
      (FrequencyRec Nothing timeNow 0.0 0.0 Nothing)
      (Trace $ T.contramap unfoldTrace (unpackTrace (filterTraceMaybe vtracer)))
  where
    checkLimiting ::
         Double
      -> FrequencyRec a
      -> LoggingContext
      -> Maybe TraceControl
      -> a
      -> m (FrequencyRec a)
    checkLimiting _thresholdPeriod fs@FrequencyRec{} lc (Just c) message = do
      T.traceWith
        (unpackTrace ltracer)
        (lc, Just c, StartLimiting "configure")
      pure fs {frMessage = Just message}
    checkLimiting thresholdPeriod fs@FrequencyRec{..} lc Nothing message = do
      timeNow <- liftIO $ systemTimeToSeconds <$> getSystemTime
      let elapsedTime      = timeNow - frLastTime
      -- How many times too quick does the message arrive (thresholdPeriod / elapsedTime)
      -- A value less then 1.0 means the message is
      -- arriving slower then threshold
      let rawSpendReward   = if elapsedTime == 0.0
                                then 10.0
                                else thresholdPeriod / elapsedTime
      let spendReward = if rawSpendReward < 1.0 && rawSpendReward > 0.0
                                then - ((1.0 / rawSpendReward) - 1.0)
                                else rawSpendReward - 1.0
      -- Normalize so that (0.0-10.0) means message
      -- arrive quicker then threshold
      -- and (0.0..-10.0) means that messages arrive
      -- slower then threshold
      let normaSpendReward = min 10.0 (max (-10.0) spendReward)
      let newBudget        = min budgetLimit (max (-budgetLimit)
                                  (normaSpendReward + frBudget))
      case frActive of
        Nothing -> -- limiter not active
          if normaSpendReward + frBudget >= budgetLimit
            then do  -- start limiting
              traceWith
                (setSeverity Info (withLoggingContext lc ltracer))
                (StartLimiting limiterName)
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frLastRem     = timeNow
                       , frBudget      = newBudget
                       , frActive      = Just (0, timeNow)
                       }
            else  -- continue without limiting
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frLastRem     = 0.0
                       , frBudget      = newBudget
                       }
        Just (nSuppressed, lastTimeSend) -> -- is active
           if normaSpendReward + frBudget <= (- budgetLimit)
            then do -- stop limiting
              traceWith
                (setSeverity Info (withLoggingContext lc ltracer))
                (StopLimiting limiterName nSuppressed)
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frBudget      = newBudget
                       , frActive      = Nothing
                       }
            else
              let lastPeriod = timeNow - lastTimeSend
                  lastReminder = timeNow - frLastRem
              in do
                newFrLastRem <- if lastReminder > reminderPeriod
                                  then do
                                    traceWith
                                      (setSeverity Info
                                        (withLoggingContext lc ltracer))
                                      (RememberLimiting limiterName nSuppressed)
                                    pure timeNow
                                  else pure frLastRem
                if lastPeriod > thresholdPeriod
                  then -- send
                    pure fs  { frMessage     = Just message
                             , frLastTime    = timeNow
                             , frLastRem     = newFrLastRem
                             , frBudget      = newBudget
                             , frActive      = Just (nSuppressed, timeNow)
                             }
                  else  -- suppress
                    pure fs  { frMessage     = Nothing
                             , frLastTime    = timeNow
                             , frLastRem     = newFrLastRem
                             , frBudget      = newBudget
                             , frActive      = Just (nSuppressed + 1, lastTimeSend)
                             }
    unfoldTrace ::
         (LoggingContext, Maybe TraceControl, Folding a (FrequencyRec a))
      -> (LoggingContext, Maybe TraceControl, Maybe a)
    unfoldTrace (lc, mbC, Folding FrequencyRec {..}) = (lc, mbC, frMessage)

    systemTimeToSeconds :: SystemTime -> Double
    systemTimeToSeconds MkSystemTime {..} =
      fromIntegral systemSeconds + fromIntegral systemNanoseconds * 1.0E-9
