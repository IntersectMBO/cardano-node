{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.FrequencyLimiter where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift
import qualified Control.Tracer as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Text (Text, unpack)
import           Data.Time.Clock.System
import           Debug.Trace
import           GHC.Generics

import           Cardano.Logging.Trace
import           Cardano.Logging.Types

data LimitingMessage =
    StartLimiting    {name :: Text}
    -- ^ This message indicates the start of frequency limiting
  | StopLimiting     {name :: Text, suppressed :: Int}
    -- ^ This message indicates the stop of frequency limiting,
    -- and gives the number of messages that has been suppressed
  deriving (Eq, Ord, Show, Generic)

instance A.ToJSON LimitingMessage where
    toEncoding = A.genericToEncoding A.defaultOptions

data FrequencyRec a = FrequencyRec {
    frMessage    :: Maybe a   -- ^ The message to pass
  , frLastTime   :: Double    -- ^ The time since the last message did pass in seconds
  , frBonusMalus :: Double    -- ^ A value between 1.0, meaning few messages come through
                              --   and if active stop limiting and -1.0, meaning to much
                              --   messages pass and if not active start limiting
  , frActive     :: Maybe (Int, Double)
                              -- ^ Just is active and carries the number
                              -- of suppressed messages and the time of last send message
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
limitFrequency nMsgPerSecond limiterName vtracer ltracer = do
    timeNow <- systemTimeToSeconds <$> liftIO getSystemTime
    foldMTraceM
      cata
      (FrequencyRec Nothing  timeNow 0.0 Nothing)
      (T.contramap prepare (filterTraceMaybe vtracer))
  where
    prepare ::
         (LoggingContext, Either TraceControl (Folding a (FrequencyRec a)))
      -> (LoggingContext, Either TraceControl (Maybe a))
    prepare (lc, Left c)                            = (lc, Left c)
    prepare (lc, Right (Folding FrequencyRec {..})) = (lc, Right frMessage)

    cata :: FrequencyRec a -> a -> m (FrequencyRec a)
    cata fs@FrequencyRec {..} message = do
      timeNow <- liftIO $ systemTimeToSeconds <$> getSystemTime
      let realTimeBetweenMsgs = timeNow - frLastTime
      let canoTimeBetweenMsgs = 1.0 / nMsgPerSecond
      let diffTimeBetweenMsgs = realTimeBetweenMsgs - canoTimeBetweenMsgs
                                    -- negative if too short, positive if longer
      let diffTimeNormalized  = diffTimeBetweenMsgs / canoTimeBetweenMsgs
      let bonusMalusAdd       = min 0.5 (max (-0.5) diffTimeNormalized)
      let newBonusMalus       = min 1.0 (max (-1.0) (bonusMalusAdd + frBonusMalus))
      -- trace ("realTimeBetweenMsgs " ++ show realTimeBetweenMsgs
      --        ++ " canoTimeBetweenMsgs " ++ show canoTimeBetweenMsgs
      --        ++ " diffTimeBetweenMsgs " ++ show diffTimeBetweenMsgs
      --        ++ " diffTimeNormalized "  ++ show diffTimeNormalized
      --        ++ " bonusMalusAdd "       ++ show bonusMalusAdd
      --        ++ " newBonusMalus "       ++ show newBonusMalus) $
      case frActive of
        Nothing ->
          if bonusMalusAdd + frBonusMalus <= -1.0
            then do  -- start limiting
              traceWith
                (setSeverity Info ltracer)
                (StartLimiting limiterName)
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frBonusMalus  = newBonusMalus
                       , frActive      = Just (0, timeNow)
                       }
            else  -- continue without limiting
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frBonusMalus  = newBonusMalus
                       }
        Just (nSuppressed, lastTimeSend) ->
          if bonusMalusAdd + frBonusMalus >= 1.0
            then do -- stop limiting
              traceWith
                (setSeverity Info ltracer)
                (StopLimiting limiterName nSuppressed)
              pure fs  { frMessage     = Just message
                       , frLastTime    = timeNow
                       , frBonusMalus  = newBonusMalus
                       , frActive      = Nothing
                       }
            else
              let realTimeBetweenMsgs2 = timeNow - lastTimeSend
              in
              -- trace ("realTimeBetweenMsgs2 " ++ show realTimeBetweenMsgs2
              --           ++ " canoTimeBetweenMsgs " ++ show canoTimeBetweenMsgs) $
                  if realTimeBetweenMsgs2 > canoTimeBetweenMsgs
                    then -- send
                      pure fs  { frMessage     = Just message
                               , frLastTime    = timeNow
                               , frBonusMalus  = newBonusMalus
                               , frActive      = Just (nSuppressed, timeNow)
                               }
                    else  -- suppress
                      pure fs  { frMessage     = Nothing
                               , frLastTime    = timeNow
                               , frBonusMalus  = newBonusMalus
                               , frActive      = Just (nSuppressed + 1, lastTimeSend)
                               }

systemTimeToSeconds :: SystemTime -> Double
systemTimeToSeconds MkSystemTime {..} =
  fromIntegral systemSeconds + fromIntegral systemNanoseconds * 1.0E-9
