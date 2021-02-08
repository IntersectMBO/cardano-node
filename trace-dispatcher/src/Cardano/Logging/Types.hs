{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Cardano.Logging.Types where

import           Control.Tracer
import           Data.Text (Text)
import           Katip

type Trace m a = Tracer m (LoggingContext, a)

data LoggingContext = LoggingContext {
    lcContext  :: [Text]
  , lcSeverity :: Maybe Severity
  , lcPrivacy  :: Maybe PrivacyAnnotation
  , lcKatip    :: Maybe LogEnv
}

data LoggingContextKatip = LoggingContextKatip {
    lk       :: LoggingContext
  , lkLogEnv :: LogEnv
}


emptyLoggingContext :: LoggingContext
emptyLoggingContext = LoggingContext [] Nothing Nothing Nothing

data PrivacyAnnotation =
      Confidential -- confidential information - handle with care
    | Public       -- indifferent - can be public.
    deriving (Show, Eq, Ord, Bounded, Enum)

data SeverityF
    = DebugF                   -- ^ Debug messages
    | InfoF                    -- ^ Information
    | NoticeF                  -- ^ Normal runtime Conditions
    | WarningF                 -- ^ General Warnings
    | ErrorF                   -- ^ General Errors
    | CriticalF                -- ^ Severe situations
    | AlertF                   -- ^ Take immediate action
    | EmergencyF               -- ^ System is unusable
    | SilenceF
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Folding a b = Folding b
  deriving (ToObject)
