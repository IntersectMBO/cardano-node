{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Cardano.Logging.Types where

import           Control.Tracer
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           GHC.Generics


-- | Every message needs this to define how to represent it
class Logging a where
  -- | Machine readable representation with the possibility to represent
  -- with different details based on the detail level.
  -- Falls back to ToJson of Aeson in the default representation
  forMachine :: DetailLevel -> a -> A.Object
  default forMachine :: A.ToJSON a => DetailLevel -> a -> A.Object
  forMachine _ v = case A.toJSON v of
    A.Object o     -> o
    s@(A.String _) -> HM.singleton "string" s
    _              -> mempty

  -- | Human readable representation.
  forHuman :: a -> Text
  forHuman v = ""

  -- | Metrics representation.
  -- No metrics by default
  asMetrics :: a -> [Metric]
  asMetrics v = []

data Metric
  -- | An integer metric.
  -- If a text is given it is appended as last element to the namespace
    = IntM (Maybe Text) Int
  -- | A double metric.
  -- If a text is given it is appended as last element to the namespace
    | DoubleM (Maybe Text) Double
  deriving (Show, Eq, Generic)

type Namespace = [Text]
type Selector  = [Text]

-- | Tracer comes from the contra-tracer package and carries a context here
type Trace m a = Tracer m (LoggingContext, Either TraceControl a)

-- | Context of a message
data LoggingContext = LoggingContext {
    lcContext  :: Namespace
  , lcSeverity :: Maybe SeverityS
  , lcPrivacy  :: Maybe Privacy
  , lcDetails  :: Maybe DetailLevel}
  deriving (Show, Eq, Generic)

emptyLoggingContext :: LoggingContext
emptyLoggingContext = LoggingContext [] Nothing Nothing Nothing

-- | Formerly known as verbosity
data DetailLevel = DBrief | DRegular | DDetailed
    deriving (Show, Eq, Ord, Bounded, Enum, Generic)

-- | Privacy of a message
data Privacy =
      Public                    -- ^ can be public.
    | Confidential              -- ^ confidential information - handle with care
    deriving (Show, Eq, Ord, Bounded, Enum, Generic)

-- | Severity of a message
data SeverityS
    = Debug                   -- ^ Debug messages
    | Info                    -- ^ Information
    | Notice                  -- ^ Normal runtime Conditions
    | Warning                 -- ^ General Warnings
    | Error                   -- ^ General Errors
    | Critical                -- ^ Severe situations
    | Alert                   -- ^ Take immediate action
    | Emergency               -- ^ System is unusable
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

-- | Severity for a filter
data SeverityF
    = DebugF                   -- ^ Debug messages
    | InfoF                    -- ^ Information
    | NoticeF                  -- ^ Normal runtime Conditions
    | WarningF                 -- ^ General Warnings
    | ErrorF                   -- ^ General Errors
    | CriticalF                -- ^ Severe situations
    | AlertF                   -- ^ Take immediate action
    | EmergencyF               -- ^ System is unusable
    | SilenceF                 -- ^ Don't show anything
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

-- Configuration options for individual namespace elements
data ConfigOption =
    -- | Severity level for a filter (default is WarningF)
    CoSeverity SeverityF
    -- | Detail level (Default is DRegular)
  | CoDetail DetailLevel
    -- | Privacy level (Default is Public)
  | CoPrivacy Privacy
  deriving (Eq, Ord, Show, Generic)

data TraceConfig = TraceConfig {

     -- | Options specific to a certain namespace
    tcOptions :: Map.Map Namespace [ConfigOption]

  --  Forwarder:
     -- Can their only be one forwarder? Use one of:

     --  Forward messages to the following address
--  ,  tcForwardTo :: RemoteAddr

     --  Forward messages to the following address
--  ,  tcForwardTo :: Map TracerName RemoteAddr

  --  ** Katip:

--  ,  tcDefaultScribe :: ScribeDefinition

--  ,  tcScripes :: Map TracerName -> ScribeDefinition

  --  EKG:
     --  Port for EKG server
--  ,  tcPortEKG :: Int

  -- Prometheus:
    --  Host/port to bind Prometheus server at
--  ,  tcBindAddrPrometheus :: Maybe (String,Int)
}
  deriving (Eq, Ord, Show, Generic)

emptyTraceConfig = TraceConfig {tcOptions = Map.empty}

-- | When configuring a net of tracers, it should be run with Config on all
-- entry points first, and then with Optimize. When reconfiguring it needs to
-- run Reset followed by Config followed by Optimize
data TraceControl =
    Reset
  | Config TraceConfig
  | Optimize

-- | Type for a Fold
newtype Folding a b = Folding b

instance Logging b => Logging (Folding a b) where
  forMachine v (Folding b) =  forMachine v b
  forHuman (Folding b)     =  forHuman b
  asMetrics (Folding b)    =  asMetrics b

instance A.ToJSON Metric where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON LoggingContext where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON DetailLevel where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Privacy where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON SeverityS where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON SeverityF where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON ConfigOption where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON TraceConfig where
    toEncoding = A.genericToEncoding A.defaultOptions
