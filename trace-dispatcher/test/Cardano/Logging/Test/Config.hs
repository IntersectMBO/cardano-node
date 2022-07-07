{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Logging.Test.Config (
    config1
  , config2
  , config3
  , config4
  ) where

import           Data.Map (fromList)
import           Test.QuickCheck

import           Cardano.Logging


-- | different configurations for testing
config1 :: TraceConfig
config1 = emptyTraceConfig {
  tcOptions = fromList
    [([] :: Namespace,
         [ ConfSeverity (SeverityF (Just Debug))
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured, Forwarder, EKGBackend]
         ])
    ]
  }

config2 :: TraceConfig
config2 = emptyTraceConfig {
  tcOptions = fromList
    [ ([] :: Namespace,
         [ ConfSeverity (SeverityF (Just Debug))
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured, Forwarder, EKGBackend]
         ])
    , (["Test", "Message1"],
         [ ConfSeverity (SeverityF (Just Info))
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured, EKGBackend]
         ])
    , (["Test", "Message2"],
         [ ConfSeverity (SeverityF (Just Error))
         , ConfDetail DMinimal
         , ConfBackend [Forwarder, EKGBackend]
         ])
    ]
  }


config3 :: TraceConfig
config3 = emptyTraceConfig {
  tcOptions = fromList
    [ ([] :: Namespace,
         [ ConfSeverity (SeverityF (Just Debug))
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured, Forwarder, EKGBackend]
         ])
    , (["Test", "Message1"],
         [ ConfSeverity (SeverityF (Just Debug))
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured, EKGBackend]
         , ConfLimiter 100
         ])
    , (["Test", "Message2"],
         [ ConfSeverity (SeverityF (Just Error))
         , ConfDetail DMinimal
         , ConfBackend [Forwarder, EKGBackend]
         ])
    ]
  }

-- | different configurations for testing
config4 :: TraceConfig
config4 = emptyTraceConfig {
  tcOptions = fromList
    [([] :: Namespace,
         [ ConfSeverity (SeverityF (Just Debug))
         , ConfDetail DNormal
         , ConfBackend [EKGBackend]
         ])
    ]
  }

instance Arbitrary TraceConfig where
  arbitrary = elements [config1, config2]
