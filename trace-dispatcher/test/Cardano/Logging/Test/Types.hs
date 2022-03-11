module Cardano.Logging.Test.Types (
    MessageID
  , Message (..)
  , ScriptedMessage (..)
  , Script (..)
  , ScriptRes (..)
  , scriptLength
  , emptyScriptRes
  ) where

import           Data.Aeson (Value (..), (.=))
import           Data.Text hiding (length)
import           Test.QuickCheck

import           Cardano.Logging

type MessageID = Int

data Message =
    Message1 MessageID Int
  | Message2 MessageID Text
  | Message3 MessageID Double
  deriving (Eq, Ord, Show)

instance LogFormatting Message where
  forMachine _dtal (Message1 mid i) =
    mconcat [ "kind" .= String "Message1"
            , "mid" .= ("<" <> showT mid <> ">")
            , "workload" .= String (showT i)
            ]
  forMachine DMinimal (Message2 mid _s) =
    mconcat [ "mid" .= ("<" <> showT mid <> ">")
            , "kind" .= String "Message2"
            ]
  forMachine _dtal (Message2 mid s) =
    mconcat [ "kind" .= String "Message2"
            , "mid" .= String ("<" <> showT mid <> ">")
            , "workload" .= String s
            ]
  forMachine _dtal (Message3 mid d) =
    mconcat [ "kind" .= String "Message3"
            , "mid" .= String (showT mid)
            , "workload" .= String (showT d)
            ]
  forHuman (Message1 mid i) =
      "Message1 <" <> showT mid <> "> " <> showT i
  forHuman (Message2 mid s) =
      "Message2 <" <> showT mid <> "> " <> s
  forHuman (Message3 mid d) =
      "Message3 <" <> showT mid <> "> " <> showT d
  asMetrics (Message1 mid _i) =
      [ IntM "Metrics1" (fromIntegral mid)
      , IntM "Metrics2" (fromIntegral mid)
      , IntM "Metrics3" (fromIntegral mid)
      , IntM "Metrics4" (fromIntegral mid)
      , IntM "Metrics5" (fromIntegral mid)]
  asMetrics _ = []

instance Arbitrary Message where
  arbitrary = oneof
    [ Message1 0 <$> arbitrary,
      Message2 0 <$> elements ["Hallo", "Goodbye", "Whatelse"],
      Message3 0 <$> arbitrary
    ]


-- | Adds a time between 0 and 1.
--   0 is the time of the test start, and 1 the test end
data ScriptedMessage = ScriptedMessage Double Message
  deriving (Eq, Show)

-- Ordered by time
instance Ord ScriptedMessage where
  compare (ScriptedMessage d1 _m1) (ScriptedMessage d2 _m2) = compare d1 d2

instance Arbitrary ScriptedMessage where
  arbitrary = ScriptedMessage <$> choose (0.0, 1.0) <*> arbitrary

newtype Script = Script [ScriptedMessage]
  deriving (Eq, Show)

scriptLength :: Script -> Int
scriptLength (Script m) = length m

instance Arbitrary Script where
  arbitrary = Script <$> listOf arbitrary

data ScriptRes = ScriptRes {
    srScript     :: Script
  , srStdoutRes  :: [FormattedMessage]
  , srForwardRes :: [FormattedMessage]
  , srEkgRes     :: [FormattedMessage]
  }

emptyScriptRes :: ScriptRes
emptyScriptRes =  ScriptRes {
    srScript = Script []
  , srStdoutRes = []
  , srForwardRes = []
  , srEkgRes = []
}
