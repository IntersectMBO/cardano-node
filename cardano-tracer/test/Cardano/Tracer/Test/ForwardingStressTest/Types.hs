{-# LANGUAGE ViewPatterns #-}

module Cardano.Tracer.Test.ForwardingStressTest.Types (
    MessageID
  , Message (..)
  , ScriptedMessage (..)
  , Script (..)
  , ScriptRes (..)
  , scriptLength
  , scriptMessages
  , emptyScriptRes
  ) where

import           Cardano.Logging
import qualified Cardano.Tracer.Test.Utils as Utils

import           Data.Aeson (FromJSON (..), Value (..), withObject, (.:), (.=))
import           Data.Text hiding (length, reverse)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Text.Read (readMaybe)

import           Test.QuickCheck

type MessageID = Int

data Message =
    Message1 MessageID Int
  | Message2 MessageID Text
  | Message3 MessageID Double
  deriving (Eq, Ord, Show)

instance FromJSON Message where
  parseJSON = withObject "Message" \obj -> do
    kind     <- obj .: "kind"
    mid      <- obj .: "mid"
    workload <- obj .: "workload"
    pure case (kind, stripBrackets mid, workload) of
      (String "Message1", Just (readMaybe @Int -> Just mid'), String (readMaybe @Int . unpack -> Just workload')) -> Message1 mid' workload'
      (String "Message2", Just (readMaybe @Int -> Just mid'), String workload') -> Message2 mid' workload'
      (String "Message3", Just (readMaybe @Int -> Just mid'), String (readMaybe @Double . unpack -> Just workload')) -> Message3 mid' workload'
      _ -> error ".."

stripBrackets :: String -> Maybe String
stripBrackets ('<':str) =
  case reverse str of
    '>':str' -> Just (reverse str')
    _ -> Nothing
stripBrackets _ = Nothing

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
            , "mid" .= String ("<" <> showT mid <> ">")
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

newtype Script = Script (Vector ScriptedMessage)
  deriving (Eq, Show)

scriptLength :: Script -> Int
scriptLength (Script m) = Vector.length m

scriptMessages :: Script -> Vector Message
scriptMessages (Script messages) = fmap (\(ScriptedMessage _ message) -> message) messages

instance Arbitrary Script where
  arbitrary = Script <$> Utils.vectorOf arbitrary

-- TODO: Where is this used?
data ScriptRes = ScriptRes {
    srScript     :: Script
  , srStdoutRes  :: [FormattedMessage]
  , srForwardRes :: [FormattedMessage]
  , srEkgRes     :: [FormattedMessage]
  }

emptyScriptRes :: ScriptRes
emptyScriptRes =  ScriptRes {
    srScript = Script Vector.empty
  , srStdoutRes = []
  , srForwardRes = []
  , srEkgRes = []
}
