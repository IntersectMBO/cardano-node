module Cardano.Logging.Test.Types (
    MessageID
  , Message (..)
  , ScriptedMessage (..)
  , Script (..)
  , ScriptRes (..)
  , scriptLength
  , emptyScriptRes
  , getMessageID
  , setMessageID
  ) where

import           Cardano.Logging

import           Data.Aeson (Value (..), (.=))
import           Data.Text hiding (length)

import           Test.QuickCheck

type MessageID = Int

data Message =
    Message1 MessageID Int
  | Message2 MessageID Text
  | Message3 MessageID Double
  deriving (Eq, Ord, Show)

getMessageID :: Message -> MessageID
getMessageID (Message1 mid _) = mid
getMessageID (Message2 mid _) = mid
getMessageID (Message3 mid _) = mid

setMessageID :: Message -> MessageID -> Message
setMessageID (Message1 _ v) mid = Message1 mid v
setMessageID (Message2 _ v) mid = Message2 mid v
setMessageID (Message3 _ v) mid = Message3 mid v

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

instance MetaTrace Message where
  namespaceFor  Message1 {} = Namespace [] ["Message1"]
  namespaceFor  Message2 {} = Namespace [] ["Message2"]
  namespaceFor  Message3 {} = Namespace [] ["Message3"]

  severityFor   (Namespace _ ["Message1"]) _ = Just Debug
  severityFor   (Namespace _ ["Message2"]) _ = Just Info
  severityFor   (Namespace _ ["Message3"]) _ = Just Error
  severityFor   _ns _ = Nothing

  privacyFor    (Namespace _ ["Message1"]) _ = Just Public
  privacyFor    (Namespace _ ["Message2"]) _ = Just Confidential
  privacyFor    (Namespace _ ["Message3"]) _ = Just Public
  privacyFor    _ns _ = Nothing

  documentFor   (Namespace _ ["Message1"]) = Just "The first message."
  documentFor   (Namespace _ ["Message2"]) = Just "The second message."
  documentFor   (Namespace _ ["Message3"]) = Just "The third message."
  documentFor   _ns = Nothing

  metricsDocFor (Namespace _ ["Message1"]) =
    [ ("Metrics1", "A number")
    , ("Metrics2", "A number")
    , ("Metrics3", "A number")
    , ("Metrics4", "A number")
    , ("Metrics5", "A number")
    ]
  metricsDocFor _ =  []

  allNamespaces = [ Namespace [] ["Message1"]
                  , Namespace [] ["Message2"]
                  , Namespace [] ["Message3"]]

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
