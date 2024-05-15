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

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Aeson (FromJSON (..), Object, Value (..), withObject, (.=))
import           Data.Aeson.Types (Parser, parseFail, -- parseEither,
                                   parseField)
import           Data.Text hiding (empty, length, reverse)
import qualified Data.Text as Text
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

-- | This instance is written by hand to parse the custom Message
-- format.
--
-- @
-- {"at":"2024-05-08T15:13:05.005807034Z","ns":["Test","Message1"],"data":..,"sev":"Debug","thread":"26","host":"KindStar"}
-- @
--
-- Where the @data@ field can be of the following form:
--
-- @
-- {"kind":"Message1","mid":"<1>","workload":"3"}
-- {"kind":"Message2","mid":"<11>","workload":"Hallo"}
-- {"kind":"Message3","mid":"<4>","workload":"0.7365603545830433"}
-- @
instance FromJSON Message where
  parseJSON :: Value -> Parser Message
  parseJSON a = parseFromTrace a <|> parseFromObj a

parseFromTrace :: Value -> Parser Message
parseFromTrace = withObject "Message" \obj -> do
  -- parseField = (.:), defined in `aeson'.
  parseFromObjInner =<< parseField @Object obj "data"

parseFromObj :: Value -> Parser Message
parseFromObj = withObject "Message" parseFromObjInner

parseFromObjInner :: Object -> Parser Message
parseFromObjInner obj = do
  messageId_ <- parseField obj "mid"
  messageId <- maybe (parseFail "parseFromObjInner: No `mid' field found.") pure
    (stripBrackets messageId_ >>= readMaybe @MessageID . unpack)

  kind     <- parseField @String obj "kind"
  workload <- parseField @String obj "workload"
  case kind of
    "Message1" ->
      case readMaybe @Int workload of
        Just workloadInt -> pure $ Message1 messageId workloadInt
        Nothing -> parseFail ("FromJSON Message: Could not parse 'workload' field: " ++ workload ++ "\n  + " ++ show obj)
    "Message2" ->
      pure $ Message2 messageId (pack workload)
    "Message3" ->
      case readMaybe @Double workload of
        Just workloadDouble -> pure $ Message3 messageId workloadDouble
        Nothing -> parseFail ("FromJSON Message: Could not parse 'workload' field: " ++ workload ++ "\n  + " ++ show obj)
    _ ->
      parseFail "FromJSON Message: 'kind' not one of Message1, Message2 or Message3."

stripBrackets :: Text -> Maybe Text
stripBrackets t = do
  (ch1, rest)  <- Text.uncons t
  (rest', chn) <- Text.unsnoc rest
  guard $ ch1 == '<' && chn == '>'
  pure rest'

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

-- | Ordered by time.
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
