module Cardano.Logging.Test.Messages (
    namesForMessage
  , severityForMessage
  , privacyForMessage
  , docMessage
  , getMessageID
  , setMessageID
  ) where

import           Data.Text

import           Cardano.Logging
import           Cardano.Logging.Test.Types

getMessageID :: Message -> MessageID
getMessageID (Message1 mid _) = mid
getMessageID (Message2 mid _) = mid
getMessageID (Message3 mid _) = mid

setMessageID :: Message -> MessageID -> Message
setMessageID (Message1 _ v) mid = Message1 mid v
setMessageID (Message2 _ v) mid = Message2 mid v
setMessageID (Message3 _ v) mid = Message3 mid v

namesForMessage :: Message -> [Text]
namesForMessage Message1 {} = ["Message1"]
namesForMessage Message2 {} = ["Message2"]
namesForMessage Message3 {} = ["Message3"]

severityForMessage :: Message -> SeverityS
severityForMessage Message1 {} = Debug
severityForMessage Message2 {} = Info
severityForMessage Message3 {} = Error

privacyForMessage :: Message -> Privacy
privacyForMessage Message1 {} = Public
privacyForMessage Message2 {} = Confidential
privacyForMessage Message3 {} = Public

docMessage :: Documented Message
docMessage = Documented [
    DocMsg
      (Message1 1 1)
      []
      "The first message."
  , DocMsg
      (Message2 1 "")
      []
      "The second message."
  , DocMsg
      (Message3 1 1.0)
      [("Metrics1", "A number")]
      "The third message."
  ]
