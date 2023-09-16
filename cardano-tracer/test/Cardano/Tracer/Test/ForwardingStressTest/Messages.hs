module Cardano.Tracer.Test.ForwardingStressTest.Messages (
    getMessageID
  , setMessageID
  ) where


import           Cardano.Logging
import           Cardano.Tracer.Test.ForwardingStressTest.Types

getMessageID :: Message -> MessageID
getMessageID (Message1 mid _) = mid
getMessageID (Message2 mid _) = mid
getMessageID (Message3 mid _) = mid

setMessageID :: Message -> MessageID -> Message
setMessageID (Message1 _ v) mid = Message1 mid v
setMessageID (Message2 _ v) mid = Message2 mid v
setMessageID (Message3 _ v) mid = Message3 mid v

instance MetaTrace Message where
  namespaceFor  Message1 {} = Namespace [] ["Message1"]
  namespaceFor  Message2 {} = Namespace [] ["Message2"]
  namespaceFor  Message3 {} = Namespace [] ["Message3"]

  severityFor   (Namespace _ ["Message1"]) _ = Just Debug
  severityFor   (Namespace _ ["Message2"]) _ = Just Info
  severityFor   (Namespace _ ["Message3"]) _ = Just Error
  severityFor   _ns _ = Nothing

  privacyFor    (Namespace _ ["Message1"]) _ = Just Public
  privacyFor    (Namespace _ ["Message2"]) _ = Just Public
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
