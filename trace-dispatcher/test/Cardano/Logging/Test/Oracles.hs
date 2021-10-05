{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Oracles (
    oracleMessages
  , occurences
  ) where

import qualified Data.Text as T
import           Test.QuickCheck
import           Text.Read (readMaybe)

import           Cardano.Logging
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Types

import Debug.Trace


-- | Checks for every message that it appears or does not appear at the right
-- backend. Tests filtering and routing to backends
oracleMessages ::  TraceConfig -> ScriptRes -> Property
oracleMessages conf ScriptRes {..} =
    let Script msgs = srScript
    in property $ all oracleMessage msgs
  where
    oracleMessage :: ScriptedMessage -> Bool
    oracleMessage (ScriptedMessage _t msg) =
      let filterSeverity = getSeverity conf ("Node" : "Test" : namesForMessage msg)
          backends = getBackends conf ("Node" : "Test" : namesForMessage msg)
          inStdout = hasStdoutBackend backends
                      && fromEnum (severityForMessage msg) >= fromEnum filterSeverity
          isCorrectStdout = includedExactlyOnce msg srStdoutRes == inStdout
          inForwarder = elem Forwarder backends
                      && fromEnum (severityForMessage msg) >= fromEnum filterSeverity
                      && privacyForMessage msg == Public
          isCorrectForwarder = includedExactlyOnce msg srForwardRes == inForwarder
          inEKG = elem EKGBackend backends
                      && fromEnum (severityForMessage msg) >= fromEnum filterSeverity
                      && not (null (asMetrics msg))
          isCorrectEKG = includedExactlyOnce msg srEkgRes == inEKG
          res = isCorrectStdout && isCorrectForwarder && isCorrectEKG
      in case traceMessage isCorrectStdout isCorrectForwarder isCorrectEKG msg of
        Nothing -> res
        Just str -> trace str res
    traceMessage :: Bool -> Bool -> Bool -> Message -> Maybe String
    traceMessage isCorrectStdout isCorrectForwarder isCorrectEKG msg
      | not isCorrectStdout
      = Just
          ("stdoutTracer wrong filtering or routing for "
             <> show msg <> " config " <> show conf)
      | not isCorrectForwarder
      = Just
          ("forwardTracer wrong filtering or routing for "
             <> show msg <> " config " <> show conf)
      | not isCorrectEKG
      = Just
          ("ekgTracer wrong filtering or routing for "
             <> show msg <> " config " <> show conf)
      | otherwise = Nothing


-- | Is the stdout backend included in this configuration
hasStdoutBackend :: [BackendConfig] -> Bool
hasStdoutBackend []             = False
hasStdoutBackend (Stdout _ : _) = True
hasStdoutBackend (_ : rest)     = hasStdoutBackend rest

-- | Is this message in some form included in the formatted messages exactly once
includedExactlyOnce :: Message -> [FormattedMessage] -> Bool
includedExactlyOnce msg list =
    let msgID = getMessageID msg
    in case occurences msgID list of
          1 -> True
          0 -> False
          _ -> error $ "Multiple occurences of message " <> show msgID

-- | How often does the message with this id appears in the list of
-- formatted messsages?
occurences :: MessageID -> [FormattedMessage] -> Int
occurences _mid [] = 0
occurences  mid (fmsg : rest) = if isMessageWithId mid fmsg
                                  then 1 + occurences mid rest
                                  else occurences mid rest

-- | Returns true if the given message has this id, otherwise fals
isMessageWithId :: MessageID -> FormattedMessage -> Bool
isMessageWithId mid (FormattedMetrics [IntM _ idm])
                                        = fromIntegral idm == mid
isMessageWithId _   (FormattedMetrics [])   = False
isMessageWithId mid (FormattedHuman _ txt)  = idInText mid txt
isMessageWithId mid (FormattedMachine txt)  = idInText mid txt
isMessageWithId mid (FormattedForwarder to) =
  case toHuman to of
    Just txt -> idInText mid txt
    Nothing  -> case toMachine to of
                  Just txt -> idInText mid txt
                  Nothing  -> error "No text found in trace object"

-- | Is this message id part of the text?
idInText :: MessageID -> T.Text -> Bool
idInText mid txt =
  case extractId txt of
    Nothing -> False
    Just i  -> i == mid

-- | Extract a messageID from a text. It is always fumnd in the form '<?..>'
extractId :: T.Text -> Maybe Int
extractId txt =
  let ntxt = T.takeWhile (/= '>')
                (T.drop 1
                  (T.dropWhile (/= '<') txt))
  in readMaybe (T.unpack ntxt)
