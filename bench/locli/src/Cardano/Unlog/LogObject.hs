{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-orphans #-}

module Cardano.Unlog.LogObject (module Cardano.Unlog.LogObject) where

import Prelude (head, id, show, unzip3)
import Cardano.Prelude hiding (Text, head, show)

import Control.Monad (fail)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object, (.:), (.:?))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Aeson.Key qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as LText
import Data.Text.Short qualified as Text
import Data.Text.Short (ShortText, fromText, toText)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Map qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V

import Cardano.Logging.Resources.Types

import Cardano.Analysis.Ground
import Cardano.Util

import Data.Accum (zeroUTCTime)


type Text = ShortText

runLiftLogObjects :: [JsonLogfile] -> Maybe HostDeduction
                  -> ExceptT LText.Text IO [(JsonLogfile, [LogObject])]
runLiftLogObjects fs (fmap hostDeduction -> mHostDed) = liftIO $
  forConcurrently fs
    (\f -> (f,) . fmap (setLOhost f mHostDed) <$> readLogObjectStream (unJsonLogfile f))
 where
   setLOhost :: JsonLogfile -> Maybe (JsonLogfile -> Host) -> LogObject -> LogObject
   setLOhost _   Nothing lo = lo
   setLOhost lf (Just f) lo = lo { loHost = f lf }

   -- joinT :: (IO a, IO b) -> IO (a, b)
   -- joinT (a, b) = (,) <$> a <*> b

readLogObjectStream :: FilePath -> IO [LogObject]
readLogObjectStream f =
  LBS.readFile f
    <&>
    fmap (either (LogObject zeroUTCTime "Cardano.Analysis.DecodeError" "DecodeError" "" (TId "0") . LODecodeError)
                 id
          . AE.eitherDecode)
    . LBS.split (fromIntegral $ fromEnum '\n')

data LogObject
  = LogObject
    { loAt   :: !UTCTime
    , loNS   :: !Text
    , loKind :: !Text
    , loHost :: !Host
    , loTid  :: !TId
    , loBody :: !LOBody
    }
  deriving (Generic, Show)
  deriving anyclass NFData

instance ToJSON LogObject

instance Print ShortText where
  hPutStr   h = hPutStr   h . toText
  hPutStrLn h = hPutStrLn h . toText

deriving instance NFData a => NFData (Resources a)

--
-- LogObject stream interpretation
--
type Threeple t = (t, t, t)

interpreters :: Threeple (Map Text (Object -> Parser LOBody))
interpreters = map3ple Map.fromList . unzip3 . fmap ent $
  -- Every second:
  [ (,,,) "Resources" "Resources" "" $
    \v -> LOResources <$> parsePartialResourceStates (Object v)

  -- Leadership:
  , (,,,) "TraceStartLeadershipCheck" "Forge.StartLeadershipCheck" "Forge.Loop.StartLeadershipCheck" $
    \v -> LOTraceStartLeadershipCheck
            <$> v .: "slot"
            <*> (v .:? "utxoSize"     <&> fromMaybe 0)
            <*> (v .:? "chainDensity" <&> fromMaybe 0)

  , (,,,) "TraceBlockContext" "Forge.BlockContext" "Forge.Loop.BlockContext" $
    \v -> LOBlockContext
            <$> v .: "current slot"
            <*> v .: "tipBlockNo"

  , (,,,) "TraceLedgerState" "Forge.LedgerState" "Forge.Loop.LedgerState" $
    \v -> LOLedgerState
            <$> v .: "slot"

  , (,,,) "TraceLedgerView" "Forge.LedgerView" "Forge.Loop.LedgerView" $
    \v -> LOLedgerView
            <$> v .: "slot"

  , (,,,) "TraceNodeIsLeader" "Forge.NodeIsLeader" "Forge.Loop.NodeIsLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure True

  , (,,,) "TraceNodeNotLeader" "Forge.NodeNotLeader" "Forge.Loop.NodeNotLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure False

  -- Forging:
  , (,,,) "TraceForgedBlock" "Forge.ForgedBlock" "Forge.Loop.ForgedBlock" $
    \v -> LOBlockForged
            <$> v .: "slot"
            <*> v .: "blockNo"
            <*> v .: "block"
            <*> v .: "blockPrev"

  -- Receipt:
  , (,,,) "ChainSyncClientEvent.TraceDownloadedHeader" "ChainSyncClient.ChainSyncClientEvent.DownloadedHeader" "ChainSync.Client.DownloadedHeader" $
    \v -> LOChainSyncClientSeenHeader
            <$> v .: "slot"
            <*> v .: "blockNo"
            <*> v .: "block"

  , (,,,) "SendFetchRequest" "BlockFetchClient.SendFetchRequest" "BlockFetch.Client.SendFetchRequest" $
    \v -> LOBlockFetchClientRequested
            <$> v .: "head"
            <*> v .: "length"

  , (,,,) "CompletedBlockFetch" "BlockFetchClient.CompletedBlockFetch" "BlockFetch.Client.CompletedBlockFetch" $
    \v -> LOBlockFetchClientCompletedFetch
            <$> v .: "block"

  -- Forwarding:
  , (,,,) "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock" "ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.AddBlock" "" $
    \v -> LOChainSyncServerSendHeader
            <$> v .: "block"

  , (,,,) "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock" "ChainSyncServerHeader.ChainSyncServerEvent.ServerReadBlocked.AddBlock" "ChainSync.ServerHeader.Update" $
    \v -> case ( KeyMap.lookup "risingEdge" v
               , KeyMap.lookup "blockingRead" v
               , KeyMap.lookup "rollBackTo" v) of
            -- Skip the falling edge & non-blocking reads:
            (Just (Bool False), _, _) -> pure $ LOAny v
            (_, Just (Bool False), _) -> pure $ LOAny v
            (_, _, Just _)            -> pure $ LOAny v
            -- Should be either rising edge+rollforward, or legacy:
            _ -> do
              blockLegacy <- v .:? "block"
              block       <- v .:? "addBlock"
              pure $
                LOChainSyncServerSendHeader
                ((block <|> blockLegacy)
                  & fromMaybe (error $ "Incompatible LOChainSyncServerSendHeader: " <> show v)
                  & Text.take 64
                  & Hash)

  , (,,,) "TraceBlockFetchServerSendBlock" "BlockFetchServer.SendBlock" "BlockFetch.Server.SendBlock" $
    \v -> LOBlockFetchServerSending
            <$> v .: "block"

  -- Adoption:
  , (,,,) "TraceAddBlockEvent.AddedToCurrentChain" "ChainDB.AddBlockEvent.AddedToCurrentChain" "ChainDB.AddBlockEvent.AddedToCurrentChain" $
    \v -> LOBlockAddedToCurrentChain
            <$> ((v .: "newtip")     <&> hashFromPoint)
            <*> pure Nothing
            <*> (v .:? "chainLengthDelta"
                -- Compat for node versions 1.27 and older:
                 <&> fromMaybe 1)
  -- TODO: we should clarify the distinction between the two cases (^ and v).
  , (,,,) "TraceAdoptedBlock" "Forge.AdoptedBlock" "Forge.Loop.AdoptedBlock" $
    \v -> LOBlockAddedToCurrentChain
            <$> v .: "blockHash"
            <*> ((v .: "blockSize") <&> Just)
            <*> pure 1

  -- Ledger snapshots:
  , (,,,) "TraceLedgerEvent.TookSnapshot" "LedgerEvent.TookSnapshot" "ChainDB.LedgerEvent.TookSnapshot" $
    \_ -> pure LOLedgerTookSnapshot

  -- Tx receive path & mempool:
  , (,,,) "TraceBenchTxSubServAck" "TraceBenchTxSubServAck" "TraceBenchTxSubServAck" $
    \v -> LOTxsAcked <$> v .: "txIds"

  , (,,,) "TraceTxSubmissionCollected" "TraceTxSubmissionCollected" "TraceTxSubmissionCollected" $
    \v -> LOTxsCollected
            <$> v .: "count"

  , (,,,) "TraceTxSubmissionProcessed" "TraceTxSubmissionProcessed" "TraceTxSubmissionProcessed" $
    \v -> LOTxsProcessed
            <$> v .: "accepted"
            <*> v .: "rejected"

  , (,,,) "TraceMempoolAddedTx" "Mempool.AddedTx" "Mempool.AddedTx" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,,,) "TraceMempoolRemoveTxs" "Mempool.RemoveTxs" "Mempool.RemoveTxs" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,,,) "TraceMempoolRejectedTx" "Mempool.RejectedTx" "Mempool.RejectedTx" $
    \_ -> pure LOMempoolRejectedTx

  -- Generator:
  , (,,,) "TraceBenchTxSubSummary" "TraceBenchTxSubSummary" "TraceBenchTxSubSummary" $
    \v -> do
       x :: Object <- v .: "summary"
       LOGeneratorSummary
         <$> ((x .: "ssFailures" :: Parser [Text])
              <&> null)
         <*> x .: "ssTxSent"
         <*> x .: "ssElapsed"
         <*> x .: "ssThreadwiseTps"
  ]
 where
   hashFromPoint :: LText.Text -> Hash
   hashFromPoint = Hash . fromText . Prelude.head . LText.splitOn "@"

   ent :: (a,b,c,d) -> ((a,d), (b,d), (c, d))
   ent (a,b,c,d) = ((a,d), (b,d), (c, d))

   map3ple :: (a -> b) -> (a,a,a) -> (b,b,b)
   map3ple f (x,y,z) = (f x, f y, f z)

logObjectStreamInterpreterKeysLegacy, logObjectStreamInterpreterKeysOldOrg, logObjectStreamInterpreterKeys :: [Text]
logObjectStreamInterpreterKeysLegacy = Map.keys (interpreters & fst3)
logObjectStreamInterpreterKeysOldOrg = Map.keys (interpreters & snd3)
logObjectStreamInterpreterKeys       = Map.keys (interpreters & thd3)

data LOBody
  -- Every second:
  = LOResources !ResourceStats
  -- Leadership:
  | LOTraceStartLeadershipCheck !SlotNo !Word64 !Double
  | LOBlockContext
    { loSlotNo           :: !SlotNo
    , loBlockNo          :: !BlockNo
    }
  | LOLedgerState
    { loSlotNo           :: !SlotNo
    }
  | LOLedgerView
    { loSlotNo           :: !SlotNo
    }
  | LOTraceLeadershipDecided
    { loSlotNo           :: !SlotNo
    , loLeader           :: !Bool
    }
  -- Forging:
  | LOBlockForged
    { loSlotNo           :: !SlotNo
    , loBlockNo          :: !BlockNo
    , loBlock            :: !Hash
    , loPrev             :: !Hash
    }
  -- Receipt:
  | LOChainSyncClientSeenHeader
    { loSlotNo           :: !SlotNo
    , loBlockNo          :: !BlockNo
    , loBlock            :: !Hash
    }
  | LOBlockFetchClientRequested
    { loBlock            :: !Hash
    , loLength           :: !Int
    }
  | LOBlockFetchClientCompletedFetch
    { loBlock            :: !Hash
    }
  -- Forwarding:
  | LOChainSyncServerSendHeader
    { loBlock            :: !Hash
    }
  | LOBlockFetchServerSending
    { loBlock            :: !Hash
    }
  -- Adoption:
  | LOBlockAddedToCurrentChain
    { loBlock            :: !Hash
    , loSize             :: !(Maybe Int)
    , loLength           :: !Int
    }
  -- Ledger snapshots:
  | LOLedgerTookSnapshot
  -- Tx receive path & mempool:
  | LOTxsAcked !(Vector Text)
  | LOTxsCollected !Word64
  | LOTxsProcessed !Word64 !Int
  | LOMempoolTxs !Word64
  | LOMempoolRejectedTx
  -- Generator:
  | LOGeneratorSummary !Bool !Word64 !NominalDiffTime (Vector Double)
  -- Everything else:
  | LOAny !Object
  | LODecodeError !String
  deriving (Generic, Show)
  deriving anyclass NFData

instance ToJSON LOBody

instance FromJSON LogObject where
  parseJSON = AE.withObject "LogObject" $ \v -> do
    body :: Object <- v .: "data"
    -- XXX:  fix node causing the need for this workaround
    (,) unwrapped kind <- unwrap "credentials" "val" body
    nsVorNs :: Value <- v .: "ns"
    let ns = case nsVorNs of
               Array (V.toList -> [String ns']) -> fromText ns'
               String ns' -> fromText ns'
               x -> error $
                 "The 'ns' field must be either a string, or a singleton-String vector, was: " <> show x
    LogObject
      <$> v .: "at"
      <*> pure ns
      <*> pure kind
      <*> v .: "host"
      <*> v .: "thread"
      <*> case Map.lookup  ns                                       (thd3 interpreters) <|>
               Map.lookup  ns                                       (snd3 interpreters) <|>
               Map.lookup (ns
                           & Text.stripPrefix "Cardano.Node."
                           & fromMaybe "")                          (snd3 interpreters) <|>
               Map.lookup  kind                                     (fst3 interpreters) of
            Just interp -> interp unwrapped
            Nothing -> pure $ LOAny unwrapped
   where
     unwrap :: Text -> Text -> Object -> Parser (Object, Text)
     unwrap wrappedKeyPred unwrapKey v = do
       kind <- (fromText <$>) <$> v .:? "kind"
       wrapped   :: Maybe Text <-
         (fromText <$>) <$> v .:? Aeson.fromText (toText wrappedKeyPred)
       unwrapped :: Maybe Object <- v .:? Aeson.fromText (toText unwrapKey)
       case (kind, wrapped, unwrapped) of
         (Nothing, Just _, Just x) -> (,) <$> pure x <*> (fromText <$> x .: "kind")
         (Just kind0, _, _) -> pure (v, kind0)
         _ -> fail $ "Unexpected LogObject .data: " <> show v

extendObject :: Text -> Value -> Value -> Value
extendObject k v (Object hm) = Object $ hm <> KeyMap.singleton (Aeson.fromText $ toText k) v
extendObject k _ _ = error . Text.unpack $ "Summary key '" <> k <> "' does not serialise to an Object."

parsePartialResourceStates :: Value -> Parser (Resources Word64)
parsePartialResourceStates =
  AE.withObject "NodeSetup" $
    \o ->
      Resources
      <$> o .: "CentiCpu"
      <*> o .: "CentiGC"
      <*> o .: "CentiMut"
      <*> o .: "GcsMajor"
      <*> o .: "GcsMinor"
      <*> o .: "Alloc"
      <*> o .: "Live"
      <*> (o .:? "Heap" <&> fromMaybe 0)
      <*> o .: "RSS"
      <*> o .: "CentiBlkIO"
      <*> o .: "Threads"
