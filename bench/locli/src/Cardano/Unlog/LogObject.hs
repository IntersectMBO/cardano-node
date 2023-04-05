{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-partial-fields -Wno-orphans #-}

{- HLINT ignore "Redundant <$>" -}
{- HLINT ignore "Redundant if" -}
{- HLINT ignore "Use infix" -}

module Cardano.Unlog.LogObject (module Cardano.Unlog.LogObject) where

import           Cardano.Prelude hiding (Text, head, show)
import           Prelude (head, id, show, unzip3)

import           Control.Monad (fail)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as LText
import           Data.Text.Short (ShortText, fromText, toText)
import qualified Data.Text.Short as Text
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Cardano.Logging.Resources.Types

import           Data.Profile

import           Cardano.Analysis.API.Ground
import           Cardano.Util


type Text = ShortText

-- | Input data.
data HostLogs a
  = HostLogs
    { hlRawLogfiles    :: [FilePath]
    , hlRawLines       :: Int
    , hlRawSha256      :: Hash
    , hlRawTraceFreqs  :: Map Text Int
    , hlLogs           :: (JsonLogfile, a)
    , hlFilteredSha256 :: Hash
    , hlProfile        :: [ProfileEntry I]
    }
  deriving (Generic)

deriving instance FromJSON a => FromJSON (HostLogs a)
deriving instance   ToJSON a =>   ToJSON (HostLogs a)

hlRawLogObjects :: HostLogs a -> Int
hlRawLogObjects = sum . Map.elems . hlRawTraceFreqs

data RunLogs a
  = RunLogs
    { rlHostLogs   :: Map.Map Host (HostLogs a)
    , rlFilterKeys :: [Text]
    , rlFilterDate :: UTCTime
    }
  deriving (Generic, FromJSON, ToJSON)

rlLogs :: RunLogs a -> [(JsonLogfile, a)]
rlLogs = fmap hlLogs . Map.elems . rlHostLogs

runLiftLogObjects :: RunLogs () -> Bool -> [LOAnyType]
                  -> ExceptT LText.Text IO (RunLogs [LogObject])
runLiftLogObjects rl@RunLogs{..} okDErr anyOks = liftIO $ do
  forConcurrently (Map.toList rlHostLogs)
    (uncurry readHostLogs)
    <&> \kvs -> rl { rlHostLogs = Map.fromList kvs }
 where
   readHostLogs :: Host -> HostLogs () -> IO (Host, HostLogs [LogObject])
   readHostLogs h hl@HostLogs{..} =
     readLogObjectStream (unJsonLogfile $ fst hlLogs) okDErr anyOks
     <&> (h,) . setLogs hl . fmap (setLOhost h)

   setLogs :: HostLogs a -> b -> HostLogs b
   setLogs hl x = hl { hlLogs = (fst $ hlLogs hl, x) }
   setLOhost :: Host -> LogObject -> LogObject
   setLOhost h lo = lo { loHost = h }

readLogObjectStream :: FilePath -> Bool -> [LOAnyType] -> IO [LogObject]
readLogObjectStream f okDErr anyOks =
  LBS.readFile f
    <&>
    (if okDErr then id else
        filter ((\case
                    LODecodeError input err -> error
                      (printf "Decode error while parsing %s:\n%s\non input:\n>>>  %s" f (Text.toString err) (Text.toString input))
                    _ -> True)
               . loBody)) .
    filter ((\case
                LOAny laty obj ->
                  if elem laty anyOks then True else
                    error
                    (printf "Unexpected LOAny while parsing %s -- %s: %s" f (show laty) (show obj))
                _ -> True)
             . loBody) .
    filter (not . isDecodeError "Error in $: not enough input" . loBody) .
    fmap (\bs ->
            AE.eitherDecode bs &
            either
            (LogObject zeroUTCTime "Cardano.Analysis.DecodeError" "DecodeError" "" (TId "0")
             . LODecodeError (Text.fromByteString (LBS.toStrict bs)
                               & fromMaybe "#<ERROR decoding input fromByteString>")
              . Text.fromText
              . LText.pack)
            id)
    . LBS.split (fromIntegral $ fromEnum '\n')
 where
   isDecodeError x = \case
     LODecodeError _ x' -> x == x'
     _ -> False

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

loPretty :: LogObject -> LText.Text
loPretty LogObject{..} = mconcat
  [ stripS . LText.pack $ show loAt, " "
  , LText.pack $ show loBody ]
 where stripS x = fromMaybe x $ LText.stripSuffix " UTC" x

--
-- LogObject stream interpretation
--
type Threeple t = (t, t, t)

interpreters :: Threeple (Map Text (Object -> Parser LOBody))
interpreters = map3ple Map.fromList . unzip3 . fmap ent $
  -- Every second:
  [ (,,,) "Resources" "Resources" "Resources" $
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

  , (,,,) "TraceForgeTickedLedgerState" "Forge.TickedLedgerState" "Forge.Loop.TickedLedgerState" $
    \v -> LOTickedLedgerState
            <$> v .: "slot"

  , (,,,) "TraceForgingMempoolSnapshot" "Forge.MempoolSnapshot" "Forge.Loop.MempoolSnapshot" $
    \v -> LOMempoolSnapshot
            <$> v .: "slot"

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
  , (,,,) "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock" "unknown0" "unknown1" $
    \v -> LOChainSyncServerSendHeader . fromMaybe (error $ "Incompatible LOChainSyncServerSendHeader: " <> show v)
          <$>  v .:? "block"

  , (,,,) "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock" "ChainSyncServerEvent.TraceChainSyncServerUpdate" "ChainSync.ServerHeader.Update" $
    \v -> case ( KeyMap.lookup "risingEdge" v
               , KeyMap.lookup "blockingRead" v
               , KeyMap.lookup "rollBackTo" v) of
            (Just (Bool False), _, _) -> pure $ LOAny LAFallingEdge v
            (_, Just (Bool False), _) -> pure $ LOAny LANonBlocking v
            (_, _, Just _)            -> pure $ LOAny LARollback    v
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
            <*> pure SNothing
            <*> (v .:? "chainLengthDelta"
                -- Compat for node versions 1.27 and older:
                 <&> fromMaybe 1)
  -- TODO: we should clarify the distinction between the two cases (^ and v).
  , (,,,) "TraceAdoptedBlock" "Forge.AdoptedBlock" "Forge.Loop.AdoptedBlock" $
    \v -> LOBlockAddedToCurrentChain
            <$> v .: "blockHash"
            <*> ((v .: "blockSize") <&> SJust)
            <*> pure 1

  -- Ledger snapshots:
  , (,,,) "TraceSnapshotEvent.TookSnapshot" "LedgerEvent.TookSnapshot" "ChainDB.LedgerEvent.TookSnapshot" $
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

logObjectStreamInterpreterKeysLegacy, logObjectStreamInterpreterKeys :: [Text]
logObjectStreamInterpreterKeysLegacy =
  logObjectStreamInterpreterKeysLegacy1 <> logObjectStreamInterpreterKeysLegacy2
 where
   logObjectStreamInterpreterKeysLegacy1 = Map.keys (interpreters & fst3)
   logObjectStreamInterpreterKeysLegacy2 = Map.keys (interpreters & snd3)
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
  | LOTickedLedgerState
    { loSlotNo           :: !SlotNo
    }
  | LOMempoolSnapshot
    { loSlotNo           :: !SlotNo
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
    , loSize             :: !(SMaybe Int)
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
  | LOGeneratorSummary !Bool !Word64 !NominalDiffTime ![Double]
  -- Everything else:
  | LOAny !LOAnyType !Object
  | LODecodeError
    { loRawText :: !ShortText
    , loError   :: !ShortText
    }
  deriving (Eq, Generic, Show)
  deriving anyclass NFData

data LOAnyType
  = LAFallingEdge
  | LANonBlocking
  | LARollback
  | LANoInterpreter
  deriving (Eq, Generic, NFData, Read, Show, ToJSON)

deriving instance Eq ResourceStats

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
      <*> case Map.lookup  ns                                       (thd3 interpreters)
           <|> Map.lookup  ns                                       (snd3 interpreters)
           <|> Map.lookup (kind
                           & Text.stripPrefix "Cardano.Node."
                           & fromMaybe kind)                        (snd3 interpreters)
           <|> Map.lookup  kind                                     (fst3 interpreters) of
            Just interp -> interp unwrapped
            Nothing -> pure $ LOAny LANoInterpreter unwrapped
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
    \o -> do
      rCentiCpu   <- o .:  "CentiCpu"
      rCentiGC    <- o .:  "CentiGC"
      rCentiMut   <- o .:  "CentiMut"
      rGcsMajor   <- o .:  "GcsMajor"
      rGcsMinor   <- o .:  "GcsMinor"
      rAlloc      <- o .:  "Alloc"
      rLive       <- o .:? "Heap"       <&> fromMaybe 0
      rHeap       <- o .:  "Live"
      rRSS        <- o .:  "RSS"
      rCentiBlkIO <- o .:  "CentiBlkIO"
      rNetRd      <- o .:? "NetRd"      <&> fromMaybe 0
      rNetWr      <- o .:? "NetWr"      <&> fromMaybe 0
      rFsRd       <- o .:? "FsRd"       <&> fromMaybe 0
      rFsWr       <- o .:? "FsWr"       <&> fromMaybe 0
      rThreads    <- o .:  "Threads"
      pure Resources{..}
