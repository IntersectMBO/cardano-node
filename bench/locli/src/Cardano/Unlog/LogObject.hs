{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-partial-fields -Wno-orphans #-}

{- HLINT ignore "Redundant <$>" -}
{- HLINT ignore "Move filter" -}

module Cardano.Unlog.LogObject
  ( HostLogs (..)
  , TraceFreqs
  , hlRawLogObjects
  , hlTraceFreqs
  , RunLogs (..)
  , rlLogs
  , LogObject (..)
  , loPretty
  --
  , logObjectStreamInterpreterKeys
  , LOBody (..)
  , LOAnyType (..)
  , fromTextRef
  , textRefEquals
  )
where

import           Cardano.Analysis.API.Ground
import           Cardano.Logging.Resources.Types
import           Cardano.Prelude hiding (Text, show, toText)
import           Cardano.Util

import           Prelude (show)

import qualified Data.Aeson as AE
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types (Parser)
import           Data.Data (Data)
import           Data.Hashable (hash)
import qualified Data.Map.Lazy as ML (Map)
import qualified Data.Map.Strict as Map
import           Data.Profile
import           Data.String (IsString (..))
import qualified Data.Text as LText
import           Data.Text.Short (ShortText, fromText)
import qualified Data.Text.Short as Text
import           Data.Vector (Vector)


type Text       = ShortText

type TraceFreqs = ML.Map Text Int


-- | Us of the a TextRef replaces commonly expected string parses with references
--   into a Map, reducing memory footprint - given that large runs can contain
--   >25mio log objects.
data TextRef
    = TextRef {-# UNPACK #-} !Int
    | TextLit {-# UNPACK #-} !Text
  deriving Generic
  deriving anyclass NFData

toTextRef :: Text -> TextRef
toTextRef t = let h = hash t in if Text.null (lookupTextRef h) then TextLit t else TextRef h

fromTextRef :: TextRef -> Text
fromTextRef (TextRef i) = lookupTextRef i
fromTextRef (TextLit t) = t

textRefEquals :: Text -> TextRef -> Bool
textRefEquals t = (t ==) . fromTextRef

instance Show TextRef where
  show (TextRef i) = show $ lookupTextRef i
  show (TextLit t) = show t

instance IsString TextRef where
  fromString = toTextRef . fromString

instance ToJSON TextRef where
  toJSON (TextRef i) = toJSON $ lookupTextRef i
  toJSON (TextLit t) = toJSON t

-- | Input data.
data HostLogs a
  = HostLogs
    { hlRawLogfiles    :: [FilePath]
    , hlRawLines       :: Int
    , hlRawTraceFreqs  :: TraceFreqs
    , hlLogs           :: (LogObjectSource, a)
    , hlProfile        :: [ProfileEntry I]
    , hlRawFirstAt     :: Maybe UTCTime
    , hlRawLastAt      :: Maybe UTCTime
    }
  deriving (Generic, Functor, NFData)

deriving instance FromJSON a => FromJSON (HostLogs a)
deriving instance   ToJSON a =>   ToJSON (HostLogs a)

hlRawLogObjects :: HostLogs a -> Int
hlRawLogObjects = sum . Map.elems . hlRawTraceFreqs

hlTraceFreqs :: HostLogs a -> HostLogs TraceFreqs
hlTraceFreqs HostLogs{hlLogs = (source, _), ..} =
  HostLogs {hlLogs = (source, hlRawTraceFreqs), ..}

data RunLogs a
  = RunLogs
    { rlHostLogs      :: Map.Map Host (HostLogs a)
    , rlFilterDate    :: UTCTime
    }
  deriving (Generic, Functor, FromJSON, ToJSON, NFData)

rlLogs :: RunLogs a -> [(LogObjectSource, a)]
rlLogs = fmap hlLogs . Map.elems . rlHostLogs

data LogObject
  = LogObject
    { loAt   :: !UTCTime
    , loNS   :: !TextRef
    , loHost :: !Host
    , loTid  :: !TId
    , loBody :: !LOBody
    }
  deriving (Generic, Show)
  deriving anyclass NFData

instance ToJSON LogObject

deriving instance NFData a => NFData (Resources a)


loPretty :: LogObject -> LText.Text
loPretty LogObject{..} = mconcat
  [ stripS . LText.pack $ show loAt, " "
  , LText.pack $ show loBody ]
 where stripS x = fromMaybe x $ LText.stripSuffix " UTC" x

--
-- Compat wrappers:
--
--- Needed becayse Ouroboros.Network.Block.BlockNo(..) imports a newtype instance,
---  ..whereas node's logs might contain an { unBlockNo :: BlockNo } object.
newtype BlockNoCompat =
  BlockNoCompat { unBlockNo :: BlockNo }
  deriving stock Generic
  deriving anyclass FromJSON

--
-- LogObject stream interpretation
--
interpreters :: Map Text (Object -> Parser LOBody)
interpreters = Map.fromList
  -- Every second:
  [ (,) "Resources" $
    \v -> LOResources <$> parsePartialResourceStates (Object v)

  -- Leadership:
  , (,) "Forge.Loop.StartLeadershipCheck" $
    \v -> LOTraceStartLeadershipCheck
            <$> v .: "slot"
            <*> (v .:? "utxoSize"     <&> fromMaybe 0)
            <*> (v .:? "chainDensity" <&> fromMaybe 0)

  , (,) "Forge.Loop.BlockContext" $
    \v -> LOBlockContext
            <$> v .: "current slot"
            <*> ((v .: "tipBlockNo")
                 -- BlockContext's block number is inconsistent
                 -- with the rest of traces.
                 <&> BlockNo . fromIntegral . pred @Int)

  , (,) "Forge.Loop.LedgerState" $
    \v -> LOLedgerState
            <$> v .: "slot"

  , (,) "Forge.Loop.LedgerView" $
    \v -> LOLedgerView
            <$> v .: "slot"

  , (,) "Forge.Loop.NodeIsLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure True

  , (,) "Forge.Loop.NodeNotLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure False

  , (,) "Forge.Loop.TickedLedgerState" $
    \v -> LOTickedLedgerState
            <$> v .: "slot"

  , (,) "Forge.Loop.MempoolSnapshot" $
    \v -> LOMempoolSnapshot
            <$> v .: "slot"

  -- Forging:
  , (,) "Forge.Loop.ForgedBlock" $
    \v -> LOBlockForged
            <$> v .: "slot"
            <*> v .: "blockNo"
            <*> v .: "block"
            <*> v .: "blockPrev"

  -- Receipt:
  , (,) "ChainSync.Client.DownloadedHeader" $
    \v -> LOChainSyncClientSeenHeader
            <$> v .: "slot"
            <*> ((v .: "blockNo")
                 <|>
                 ((v .: "blockNo") <&>
                  \(BlockNoCompat x) -> x))
            <*> v .: "block"

  , (,) "BlockFetch.Client.SendFetchRequest" $
    \v -> LOBlockFetchClientRequested
            <$> v .: "head"
            <*> v .: "length"

  , (,) "BlockFetch.Client.CompletedBlockFetch" $
    \v -> LOBlockFetchClientCompletedFetch
            <$> v .: "block"

  -- Forwarding:
  , (,) "ChainSync.ServerHeader.Update" $
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

  , (,) "BlockFetch.Server.SendBlock" $
    \v -> LOBlockFetchServerSending
            <$> v .: "block"

  -- Adoption:
  , (,) "ChainDB.AddBlockEvent.AddedToCurrentChain" $
    \v -> LOBlockAddedToCurrentChain
            <$> ((v .: "newtip")     <&> hashFromPoint)
            <*> pure SNothing
            <*> (v .:? "chainLengthDelta"
                -- Compat for node versions 1.27 and older:
                 <&> fromMaybe 1)

  , (,) "Forge.Loop.AdoptedBlock" $
    \v -> LOBlockAddedToCurrentChain
            <$> v .: "blockHash"
            <*> ((v .: "blockSize") <&> SJust)
            <*> pure 1

  -- Ledger related:
  , (,) "ChainDB.LedgerEvent.Snapshot.TookSnapshot" $
    \_ -> pure LOLedgerTookSnapshot
  -- If needed, this could track slot and duration (SMaybe):
  -- {"at":"2026-09-10T12:28:28.864828281Z","ns":"ChainDB.LedgerEvent.Snapshot.TookSnapshot","data":{"enclosedTime":{"tag":"RisingEdge"},"kind":"TookSnapshot","snapshot":{"kind":"snapshot"},"tip":"RealPoint (SlotNo 56096) a071837adb7010366e7ff8ed344b2f9232dbbfe077a1e040a621431ff4de27ce"},"sev":"Info","thread":"102","host":"node-1"}
  -- {"at":"2026-09-10T12:28:48.984750482Z","ns":"ChainDB.LedgerEvent.Snapshot.TookSnapshot","data":{"enclosedTime":{"contents":20.119900237,"tag":"FallingEdgeWith"},"kind":"TookSnapshot","snapshot":{"kind":"snapshot"},"tip":"RealPoint (SlotNo 56096) a071837adb7010366e7ff8ed344b2f9232dbbfe077a1e040a621431ff4de27ce"},"sev":"Info","thread":"102","host":"node-1"}

  , (,) "LedgerMetrics" $
    \v -> LOLedgerMetrics
            <$> v .: "slot"
            <*> v .: "utxoSize"
            <*> v .: "chainDensity"

  -- Tx receive path & mempool:
  , (,) "TxSubmission.TxInbound.Collected" $
    \v -> LOTxsCollected
            <$> v .: "count"

  , (,) "TxSubmission.TxInbound.Processed" $
    \v -> LOTxsProcessed
            <$> v .: "accepted"
            <*> v .: "rejected"

  , (,) "Mempool.AddedTx" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,) "Mempool.RemoveTxs" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,) "Mempool.RejectedTx" $
    \_ -> pure LOMempoolRejectedTx

  -- Generator:
  , (,) "Benchmark.BenchTxSubSummary" $
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
   hashFromPoint = Hash . fromText . LText.take 64



logObjectStreamInterpreterKeys :: [Text]
logObjectStreamInterpreterKeys = Map.keys interpreters

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
  -- Ledger related:
  | LOLedgerTookSnapshot
  | LOLedgerMetrics
    { loSlotNo           :: !SlotNo
    , loUtxoSize         :: !Word64
    , loChainDensity     :: !Double
    }
  -- Tx receive path & mempool:
  | LOTxsAcked !(Vector Text)     -- Note: There currently appears to be no specific trace carrying that information; this was based on "TraceBenchTxSubServAck"; left in as a placeholder for now.
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
  deriving (Eq, Generic, Show, Data)
  deriving anyclass NFData

data LOAnyType
  = LAFallingEdge
  | LANonBlocking
  | LARollback
  | LANoInterpreter
  deriving (Eq, Generic, NFData, Read, Show, ToJSON, Data)

deriving instance Eq       ResourceStats
deriving instance Data     ResourceStats

instance ToJSON LOBody

instance FromJSON LogObject where
  parseJSON = AE.withObject "LogObject" $ \v -> do
    body :: Object <- v .: "data"
    ns   :: Text   <- fromText <$> v .: "ns"
    LogObject
      <$> v .: "at"
      <*> pure (toTextRef ns)
      <*> v .: "host"
      <*> v .: "thread"
      <*> case Map.lookup ns interpreters of
            Just interp -> interp body
            Nothing -> pure $ LOAny LANoInterpreter v

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
      rLive       <- o .:  "Live"
      rHeap       <- o .:? "Heap"       .!= 0
      rRSS        <- o .:  "RSS"
      rCentiBlkIO <- o .:  "CentiBlkIO"
      rNetRd      <- o .:? "NetRd"      .!= 0
      rNetWr      <- o .:? "NetWr"      .!= 0
      rFsRd       <- o .:? "FsRd"       .!= 0
      rFsWr       <- o .:? "FsWr"       .!= 0
      rThreads    <- o .:  "Threads"
      pure Resources{..}

{-# NOINLINE lookupTextRef #-}
lookupTextRef :: Int -> Text
lookupTextRef ref = Map.findWithDefault Text.empty ref dict
  where
    dict    = Map.fromList [(hash t, t) | t <- concat [allKeys, newTr]]
    allKeys = Map.keys interpreters
              & filter (not . Text.null)

    -- Common string parses from new tracing with no known interpreter.
    -- When parsing, those are replaced by their hashes, as the parser would
    -- redundantly allocate >2.7mio Text values per node on the heap for a cluster run,
    -- blowing up RAM for analysis. To recreate:
    -- * Count absolute occurrences on a node's log output: awk 'NR >40' stdout | jq '.ns' | sort | uniq -c | sort -rn
    -- * Make sure there's no parser for the namespace defined in `interpreters` above
    -- * Add `.ns` Text values for high frequency traces verbatim to the list
    newTr =
      [ "AcknowledgedFetchRequest"
      , "AddedFetchRequest"
      , "BlockFetch.Client.AcknowledgedFetchRequest"
      , "BlockFetch.Client.AddedFetchRequest"
      , "BlockFetch.Client.CompletedFetchBatch"
      , "BlockFetch.Client.StartedFetchBatch"
      , "BlockFetch.Remote.Receive.BatchDone"
      , "BlockFetch.Remote.Receive.Block"
      , "BlockFetch.Remote.Receive.StartBatch"
      , "BlockFetchServer"
      , "ChainDB.AddBlockEvent.AddBlockValidation.UpdateLedgerDb"
      , "ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate"
      , "ChainDB.AddBlockEvent.AddedBlockToQueue"
      , "ChainDB.AddBlockEvent.AddedBlockToVolatileDB"
      , "ChainDB.AddBlockEvent.ChangingSelection"
      , "ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB"
      , "ChainDB.AddBlockEvent.PipeliningEvent.OutdatedTentativeHeader"
      , "ChainDB.AddBlockEvent.PipeliningEvent.SetTentativeHeader"
      , "ChainDB.AddBlockEvent.PoppedBlockFromQueue"
      , "ChainDB.AddBlockEvent.TryAddToCurrentChain"
      , "ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB"
      , "ChainDB.FollowerEvent.NewFollower"
      , "ChainDB.GCEvent.ScheduledGC"
      , "ChainDB.IteratorEvent.StreamFromVolatileDB"
      , "ChainSync.Client.JumpingInstructionIs"
      , "ChainSync.Client.JumpingWaitingForNextInstruction"
      , "ChainSyncServer.Update"
      , "CompletedFetchBatch"
      , "CopiedBlockToImmutableDB"
      , "DownloadedHeader"
      , "Forge.ForgingStats"
      , "Forge.StateInfo.StateInfo"
      , "ForgingStats"
      , "IgnoreBlockAlreadyInVolatileDB"
      , "Net.Handshake.Local.Receive.ProposeVersions"
      , "Net.Handshake.Local.Send.AcceptVersion"
      , "OutdatedTentativeHeader"
      , "Recv"
      , "ResourceStats"
      , "SetTentativeHeader"
      , "StartedFetchBatch"
      , "StateQueryServer.Receive.Query"
      , "StateQueryServer.Receive.Release"
      , "StreamFromVolatileDB"
      , "TraceAddBlockEvent.ChangingSelection"
      , "TraceAddBlockEvent.PoppedBlockFromQueue"
      , "TxSubmission.TxInbound.AddedToMempool"
      , "TxSubmission.TxInbound.CanRequestMoreTxs"
      , "TxSubmission.TxInbound.CannotRequestMoreTxs"
      , "TxSubmission.TxInbound.RejectedFromMempool"
      , "UpdateLedgerDbTraceEvent.StartedPushingBlockToTheLedgerDb"
      ]
