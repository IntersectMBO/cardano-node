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

import Prelude (error, head, id, show)
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

type ACouple t = (t, t)

interpreters :: ACouple (Map Text (Object -> Parser LOBody))
interpreters = (Map.fromList *** Map.fromList) . unzip . fmap ent $
  [ (,,) "TraceStartLeadershipCheck" "Cardano.Node.Forge.StartLeadershipCheck" $
    \v -> LOTraceStartLeadershipCheck
            <$> v .: "slot"
            <*> (v .:? "utxoSize"     <&> fromMaybe 0)
            <*> (v .:? "chainDensity" <&> fromMaybe 0)

  , (,,) "TraceBlockContext" "Cardano.Node.Forge.BlockContext" $
    \v -> LOBlockContext
            <$> v .: "tipBlockNo"

  , (,,) "TraceNodeIsLeader" "Cardano.Node.Forge.NodeIsLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure True

  , (,,) "TraceNodeNotLeader" "Cardano.Node.Forge.NodeNotLeader" $
    \v -> LOTraceLeadershipDecided
            <$> v .: "slot"
            <*> pure False

  , (,,) "TraceMempoolAddedTx" "Cardano.Node.Mempool.AddedTx" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,,) "TraceMempoolRemoveTxs" "Cardano.Node.Mempool.RemoveTxs" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,,) "TraceMempoolRejectedTx" "Cardano.Node.Mempool.RejectedTx" $
    \_ -> pure LOMempoolRejectedTx

  , (,,) "TraceLedgerEvent.TookSnapshot" "Cardano.Node.LedgerEvent.TookSnapshot" $
    \_ -> pure LOLedgerTookSnapshot

  , (,,) "TraceBenchTxSubSummary" "TraceBenchTxSubSummary" $
    \v -> do
       x :: Object <- v .: "summary"
       LOGeneratorSummary
         <$> ((x .: "ssFailures" :: Parser [Text])
              <&> null)
         <*> x .: "ssTxSent"
         <*> x .: "ssElapsed"
         <*> x .: "ssThreadwiseTps"

  , (,,) "TraceBenchTxSubServAck" "TraceBenchTxSubServAck" $
    \v -> LOTxsAcked <$> v .: "txIds"

  , (,,) "Resources" "Cardano.Node.Resources" $
    \v -> LOResources <$> parsePartialResourceStates (Object v)

  , (,,) "TraceTxSubmissionCollected" "TraceTxSubmissionCollected" $
    \v -> LOTxsCollected
            <$> v .: "count"

  , (,,) "TraceTxSubmissionProcessed" "TraceTxSubmissionProcessed" $
    \v -> LOTxsProcessed
            <$> v .: "accepted"
            <*> v .: "rejected"

  , (,,) "TraceForgedBlock" "Cardano.Node.Forge.ForgedBlock" $
    \v -> LOBlockForged
            <$> v .: "block"
            <*> v .: "blockPrev"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,,) "TraceAddBlockEvent.AddedToCurrentChain" "Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain" $
    \v -> LOBlockAddedToCurrentChain
            <$> ((v .: "newtip")     <&> hashFromPoint)
            <*> pure Nothing
            <*> (v .:? "chainLengthDelta"
                -- Compat for node versions 1.27 and older:
                 <&> fromMaybe 1)
  -- TODO: we should clarify the distinction between the two cases (^ and v).
  , (,,) "TraceAdoptedBlock" "Cardano.Node.Forge.AdoptedBlock" $
    \v -> LOBlockAddedToCurrentChain
            <$> v .: "blockHash"
            <*> ((v .: "blockSize") <&> Just)
            <*> pure 1
  , (,,) "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock" "Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.AddBlock" $
    \v -> LOChainSyncServerSendHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,,) "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock" "Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerReadBlocked.AddBlock" $
    \v -> LOChainSyncServerSendHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  -- v, but not ^ -- how is that possible?
  , (,,) "TraceBlockFetchServerSendBlock" "Cardano.Node.BlockFetchServer.SendBlock" $
    \v -> LOBlockFetchServerSending
            <$> v .: "block"
  , (,,) "SendFetchRequest" "Cardano.Node.BlockFetchClient.SendFetchRequest" $
    \v -> LOBlockFetchClientRequested
            <$> v .: "head"
            <*> v .: "length"
  , (,,) "ChainSyncClientEvent.TraceDownloadedHeader" "Cardano.Node.ChainSyncClient.ChainSyncClientEvent.DownloadedHeader" $
    \v -> LOChainSyncClientSeenHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,,) "CompletedBlockFetch" "Cardano.Node.BlockFetchClient.CompletedBlockFetch" $
    \v -> LOBlockFetchClientCompletedFetch
            <$> v .: "block"
  ]
 where
   hashFromPoint :: LText.Text -> Hash
   hashFromPoint = Hash . fromText . Prelude.head . LText.splitOn "@"

   ent :: (a,b,c) -> ((a,c), (b,c))
   ent (a,b,c) = ((a,c), (b,c))

logObjectStreamInterpreterKeysLegacy, logObjectStreamInterpreterKeys :: [Text]
logObjectStreamInterpreterKeysLegacy = Map.keys (fst interpreters)
logObjectStreamInterpreterKeys       = Map.keys (snd interpreters)

data LOBody
  = LOTraceStartLeadershipCheck !SlotNo !Word64 !Double
  | LOTraceLeadershipDecided    !SlotNo !Bool
  | LOResources !ResourceStats
  | LOMempoolTxs !Word64
  | LOMempoolRejectedTx
  | LOLedgerTookSnapshot
  | LOBlockContext !Word64
  | LOGeneratorSummary !Bool !Word64 !NominalDiffTime (Vector Double)
  | LOTxsAcked !(Vector Text)
  | LOTxsCollected !Word64
  | LOTxsProcessed !Word64 !Int
  | LOBlockForged
    { loBlock            :: !Hash
    , loPrev             :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlotNo           :: !SlotNo
    }
  | LOBlockAddedToCurrentChain
    { loBlock            :: !Hash
    , loSize             :: !(Maybe Int)
    , loLength           :: !Int
    }
  | LOChainSyncServerSendHeader
    { loBlock            :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlotNo           :: !SlotNo
    }
  | LOBlockFetchServerSending
    { loBlock            :: !Hash
    }
  | LOBlockFetchClientRequested
    { loBlock            :: !Hash
    , loLength           :: !Int
    }
  | LOChainSyncClientSeenHeader
    { loBlock            :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlotNo           :: !SlotNo
    }
  | LOBlockFetchClientCompletedFetch
    { loBlock            :: !Hash
    }
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
      <*> case Map.lookup ns   (snd interpreters) <|>
               Map.lookup kind (fst interpreters) of
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
