{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-# OPTIONS_GHC -fno-warn-unused-imports  #-}


module Cardano.Unlog.LogObjectDB
       ( AsSQLData (..)
       , SummaryDB (..)
       , SQLRunnable
       , TraceFreqs

       , sqlToLogObject
       , logObjectToSql
       , errorToSql
       , summaryToSql
       , traceFreqsToSql

       , runSqlRunnable

       , allLOBodyConstructors
       , knownLOBodyConstructors
       ) where

import           Cardano.Analysis.API.Ground
import           Cardano.Logging.Resources.Types (ResourceStats, Resources (..))
import           Cardano.Unlog.LogObject
import           Cardano.Util hiding (count)

import           Prelude

import           Data.Aeson as Aeson (decodeStrict, encode)
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL (toStrict)
import           Data.Data (dataTypeConstrs, dataTypeOf, showConstr, toConstr)
import qualified Data.Map.Lazy as ML
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as TS (Text, empty, intercalate, pack, splitOn, unpack)
import qualified Data.Text.Lazy as TL (Text, fromStrict, pack)
import qualified Data.Text.Short as ShortText (ShortText, empty, fromText, pack, toText)

import           Database.Sqlite.Easy hiding (Text)
import           Database.Sqlite.Easy.Internal (SQL (..))


data SummaryDB = SummaryDB
  { sdbName     :: Host
  , sdbLines    :: Int
  , sdbFirstAt  :: UTCTime
  , sdbLastAt   :: UTCTime
  }

type TraceFreqs = ML.Map ShortText.ShortText Int

-- an SQL statement, and its arguments; directly applicable to `uncurry runWith`
type SQLRunnable = (SQL, [SQLData])

runSqlRunnable :: SQLRunnable -> SQLite [[SQLData]]
runSqlRunnable = uncurry runWith


-- table error

insertError :: SQL
insertError =
  "INSERT INTO error VALUES (?,?)"

errorToSql :: String -> String -> SQLRunnable
errorToSql errorMsg origInput =
  (insertError, toArgs $ Tuple ("", errorMsg) ("", origInput))

-- table summary

insertSummary :: SQL
insertSummary =
  "INSERT INTO summary VALUES (?,?,?,?)"

summaryToSql :: SummaryDB -> SQLRunnable
summaryToSql SummaryDB{sdbName = Host name, ..} =
  ( insertSummary
  , [ toSqlData name, toSqlData sdbLines, toSqlData sdbFirstAt, toSqlData sdbLastAt ]
  )

-- table tracefreq

insertTraceFreq :: SQL
insertTraceFreq =
  "INSERT INTO tracefreq VALUES (?,?)"

traceFreqsToSql :: TraceFreqs -> [SQLRunnable]
traceFreqsToSql ts =
  (,) insertTraceFreq <$>
    [ toArgs (Tuple ("", k) ("", v)) | (k, v) <- ML.toAscList ts ]

-- table resource

insertResource :: SQL
insertResource =
  "INSERT INTO resource VALUES (?,?,?,?,?,?)"

resourceArgs :: UTCTime -> ResourceStats -> [SQLData]
resourceArgs at rs@Resources{rCentiCpu, rRSS, rHeap, rAlloc} =
  [ toSqlData at
  , toSqlData rCentiCpu
  , toSqlData rRSS
  , toSqlData rHeap
  , toSqlData rAlloc
  , toSqlData rs
  ]

-- table slot

insertSlot :: SQL
insertSlot =
  "INSERT INTO slot VALUES (?,?,?,?)"

slotArgs :: UTCTime -> ArgNTuple -> [SQLData]
slotArgs at args@Triple{}   = toSqlData at : toArgs args
slotArgs _  _               = error "slotArgs: three arguments expected"

-- tables event and txns

logObjectToSql :: LogObject -> Maybe SQLRunnable
logObjectToSql lo@LogObject{loAt, loBody, loTid} =
    -- case Aeson.eitherDecode inp of

    -- Left err -> Just (insertError, toArgs $ Tuple ("", err) ("", BSL.unpack inp))

    -- Right  ->
    case loBody of

      -- no suitable interpreter found when parsing log object stream
      LOAny{}                       -> Nothing
      -- trace not emitted by the node
      LOGeneratorSummary{}          -> Nothing
      -- not required for analysis
      LOTxsAcked{}                  -> Nothing

      LOResources stats             -> Just (insertResource, resourceArgs loAt stats)

      LOTraceStartLeadershipCheck slot utxoSize chainDensity
                                    -> Just (insertSlot, slotArgs loAt (Triple ("", slot) ("", utxoSize) ("", chainDensity)))
      -- forging
      LOBlockContext slot block     -> newLOEvent $ Tuple     ("slot", slot) ("block", block)
      LOLedgerState s               -> newLOEvent $ Singleton ("slot", s)
      LOLedgerView s                -> newLOEvent $ Singleton ("slot", s)
      LOTraceLeadershipDecided s b  -> newLOEvent $ Tuple     ("slot", s) ("block", b)
      LOTickedLedgerState s         -> newLOEvent $ Singleton ("slot", s)
      LOMempoolSnapshot s           -> newLOEvent $ Singleton ("slot", s)
      LOBlockForged s b h1 h2       -> newLOEvent $ Triple    ("slot", s) ("block", b) ("hash", (h1, h2))

      -- diffusion
      LOChainSyncClientSeenHeader s b h
                                    -> newLOEvent $ Triple    ("slot", s) ("block", b) ("hash", h)
      LOBlockFetchClientRequested h len
                                    -> newLOEvent $ Tuple     ("block", len) ("hash", h)
      LOBlockFetchClientCompletedFetch h
                                    -> newLOEvent $ Singleton ("hash", h)
      LOChainSyncServerSendHeader h
                                    -> newLOEvent $ Singleton ("hash", h)
      LOBlockFetchServerSending h
                                    -> newLOEvent $ Singleton ("hash", h)
      LOBlockAddedToCurrentChain h mSz len
                                    -> newLOEvent $ Triple    ("slot", mSz) ("block", len) ("hash", h)

      LOLedgerTookSnapshot          -> newLOEvent Empty

      -- txn receive path
      LOTxsCollected c              -> newLOTxns $ Tuple      ("count", c) ("tid", loTid)
      LOTxsProcessed c r            -> newLOTxns $ Triple     ("count", c) ("rejected", r) ("tid", loTid)
      LOMempoolTxs c                -> newLOTxns $ Singleton  ("count", c)
      LOMempoolRejectedTx           -> newLOTxns Empty

      -- that goes to the error table
      LODecodeError rawText err     -> Just (insertError, toArgs $ Tuple ("", err) ("", rawText))

      where
        newLOEvent = Just . insertVariadic "event" lo
        newLOTxns  = Just . insertVariadic "txns"  lo


insertVariadic :: SQL -> LogObject -> ArgNTuple -> SQLRunnable
insertVariadic table LogObject{loAt, loBody} argNTuple = (sql, args)
  where
    args              = toSqlData loAt : toSqlData loBody : toArgs argNTuple
    (columns, templ)  = toFieldList argNTuple
    sql               = "INSERT INTO " <> table <>"(at,cons" <> columns <> ") VALUES (?,?" <> templ <> ")"


-- some minimal guarantees for the variadic INSERTs on tables event and txns

type Column = TS.Text

-- values to store, paired with their column name
data ArgNTuple where
  Empty     :: ArgNTuple
  Singleton :: forall x.     (AsSQLData x) => (Column, x) -> ArgNTuple
  Tuple     :: forall x y.   (AsSQLData x, AsSQLData y) => (Column, x) -> (Column, y) -> ArgNTuple
  Triple    :: forall x y z. (AsSQLData x, AsSQLData y, AsSQLData z) => (Column, x) -> (Column, y) -> (Column, z) -> ArgNTuple

toArgs :: ArgNTuple -> [SQLData]
toArgs = \case
  Empty                           -> []
  Singleton (_, x)                -> [toSqlData x]
  Tuple     (_, x) (_, y)         -> [toSqlData x, toSqlData y]
  Triple    (_, x) (_, y) (_, z)  -> [toSqlData x, toSqlData y, toSqlData z]

-- for simplicity's sake, this yields both the column names
-- and the correct number of additional placeholders to extend the template
toFieldList :: ArgNTuple -> (SQL, SQL)
toFieldList = \case
  Empty                           -> (""           , "")
  Singleton (x, _)                -> (go [x]       , ",?")
  Tuple     (x, _) (y, _)         -> (go [x, y]    , ",?,?")
  Triple    (x, _) (y, _) (z, _)  -> (go [x, y, z] , ",?,?,?")
  where
    go = SQL . TS.intercalate "," . (TS.empty :)


sqlToLogObject :: SummaryDB -> [SQLData] -> LogObject
sqlToLogObject _ [] = error "toLogObject: no columns in result row"
sqlToLogObject SummaryDB{sdbName} (at : rest) =
  let body = fromSqlDataWithArgs rest
  in LogObject
    { loAt    = fromSqlData at
    , loNS    = ""
    , loKind  = ""
    , loHost  = sdbName
    , loTid   = logObjectNeedsTIdforAnalysis rest body
    , loBody  = body
    }

-- There's only a couple of log objects that need the TId field for analysis.
-- Hence, it's only stored for those.
-- NB. The assumption here is it is the last column in the schema for table 'txns'
logObjectNeedsTIdforAnalysis :: [SQLData] -> LOBody -> TId
logObjectNeedsTIdforAnalysis args = \case
  LOTxsCollected{}  -> theTId
  LOTxsProcessed{}  -> theTId
  _                 -> TId ShortText.empty
  where
    theTId = fromSqlData $ last args

toLOBodyConverters :: [SQLData] -> ML.Map TL.Text LOBody
toLOBodyConverters args = ML.fromList
  [ ( "LOResources",          LOResources (fromSqlData $ last args))

  , ( "LOTraceStartLeadershipCheck"
    , LOTraceStartLeadershipCheck (fromSqlData slot) (fromSqlData utxoSize) (fromSqlData chainDens)
    )

  -- forging
  , ( "LOBlockContext",       LOBlockContext (fromSqlData slot) (fromSqlData block))
  , ( "LOLedgerState",        LOLedgerState (fromSqlData slot))
  , ( "LOLedgerView",         LOLedgerView (fromSqlData slot))
  , ( "LOTraceLeadershipDecided"
    , LOTraceLeadershipDecided (fromSqlData slot) (fromSqlData block)
    )
  , ( "LOTickedLedgerState",  LOTickedLedgerState (fromSqlData slot))
  , ( "LOMempoolSnapshot",    LOMempoolSnapshot (fromSqlData slot))
  , ( "LOBlockForged",        uncurry (LOBlockForged (fromSqlData slot) (fromSqlData block)) (fromSqlData hash))

  -- diffusion
  , ( "LOChainSyncClientSeenHeader"
    , LOChainSyncClientSeenHeader (fromSqlData slot) (fromSqlData block) (fromSqlData hash)
    )
  , ( "LOBlockFetchClientRequested"
    , LOBlockFetchClientRequested (fromSqlData hash) (fromSqlData block)
    )
  , ( "LOBlockFetchClientCompletedFetch"
    , LOBlockFetchClientCompletedFetch (fromSqlData hash)
    )
  , ( "LOChainSyncServerSendHeader"
    , LOChainSyncServerSendHeader (fromSqlData hash)
    )
  , ( "LOBlockFetchServerSending"
    , LOBlockFetchServerSending (fromSqlData hash)
    )
  , ( "LOBlockAddedToCurrentChain"
    , LOBlockAddedToCurrentChain (fromSqlData hash) (fromSqlData slot) (fromSqlData block)
    )

  , ( "LOLedgerTookSnapshot", LOLedgerTookSnapshot)

  -- txn receive path
  , ( "LOTxsCollected",       LOTxsCollected (fromSqlData count))
  , ( "LOTxsProcessed",       LOTxsProcessed (fromSqlData count) (fromSqlData rejected))
  , ( "LOMempoolTxs",         LOMempoolTxs (fromSqlData count))
  , ( "LOMempoolRejectedTx",  LOMempoolRejectedTx)

  -- constructor not expected to appear given the definition of `selectAll`
  , ( "LODecodeError",        errorGiven "LODecodeError")

  -- all constructors not expected to appear given the definition of `logLineToSQL`
  , ( "LOAny",                errorGiven "LOAny")
  , ( "LOGeneratorSummary",   errorGiven "LOGeneratorSummary")
  , ( "LOTxsAcked",           errorGiven "LOTxsAcked")
  ]
  where
    errorGiven cons = LODecodeError (ShortText.pack $ show args) ("toLOBodyConverters: unexpected " <> cons <> " (with args)")

    -- match remaining columns (after 'at' and 'cons') on a result row from `selectAll`,
    -- offering custom matches for each table.

    -- table: event
    slot : block : _ : hash : _ = args

    -- table: slot
    _ : utxoSize : chainDens : _ = args

    -- table: txns
    count : rejected : _ = args

toLOBody :: [SQLData] -> LOBody
toLOBody (SQLText cons : args) = fromMaybe unresolved resolve
  where
    resolve     = TL.fromStrict cons `ML.lookup` toLOBodyConverters args
    unresolved  = LODecodeError (ShortText.fromText cons) "toLOBody: no converter for that constructor; LOBody type definition may have changed in `locli` code"
toLOBody r = error $ "toLOBody: could not pattern match on result row " ++ show r


allLOBodyConstructors, knownLOBodyConstructors :: Set.Set TL.Text
knownLOBodyConstructors = ML.keysSet $ toLOBodyConverters []
allLOBodyConstructors   = Set.fromList $ map (TL.pack . showConstr) (dataTypeConstrs $ dataTypeOf (undefined :: LOBody))


--
-- data marshalling
--

class AsSQLData x where
  toSqlData   :: x -> SQLData

  fromSqlData :: SQLData -> x

  fromSqlDataWithArgs :: [SQLData] -> x
  fromSqlDataWithArgs = \case
    [x] -> fromSqlData x
    _   -> error "fromSqlDataWithArgs(default): arg count must be exactly one"


instance {-# OVERLAPPABLE #-} Integral a => AsSQLData a where
  toSqlData   = SQLInteger . fromIntegral
  fromSqlData = withSqlInteger fromIntegral

instance AsSQLData Bool where
  toSqlData   = bool (SQLInteger 0) (SQLInteger 1)
  fromSqlData = withSqlInteger (== 1)

instance AsSQLData Double where
  toSqlData   = SQLFloat
  fromSqlData = withSqlFloat id

instance AsSQLData String where
  toSqlData   = SQLText . TS.pack
  fromSqlData = withSqlText TS.unpack

instance AsSQLData UTCTime where
  toSqlData   = SQLFloat . realToFrac . utcTimeToPOSIXSeconds
  fromSqlData = withSqlFloat (posixSecondsToUTCTime . realToFrac)

instance AsSQLData LOBody where
  toSqlData           = SQLText . TS.pack . showConstr . toConstr
  fromSqlData         = const $ error "fromSqlData(LOBody): argument list needed"
  fromSqlDataWithArgs = toLOBody

instance AsSQLData SlotNo where
  toSqlData   = toSqlData . unSlotNo
  fromSqlData = SlotNo . fromSqlData

instance AsSQLData BlockNo where
  toSqlData   = toSqlData . unBlockNo
  fromSqlData = BlockNo . fromSqlData

instance AsSQLData ShortText.ShortText where
  toSqlData   = SQLText . ShortText.toText
  fromSqlData = withSqlText ShortText.fromText

instance AsSQLData Hash where
  toSqlData   = toSqlData . unHash
  fromSqlData = Hash . fromSqlData

instance AsSQLData TId where
  toSqlData   = toSqlData . unTId
  fromSqlData = TId . fromSqlData

-- a shortcut, so we only need one TEXT argument column in table `event`
instance AsSQLData (Hash, Hash) where
  toSqlData (unHash -> h1, unHash -> h2) =
    SQLText . ShortText.toText $ h1 <> "|" <> h2
  fromSqlData = withSqlText $ \t ->
    case TS.splitOn "|" t of
      [h1, h2] -> (Hash $ ShortText.fromText h1, Hash $ ShortText.fromText h2)
      _        -> error "fromSqlData(Hash,Hash): unexpected pipe-separation"

instance AsSQLData ResourceStats where
  toSqlData   = SQLBlob . BSL.toStrict . Aeson.encode
  fromSqlData = withSqlBlob (fromJust . Aeson.decodeStrict)

-- this must conform to the columns in table `summary` / serialisation in `summaryToSql`
instance AsSQLData SummaryDB where
  toSqlData   = const $ error "toSqlData(SummaryDB): can't be represented as a single SQLData; use `summaryToSql`"
  fromSqlData = const $ error "fromSqlData(SummaryDB): argument list needed"
  fromSqlDataWithArgs [c1, c2, c3, c4] =
    SummaryDB
    { sdbName     = Host (fromSqlData c1)
    , sdbLines    = fromSqlData c2
    , sdbFirstAt  = fromSqlData c3
    , sdbLastAt   = fromSqlData c4
    }
  fromSqlDataWithArgs x = error $ "fromSqlDataWithArgs(SummaryDB): expected 4 columns, got:" ++ show x

instance AsSQLData a => AsSQLData (SMaybe a) where
  toSqlData   = smaybe SQLNull toSqlData
  fromSqlData = \case
    SQLNull -> SNothing
    a       -> SJust (fromSqlData a)


withSqlText :: (TS.Text -> a) -> SQLData -> a
withSqlText f = \case
  SQLText t -> f t
  a         -> error $ "withSqlText: no match on " ++ show a

withSqlInteger :: (Int64 -> a) -> SQLData -> a
withSqlInteger f = \case
  SQLInteger i  -> f i
  a             -> error $ "withSqlInteger: no match on " ++ show a

withSqlFloat :: (Double -> a) -> SQLData -> a
withSqlFloat f = \case
  SQLFloat d  -> f d
  a           -> error $ "withSqlFloat: no match on " ++ show a

withSqlBlob :: (ByteString -> a) -> SQLData -> a
withSqlBlob f = \case
  SQLBlob b   -> f b
  a           -> error $ "withSqlBlob: no match on " ++ show a
