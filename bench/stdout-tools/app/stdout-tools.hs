{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ExistentialQuantification #-}

{-

nix-shell OR nix-shell -p ghc cabal-install haskellPackages.eventlog2html jq

Count lines
--------------------------------------------------------------------------------

> ls -lah bench/stdout-tools/5nodes.stdout
-rw-r--r-- 1 fmaste users 6.4G Apr 10 19:28 bench/stdout-tools/5nodes.stdout

> time cat bench/stdout-tools/5nodes.stdout | wc -l
real  0m1.946s
user  0m0.105s
sys 0m2.728s

> time jq --raw-input . bench/stdout-tools/5nodes.stdout | wc -l
25581640
real  1m30.707s
user  1m28.129s
sys 0m8.124s

> time cabal run stdout-tools -- --file big-node:bench/stdout-tools/5nodes.stdout --filter count-lines
25581640
real  0m11.630s
user  0m10.836s
sys 0m0.826s

Count all the ns="Forge.Loop.StartLeadershipCheckPlus"
--------------------------------------------------------------------------------

-- Using jq for everything:
> time jq --raw-input --compact-output 'try fromjson | if (type == "object" and has("at")) then select(.ns=="Forge.Loop.StartLeadershipCheckPlus") else empty end' bench/stdout-tools/5nodes.stdout | wc -l
264150
real  1m30.615s
user  1m29.159s
sys 0m1.502s

-- Using jq but first filter non JSON lines with grep:
> time grep -E "^{.*" bench/stdout-tools/5nodes.stdout | jq --compact-output 'select(.ns == "Forge.Loop.StartLeadershipCheckPlus")' | wc -l
264150
real  1m9.828s
user  1m12.247s
sys 0m5.901s

$ time cabal run stdout-tools -- --file big-node:bench/stdout-tools/5nodes.stdout --filter count-FLSLCP
264150
real  0m26.420s
user  0m25.654s
sys 0m0.837s

Heap changes
--------------------------------------------------------------------------------

> grep -E "^{.*" bench/stdout-tools/5stdout | jq 'select(.ns == "Resources") | .data.Heap' | uniq
real  1m5.810s
user  1m7.716s
sys 0m3.674s

> time cabal run stdout-tools -- --file 5stdout:bench/stdout-tools/5stdout --filter heap-changes
real  0m54.360s
user  0m53.606s
sys 0m0.873s

Heap changes (52 nodes)
--------------------------------------------------------------------------------

> time for i in `seq 0 51`; do echo "node-$i" && grep -E "^{.*" run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-"$i"/stdout | jq --compact-output 'if .ns == "Resources" then .data.Heap else empty end' | uniq; done
real  9m40.413s
user  9m49.158s
sys 1m4.572s

> cabal run stdout-tools -- --run run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom --reducer heap-changes
real  9m10.366s
user  8m12.345s
sys 0m46.550s

-}

{-- RTS params:

-N:
There are two ways to run a program on multiple processors: call
Control.Concurrent.setNumCapabilities from your program, or use the RTS -N ⟨x⟩
options. -N⟨x⟩

-s:
Add the -s [⟨file⟩] RTS option when running the program to see timing stats,
which will help to tell you whether your program got faster by using more CPUs
or not. If the user time is greater than the elapsed time, then the program used
more than one CPU. You should also run the program without -N ⟨x⟩ for
comparison.

The output of +RTS -s tells you how many “sparks” were created and executed
during the run of the program (see RTS options to control the garbage
collector), which will give you an idea how well your par annotations are
working.

> eventlog2html stdout-tools.eventlog

Cabal
 --enable-profiling             Enable Executable and library profiling
 --disable-profiling            Disable Executable and library profiling
 --profiling-detail=level       Profiling detail level for executable and
                                library (default, none, exported-functions,
                                toplevel-functions, all-functions, late).
 --library-profiling-detail=level
                                Profiling detail level for libraries only.
--}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

-- base.
import           Control.Applicative (some, (<|>))
import           Control.Monad (when, foldM)
import           Data.Foldable (toList)
import           GHC.Generics
import           Data.Kind (Type)

-- package: time.
import           Data.Time.Clock
  ( UTCTime
  , getCurrentTime
  , NominalDiffTime
  , diffUTCTime
  )
-- package: containers.
import qualified Data.Sequence as Seq
-- package: text.
import qualified Data.Text as Text
-- package: text-iso8601-0.1
import qualified Data.Time.FromText as ParseTime
-- package: aeson.
import qualified Data.Aeson as Aeson
-- package: async.
import qualified Control.Concurrent.Async as Async
-- package: optparse-applicative.
import qualified Options.Applicative as Opt

import qualified Data.Log as Log

--------------------------------------------------------------------------------

data CliOpts = CliOpts
  {
  -- "--file" arguments with an optional file label if ":" separator is found.
    files      :: [(String, FilePath)]
  , inParallel :: Bool
  -- "--reducer" arguments.
  , reducers   :: [ReducerElem]
  }
  deriving Show

data ReducerElem = forall r. (Show r, Reducer r) => MkReducer r

instance Show ReducerElem where
  show (MkReducer r) = show r

cliFilterReader :: String -> Either String ReducerElem
cliFilterReader str = case str of
  "count-lines"  -> Right $ MkReducer CountLines
  "count-FLSLCP" -> Right $ MkReducer CountStartLeadershipCheckPlus
  "heap-changes" -> Right $ MkReducer HeapChanges
  "missed-slots" -> Right $ MkReducer MissedSlots
  "1s-silences"  -> Right $ MkReducer OneSecondSilences
  _ -> Left str

main :: IO ()
main = do
  cliOpts <- Opt.execParser $ Opt.info (optsParser Opt.<**> Opt.helper)
    (     Opt.fullDesc
       <> Opt.progDesc "Print a greeting for TARGET"
       <> Opt.header "hello - a test for optparse-applicative"
    )
  run cliOpts

--------------------------------------------------------------------------------

class Show r => Reducer r where
  type family Accum r :: Type
  initialOf :: r -> Accum r
  reducerOf :: r -> Accum r -> Cursor -> Accum r
  showAns   :: r -> Accum r -> String
  printAns  :: r -> Accum r -> IO ()
  printAns r acc = putStrLn $ showAns r acc

data CountLines = CountLines
  deriving Show

data CountStartLeadershipCheckPlus = CountStartLeadershipCheckPlus
  deriving Show

data HeapChanges = HeapChanges
  deriving Show

data MissedSlots = MissedSlots
  deriving Show

data OneSecondSilences = OneSecondSilences
  deriving Show

instance Reducer CountLines where
  type instance Accum CountLines = Int
  initialOf _ = 0
  reducerOf _ = (\l _ -> l + 1)
  showAns   _ = show

instance Reducer CountStartLeadershipCheckPlus where
  type instance Accum CountStartLeadershipCheckPlus = Int
  initialOf _ = 0
  reducerOf _ = (\l (Cursor _ maybeMsg) ->
    case maybeMsg of
      Nothing -> l
      (Just msg) ->
        if ns msg == "Forge.Loop.StartLeadershipCheckPlus"
        then l + 1
        else l
    )
  showAns  _ = show

instance Reducer HeapChanges where
  type instance Accum HeapChanges = (Maybe Integer, Seq.Seq (UTCTime, Integer))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ ans (Cursor _ Nothing) = ans
  reducerOf _ ans@(maybePrevHeap, sq) (Cursor _ (Just !cursorTrace)) =
    case Aeson.eitherDecodeStrictText (more cursorTrace) of
      (Right !resourcesMore) ->
        -- TODO: Use `unsnoc` when available
        let actualHeap = resourcesHeap $ traceData resourcesMore
        in case maybePrevHeap of
          Nothing -> (Just actualHeap, Seq.singleton (at cursorTrace, actualHeap))
          (Just prevHeap) ->
            if actualHeap == prevHeap
            then ans
            else (Just actualHeap, sq Seq.|> (at cursorTrace, actualHeap))
      (Left _) -> ans
  showAns _ = show
  printAns _ (_, sq) = mapM_
    (\(t,h) -> putStrLn $ show t ++ ": " ++ show h)
    (toList sq)

instance Reducer MissedSlots where
  type instance Accum MissedSlots = (Maybe Integer, Seq.Seq Integer)
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ ans (Cursor _ Nothing) = ans
  reducerOf _ ans@(maybePrevSlot, !sq) (Cursor _ (Just !(Trace _ "Forge.Loop.StartLeadershipCheckPlus" aeson))) =
    case Aeson.eitherDecodeStrictText aeson of
      (Right !dataWithSlot) ->
        -- TODO: Use `unsnoc` when available
        let actualSlot = slot $ traceData dataWithSlot
        in case maybePrevSlot of
          Nothing -> (Just actualSlot, Seq.empty)
          (Just prevSlot) ->
            if actualSlot == prevSlot + 1
            then (Just actualSlot, sq)
            else (Just actualSlot, sq Seq.>< (Seq.fromList [(prevSlot+1)..(actualSlot-1)]))
      (Left _) -> ans
  reducerOf _ ans (Cursor _ (Just _)) = ans
  showAns _ = show
  printAns _ (_, sq) = do
    ans <- foldM
      (\maybePrevSlot lostSlot ->
        case maybePrevSlot of
          Nothing -> putStr (show lostSlot) >> return (Just lostSlot)
          (Just prevSlot) ->
            if prevSlot + 1 == lostSlot
            then return (Just lostSlot)
            else putStrLn (".." ++ show prevSlot) >> return Nothing
      )
      Nothing
      (toList sq)
    when (ans /= Nothing) (putStrLn "")

instance Reducer OneSecondSilences where
  type instance Accum OneSecondSilences = (Maybe Trace, Seq.Seq (NominalDiffTime, Trace, Trace))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ (Nothing, sq) cursor = (_maybeCursorMsg cursor, sq)
  reducerOf _ (Just _prevMsg, sq) (Cursor _ Nothing) = (Just _prevMsg, sq)
  reducerOf _ (Just _prevMsg, sq) (Cursor _ (Just cursorTrace)) =
    let diffTime = diffUTCTime (at cursorTrace) (at _prevMsg)
    in if diffTime >  fromInteger 2
    then (Just cursorTrace, sq Seq.|> (diffTime, _prevMsg, cursorTrace))
    else (Just cursorTrace, sq)
  showAns   _ = show
  printAns _ (_,sq) = mapM_
    (\(ndt, t1, _) ->
      putStrLn $ (show ndt) ++ " (" ++ (show $ at t1) ++ ")"
    )
    (toList sq)

--------------------------------------------------------------------------------

optsParser :: Opt.Parser CliOpts
optsParser = CliOpts <$>
        (
          (map
            -- Parse the optional file label, looks for ":" as separator.
            (\str ->
              case span (/= ':') str of
                (f,"") -> ("",f)
                (f, s) -> (f,drop 1 s)
            )
            <$>
            some (
              Opt.strOption
              (    Opt.long "file"
                <> Opt.short 'f'
                <> Opt.metavar "FILENAME"
                <> Opt.help "Input file"
              )
            )
          )
        <|>
          (
            (\runDir -> map
              (\n -> ("node-" ++ show n,runDir ++ "/node-" ++ show n ++ "/stdout"))
              ([0..51]::[Int])
            )
            <$>
            Opt.strOption
            (    Opt.long "run"
              <> Opt.short 'r'
              <> Opt.metavar "RUN"
              <> Opt.help "Run folder"
            )
          )
        )
    <*> Opt.flag False True
          (    Opt.long "parallel"
            <> Opt.help "Process files in parallel"
          )
    <*> (some (
          (Opt.option $ Opt.eitherReader cliFilterReader)
          (    Opt.long "reducer"
            <> Opt.short 'r'
            <> Opt.metavar "REDUCER"
            <> Opt.help "Reducer"
          )
        ))

--------------------------------------------------------------------------------

run :: CliOpts -> IO ()
run (CliOpts _ _ []) = putStrLn "Nothing to do, bye!"
run cliOpts@(CliOpts _ parallel ((MkReducer r):_)) = do
  t0 <- getCurrentTime
  print r
  if not parallel
  then do
    ------------------------------------
    putStrLn "-------------------------"
    putStrLn "Apply filter to all files"
    putStrLn "-------------------------"
    ------------------------------------
    mapM_
      (\(logName,fp) -> do
        ans <- lineFoldl'
          (reducerOf r)
          (initialOf r)
          fp
        print logName
        printAns r ans
      )
      (files cliOpts)
  else do
    ---------------------------------------------------------
    putStrLn "----------------------------------------------"
    putStrLn "Do the same with all files but now in parallel"
    putStrLn "----------------------------------------------"
    ---------------------------------------------------------
    ansParallel <- Async.mapConcurrently
      (\(logName,fp) -> do
        ans <- lineFoldl'
          (reducerOf r)
          (initialOf r)
          fp
        return (logName, ans)
      )
      (files cliOpts)
    mapM_
      (\(logName,ans) -> do
        print logName
        printAns r ans
      )
      ansParallel
  t1 <- getCurrentTime
  print $ diffUTCTime t1 t0

{-- TODO: Switch to open type families for "sequential" and "parallel" folds.
  mapM_
    (\(logName,fp) -> do

      ans <- lineFoldl'
        (\accs cursor -> zipWith (\r' acc -> reducerOf r' acc cursor) rs accs)
        (map initialOf rs)
        fp
      print logName
      mapM_ (\(r, acc) -> putStrLn $ showAns r acc) (zip rs ans)
    )
    (files cliOpts)
--}

  -- End
  return ()

-- A log message.
--------------------------------------------------------------------------------

-- TODO:
-- All traces start with, use this assumption to build a "fast" decoder.
-- {"at":"2024-03-30T00:30:27.015631111Z","ns":"Reflection.TracerInfo"
-- Keep it simple!
data Trace = Trace
  -- Strict of keep thunks of `Data.Time.FromText.parseUTCTime`.
  { at :: UTCTime -- "2024-04-06T11:27:45.37268578Z"
  , ns :: Text.Text
  -- Only do `fromJSON` if needed!
  , more :: Text.Text
  }
  deriving (Eq, Show)

-- Fast & Ugly, Ugly & Fast.
-- Too many assumptions (assumption is the parent of all thing that did not go
-- quite as expected).
traceFromJson :: Text.Text -> Either String Trace
traceFromJson text =
  -- Assume '{"at":"'' is there
  case Text.splitAt 7 text of
    -- Property 'at' assumed as expected.
    ("{\"at\":\"", text') ->
           -- Assume a date like '2024-04-11T12:01:33.2135764Z' is there.
           -- The milliseconds part is variable, can't read a fix amount.
      let (atText, text'' ) = Text.break (== '"') text'
          -- Consume all the text until the next '"'.
          -- First drop the date's last '"' ans assume ',"ns":"' is there.
          (nsText, text''') = Text.break (== '"') (Text.drop 8 text'')
      --in Left $ show (atText, text'', nsText, text''')
      in  case ParseTime.parseUTCTime atText of
            (Left err) -> Left $ "parseUTCTime: " ++ err
            (Right utcTime) -> Right $
              -- Drop closing '",' of 'ns' and leave unconsumed as a new object.
              Trace utcTime nsText ("{" <> (Text.drop 2 text'''))
    _ -> Left "No {\"at\":\""

-- Keep it simple!
data TraceData a = TraceData
  -- Strict of keep thunks of `Data.Time.FromText.parseUTCTime`.
  { traceData :: a
  , sev       :: Text.Text
  , thread    :: Text.Text
  , host      :: Text.Text
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (TraceData a) where
  -- Only using a non-automatic instance because of "data" and "msgData".
  toJSON p@(TraceData _ _ _ _) =
    Aeson.object
      [ "data"   Aeson..= traceData p
      , "sev"    Aeson..= sev       p
      , "thread" Aeson..= thread    p
      , "host"   Aeson..= host      p
      ]

instance Aeson.FromJSON a => Aeson.FromJSON (TraceData a) where
  -- Only using a non-automatic instance because of "data" and "msgData".
  parseJSON =
    Aeson.withObject "TraceData" $ \o -> do
      TraceData
        <$> o Aeson..: "data"
        <*> o Aeson..: "sev"
        <*> o Aeson..: "thread"
        <*> o Aeson..: "host"

{--
class Cursor a where
  cursorText :: a -> Text.Text
  maybeCursorMsg :: a -> Maybe Msg

instance Cursor BasicCursor where
  cursorText = _cursorText
  maybeCursorMsg = _cursorText
--}

-- Keep it simple, stak accumulators if you need more things like line number.
data Cursor = Cursor
  { _cursorText :: Text.Text
  -- All lines are read (converted to Text) but `fromJSON` is lazy.
  , _maybeCursorMsg :: Maybe Trace
  }

-- Allow to `fold'` through the log file but in JSON format.
lineFoldl' :: (a -> Cursor -> a) -> a -> FilePath -> IO a
lineFoldl' f initialAcc filePath = do
  Log.lineFoldl'
    (\acc textLine ->
      let maybeTrace = case traceFromJson textLine of
                      (Left _str) -> Nothing
                      (Right trace) -> Just trace
          -- CRITICAL: Has to be "STRICT" to keep `Log.lineFoldl'`'s behaviour.
          --           I repeat, the accumulator function has to be strict!
          !nextAcc = f acc (Cursor textLine maybeTrace)
      in nextAcc
    )
    initialAcc
    filePath

--------------------------------------------------------------------------------

{--
-- Keep it simple!
data MsgAt = MsgAt
  -- Strict of keep thunks of `Data.Time.FromText.parseUTCTime`.
  { at :: !UTCTime -- "2024-04-06T11:27:45.37268578Z"
  }
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON Msg where

instance Aeson.ToJSON Msg where
--}

-- TODO:
_foldlLog3 :: (
               ((a -> Cursor -> a), a)
             , ((b -> Cursor -> b), b)
             , ((c -> Cursor -> c), c)
             )
          -> FilePath
          -> IO (a,b,c)
{-# SCC _foldlLog3 "_foldlLog3" #-}
_foldlLog3 ((fa,a),(fb,b),(fc,c)) filePath = do
  (a', b', c') <- lineFoldl'
    (\(accA,accB,accC) cursor -> {-# SCC "foldlLog3_f" #-}
        (fa accA cursor, fb accB cursor, fc accC cursor)
    )
    (a,b,c)
    filePath
  --return $! (a', b', c')
  return $ seq a' $ seq b' $ seq c' (a', b', c')

{-- TODO:
foldlWhile :: Foldable t => (a -> Bool) -> (r -> a -> r) -> r -> t a -> r
foldlWhile t f a xs  =  foldr cons (\acc -> acc) xs a
  where
    cons x r acc | t x  =  r (f acc x)
                 | otherwise  =  acc
--}

--------------------------------------------------------------------------------

{--
  "data": {
    "Alloc": 98169147912,
    "CentiBlkIO": 0,
    "CentiCpu": 10831,
    "CentiGC": 4726,
    "CentiMut": 6104,
    "FsRd": 0,
    "FsWr": 8192,
    "GcsMajor": 7,
    "GcsMinor": 3590,
    "Heap": 8629780480,
    "Live": 2529171488,
    "NetRd": 0,
    "NetWr": 0,
    "RSS": 8683200512,
    "Threads": 9,
    "kind": "ResourceStats"
  },
--}
data Resources = Resources
  { resourcesAlloc :: Integer
  , resourcesCentiBlkIO :: Integer
  , resourcesCentiCpu :: Integer
  , resourcesCentiGC :: Integer
  , resourcesCentiMut :: Integer
  , resourcesFsRd :: Integer
  , resourcesGcsMajor :: Integer
  , resourcesGcsMinor :: Integer
  , resourcesHeap :: Integer
  , resourcesLive :: Integer
  , resourcesNetRd :: Integer
  , resourcesNetWr :: Integer
  , resourcesRSS :: Integer
  , resourcesThreads :: Integer
  }
  deriving (Eq, Show, Generic)

msgCustomOptions :: Aeson.Options
msgCustomOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = drop 9
  , Aeson.constructorTagModifier = id
  }

instance Aeson.ToJSON Resources where
  toJSON = Aeson.genericToJSON msgCustomOptions
  toEncoding = Aeson.genericToEncoding msgCustomOptions

instance Aeson.FromJSON Resources where
  parseJSON = Aeson.genericParseJSON msgCustomOptions

--------------------------------------------------------------------------------

data DataWithSlot = DataWithSlot
  { slot :: Integer }
  deriving Generic

instance Aeson.ToJSON DataWithSlot where

instance Aeson.FromJSON DataWithSlot where
