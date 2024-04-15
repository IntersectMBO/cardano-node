{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
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

> time cabal run tq -- --file big-node:bench/stdout-tools/5nodes.stdout --reducer count-lines
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

$ time cabal run tq -- --file big-node:bench/stdout-tools/5nodes.stdout --reducer count-FLSLCP
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

> time cabal run tq -- --file 5stdout:bench/stdout-tools/5stdout --reducer heap-changes
real  0m54.360s
user  0m53.606s
sys 0m0.873s

Heap changes (52 nodes)
--------------------------------------------------------------------------------

> time for i in `seq 0 51`; do echo "node-$i" && grep -E "^{.*" run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom/node-"$i"/stdout | jq --compact-output 'if .ns == "Resources" then .data.Heap else empty end' | uniq; done
real  9m40.413s
user  9m49.158s
sys 1m4.572s

> cabal run tq -- --run run/2024-04-05-22-32-6b142-891-value-40M64G-nomadperfssd-bage-nom --reducer heap-changes
real  9m10.366s
user  8m12.345s
sys 0m46.550s

-}

{-- RTS params: +RTS -xc -s -l -hc

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
-- package: time.
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
-- package: text.
import qualified Data.Text as Text
-- package: async.
import qualified Control.Concurrent.Async as Async
-- package: optparse-applicative.
import qualified Options.Applicative as Opt
-- library.
import qualified Data.Log as Log
import qualified Cardano.Tracer.Trace as Trace
import qualified Cardano.Tracer.Reducer as Reducer

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

data ReducerElem = forall r. (Show r, Reducer.Reducer r) => MkReducer r

instance Show ReducerElem where
  show (MkReducer r) = show r

cliFilterReader :: String -> Either String ReducerElem
cliFilterReader str = case str of
  "count-lines"  -> Right $ MkReducer Reducer.CountLines
  "count-FLSLCP" -> Right $ MkReducer Reducer.CountStartLeadershipCheckPlus
  "heap-changes" -> Right $ MkReducer Reducer.HeapChanges
  "missed-slots" -> Right $ MkReducer Reducer.MissedSlots
  "1s-silences"  -> Right $ MkReducer Reducer.OneSecondSilences
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

optsParser :: Opt.Parser CliOpts
optsParser = CliOpts <$>
        (
          (map
            -- Parse the optional file label, looks for ":" as separator.
            addFileLabel
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
    <*> some (
          (Opt.option $ Opt.eitherReader cliFilterReader)
          (    Opt.long "reducer"
            <> Opt.short 'r'
            <> Opt.metavar "REDUCER"
            <> Opt.help "Reducer"
          )
        )
  where
    addFileLabel str =
      case span (/= ':') str of
        (f,"") -> ("",f)
        (f, s) -> (f,drop 1 s)

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
          (Reducer.reducerOf r)
          (Reducer.initialOf r)
          fp
        print logName
        Reducer.printAns r ans
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
          (Reducer.reducerOf r)
          (Reducer.initialOf r)
          fp
        return (logName, ans)
      )
      (files cliOpts)
    mapM_
      (\(logName,ans) -> do
        print logName
        Reducer.printAns r ans
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

-- Allow to `fold'` through the log file but in JSON format.
lineFoldl' :: (a -> (Either Text.Text Trace.Trace) -> a) -> a -> FilePath -> IO a
lineFoldl' f initialAcc filePath = do
  Log.lineFoldl'
    (\acc textLine ->
      -- CRITICAL: Has to be "STRICT" to keep `Log.lineFoldl'`'s behaviour.
      --           I repeat, the accumulator function has to be strict!
      let !nextAcc = f acc (Trace.fromJson textLine)
      in nextAcc
    )
    initialAcc
    filePath

--------------------------------------------------------------------------------

-- TODO:
{--
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

foldlWhile :: Foldable t => (a -> Bool) -> (r -> a -> r) -> r -> t a -> r
foldlWhile t f a xs  =  foldr cons (\acc -> acc) xs a
  where
    cons x r acc | t x  =  r (f acc x)
                 | otherwise  =  acc
--}
