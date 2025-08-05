{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
-- package: async.
import qualified Control.Concurrent.Async as Async
-- package: optparse-applicative.
import qualified Options.Applicative as Opt
-- library.
import qualified Cardano.Tracer.Reducer as Reducer
import qualified Cardano.Tracer.FilterReduce as FilterReduce

--------------------------------------------------------------------------------

data CliOpts = CliOpts
  {
  -- "--file" arguments with an optional file label if ":" separator is found.
    files      :: [(String, FilePath)]
  , inParallel :: Bool
  -- "--reducer" arguments.
  , builtins   :: [FilterReduce.FilterReduce]
  }
  deriving Show

cliFunctionReader :: String -> Either String FilterReduce.FilterReduce
cliFunctionReader str = case str of
  "count-lines"  -> Right   FilterReduce.countLinesFR
  "count-traces" -> Right   FilterReduce.countTracesFR
  "2s-silences"  -> Right $ FilterReduce.silencesFR  2
  "5s-silences"  -> Right $ FilterReduce.silencesFR  5
  "10s-silences" -> Right $ FilterReduce.silencesFR 10
  "count-FLSLCP" -> Right $ FilterReduce.countNamespaceFR "Forge.Loop.StartLeadershipCheckPlus"
  "missed-slots" -> Right   FilterReduce.missedSlotsFR
  "slot-pauses"  -> Right $ FilterReduce.slotPausesFR 1.75
  "utxo-size"    -> Right   FilterReduce.utxoSizeFR
  "heap-changes" -> Right   FilterReduce.heapChangesFR
  "live-changes" -> Right   FilterReduce.liveChangesFR
  "rss-changes"  -> Right   FilterReduce.rssChangesFR
  "cpu-ticks"    -> Right   FilterReduce.cpuTicksFR
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
              -- TODO: FIXME: Support smaller runs!!!!
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
          (Opt.option $ Opt.eitherReader cliFunctionReader)
          (    Opt.long "builtin"
            <> Opt.short 'b'
            <> Opt.metavar "BUILTIN"
            <> Opt.help "Builtin"
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
run cliOpts@(CliOpts _ parallel (b@(FilterReduce.MkFilterReduce f r):_)) = do
  t0 <- getCurrentTime
  print b
  if not parallel
  then do
    ------------------------------------
    putStrLn "-------------------------"
    putStrLn "Apply filter to all files"
    putStrLn "-------------------------"
    ------------------------------------
    mapM_
      (\(logName,fp) -> do
        ans <- FilterReduce.filterReduce f r fp
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
        ans <- FilterReduce.filterReduce f r fp
        return (logName, ans)
      )
      (files cliOpts)
    mapM_
      (\(logName,ans) -> do
        putStrLn $ "# " ++ logName
        Reducer.printAns r ans
        putStrLn ""
      )
      ansParallel
  t1 <- getCurrentTime
  print $ diffUTCTime t1 t0
  -- End
  return ()

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
