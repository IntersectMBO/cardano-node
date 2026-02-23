{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- Package: "base".
import Control.Exception (handle, IOException)
import Control.Monad (forM, forM_)
import Data.List (isSuffixOf, foldl')
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
-- Package: "async".
import qualified Control.Concurrent.Async as Async
-- Package: "directory".
import qualified System.Directory as Dir
-- Package: "filepath".
import qualified System.FilePath as Path
-- Package: "process".
import qualified System.Process as Proc

-------------------------------------------------------------------------------

-- | The external program to run on each .json file.
externalProgram :: FilePath
externalProgram = "cat" 

-- | Arguments to pass to the external program before the file path.
programArgs :: [String]
programArgs = []

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "--- Starting Parallel JSON Directory Traversal ---"
  args <- getArgs
  -- Target path is either command line arg or current directory.
  targetPath <- case args of
    (path:_) -> return path
    []       -> Dir.getCurrentDirectory
  -- Canonicalize the path for absolute reporting.
  root <- Dir.canonicalizePath targetPath
  putStrLn $ "Scanning root: " ++ root
  -- Run the traversal directly
  allResults <- traverseAndProcess root
  -- Single pass to calculate total, successes, and collect failures
  let (total, successCount, failures) = foldl'
        (\(t, s, fs) (path, status) -> case status of
            ExitSuccess      -> (t + 1, s + 1, fs)
            ExitFailure code -> (t + 1, s, (path, code) : fs)) 
        (0 :: Int, 0 :: Int, []) 
        allResults
      failureCount = total - successCount
  putStrLn "--- Processing Complete ---"
  putStrLn $ "Total .json files found: " ++ show total
  putStrLn $ "Successful executions: " ++ show successCount
  putStrLn $ "Failed executions:     " ++ show failureCount
  -- Handle exit status based on results
  if failureCount > 0
  then do
    putStrLn "\n--- Summary of Failures ---"
    forM_ (reverse failures) $ \(path, code) ->
      putStrLn $ path ++ " failed with code: " ++ show code
    -- Exit with error code 1.
    exitFailure
  else do
    -- Exit with code 0.
    exitSuccess

--------------------------------------------------------------------------------

-- | The core worker function that executes the external process.
-- Returns the path and the ExitCode.
processJsonFile :: FilePath -> IO (FilePath, ExitCode)
processJsonFile path = do
  putStrLn $ "[Worker] Processing: " ++ path
  (exitCode, _, stderr) <- Proc.readProcessWithExitCode externalProgram (programArgs ++ [path]) ""
  case exitCode of
    ExitSuccess -> do
      putStrLn $ "[Success] Finished " ++ path
      return (path, exitCode)
    ExitFailure code -> do
      putStrLn $ "[Error] Failed " ++ path ++ " with code " ++ show code
      putStrLn $ "Stderr: " ++ stderr
      return (path, exitCode)

-- | Traverses a path: if it's a directory, it spawns async tasks for all contents.
-- If it's a .json file, it processes it.
traverseAndProcess :: FilePath -> IO [(FilePath, ExitCode)]
traverseAndProcess path = handle
  (\e -> do
      putStrLn $ "Warning: Could not access " ++ path ++ ": " ++ show (e :: IOException)
      return []
  )
  (do
    isDir <- Dir.doesDirectoryExist path
    if isDir
      then do
        -- Get directory contents.
        allEntries <- Dir.getDirectoryContents path
        let entries = filter (`notElem` [".", ".."]) allEntries
        -- Spawn an async task for every item found in this directory.
         tasks <- forM entries $ \entry ->
           Async.async (traverseAndProcess (path Path.</> entry))
        -- Wait for all child tasks and flatten the results.
        results <- mapM Async.wait tasks
        return (concat results)
      else if ".json" `isSuffixOf` path
        then do
          -- It's a file we care about; process it
          res <- processJsonFile path
          return [res]
        else
          -- It's a file we don't care about
          return []
  )

