{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Exception (IOException, catch, finally)
import           Control.Monad (unless, void, when)
import           System.Directory (doesFileExist, getCurrentDirectory, listDirectory,
                   removePathForcibly)
import           System.Exit (exitFailure)
import           System.FilePath (takeDirectory, (</>))
import           System.IO (hPutStrLn, stderr)
import           System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess,
                   proc, terminateProcess, waitForProcess)

main :: IO ()
main = do
  let dir      = "/tmp/cardano-tracer-restart-test-logs"
      sock     = "sock"
      logLink  = dir </> (sock ++ "@0") </> "node.json"
      waitSecs = 5

  cfg <- resolveConfigPath

  -- Fresh start.
  catchIgnore (removePathForcibly dir)

  -- Run demo-forwarder
  forwarderH <- startProc "demo-forwarder" [sock, "Initiator"]

  let cleanup = catchIgnore (removePathForcibly dir)

  -- Ensure the forwarder is killed and logs cleaned at the very end
  finally
    (do
      -- Start first cardano-tracer
      tracer1H <- startProc "cardano-tracer" ["--config", cfg]

      finally
        (do
          sleepSeconds waitSecs
          ensureNonEmpty logLink
            "Nothing has been written to the file with trace messages"

          -- Kill first tracer
          terminateProcess tracer1H
          void (waitForProcess tracer1H)

          -- Start second tracer
          tracer2H <- startProc "cardano-tracer" ["--config", cfg]

          finally
            (do
              sleepSeconds waitSecs
              ensureNonEmpty logLink
                "Nothing has been written to the file since the restart of cardano-tracer"

              let logDir = dir </> (sock ++ "@0")
              entries <- listDirectory logDir
              let candidates = filter (not . isSpecialEntry) entries
              let count = length candidates
              when (count /= 2) $ do
                hPutStrLn stderr $
                  "Two log files are expected to be present, found: " ++ show count
                exitFailure
              pure ())
            (safeTerminate tracer2H))
        (safeTerminate tracer1H))
    (do
        safeTerminate forwarderH
        cleanup)

--------------------------------------------------------------------------------
-- Helpers

startProc :: FilePath -> [String] -> IO ProcessHandle
startProc cmd args = do
  (_, _, _, ph) <- createProcess (proc cmd args)
                    { std_in  = Inherit
                    , std_out = Inherit
                    , std_err = Inherit
                    }
  pure ph

sleepSeconds :: Int -> IO ()
sleepSeconds s = threadDelay (s * 1000000)

ensureNonEmpty :: FilePath -> String -> IO ()
ensureNonEmpty fp errMsg = do
  exists <- doesFileExist fp
  unless exists $ failWith errMsg
  sz <- getFileSize fp
  when (sz == 0) $ failWith errMsg
  where
    failWith msg = hPutStrLn stderr msg >> exitFailure

getFileSize :: FilePath -> IO Integer
getFileSize fp = do
  contents <- readFile fp
  pure (toInteger (length contents))

safeTerminate :: ProcessHandle -> IO ()
safeTerminate ph = terminateProcess ph >> void (waitForProcess ph)

catchIgnore :: IO () -> IO ()
catchIgnore act = act `catch` \(_ :: IOException) -> pure ()

isDot :: FilePath -> Bool
isDot "."  = True
isDot ".." = True
isDot _    = False

isCurrentLog :: FilePath -> Bool
isCurrentLog "node.json" = True
isCurrentLog _           = False

isSpecialEntry :: FilePath -> Bool
isSpecialEntry fp = isDot fp || isCurrentLog fp

resolveConfigPath :: IO FilePath
resolveConfigPath = go =<< getCurrentDirectory
  where
    configRelative root =
      root </> "cardano-tracer" </> "test" </> "restart-test" </> "restart-config.yaml"

    go dir = do
      let cfgPath = configRelative dir
      exists <- doesFileExist cfgPath
      if exists
        then pure cfgPath
        else do
          let parent = takeDirectory dir
          if parent == dir
            then do
              hPutStrLn stderr "Unable to locate restart-config.yaml"
              exitFailure
            else go parent
