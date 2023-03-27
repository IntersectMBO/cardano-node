{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.Pipes
  ( tests
  ) where

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Prelude

#ifdef UNIX
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import           System.IO (hClose, hFlush, hPutStr)
import           System.Posix.IO (closeFd, createPipe, fdToHandle)

import           Cardano.Api

import           Cardano.CLI.Shelley.Run.Read
import           Test.OptParse

import           Hedgehog (Property, discover, forAll, (===))
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Gen as G
import           Hedgehog.Internal.Property (failWith)
import qualified Hedgehog.Range as R
import           System.FilePath ((</>))

#else

import           Hedgehog (Property, discover, property, success)
#endif

import qualified Hedgehog as H

#ifdef UNIX

prop_readFromPipe :: Property
prop_readFromPipe = H.withTests 10 . H.property . H.moduleWorkspace "tmp" $ \ws -> do

  s <- forAll $ G.string (R.linear 1 8192) G.ascii

  let testFile = ws </> "test-file"

  H.writeFile testFile s

  -- We first test that we can read a filepath
  testFp <- noteInputFile testFile
  testFileOrPipe <- liftIO $ fileOrPipe (File testFp)
  testBs <- liftIO $ readFileOrPipe testFileOrPipe

  if LBS.null testBs
  then failWith Nothing
         $ "readFileOrPipe failed to read file: " <> fileOrPipePath testFileOrPipe
  else do
    -- We now test that we can read from a pipe.
    -- We first check that the IORef has Nothing
    mContents <- liftIO $ fileOrPipeCache testFileOrPipe
    case mContents of
      Just{} -> failWith Nothing "readFileOrPipe has incorrectly populated its IORef with contents read from a filepath."
      Nothing -> do
        -- We can reuse testFileOrPipe because we know the cache (IORef) is empty
        let txBodyStr = BSC.unpack $ LBS.toStrict testBs
        fromPipeBs <- liftIO $ withPipe txBodyStr
        if LBS.null fromPipeBs
        then failWith Nothing "readFileOrPipe failed to read from a pipe"
        else testBs === fromPipeBs

-- | Create a pipe, write some String into it, read its contents and return the contents
withPipe :: String -> IO LBS.ByteString
withPipe contents = do
  (readEnd, writeEnd) <- createPipe

  writeHandle <- fdToHandle writeEnd

  -- Write contents to pipe
  hPutStr writeHandle contents
  hFlush writeHandle
  hClose writeHandle
  pipe <- fileOrPipe $ File $ "/dev/fd/" ++ show readEnd

  -- Read contents from pipe
  readContents <- readFileOrPipe pipe
  closeFd readEnd
  pure readContents

#else
prop_readFromPipe :: Property
prop_readFromPipe = property success
#endif

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
