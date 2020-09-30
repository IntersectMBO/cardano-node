{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Chairman.Chairman
  ( chairmanOver
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Data.String
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           System.Exit (ExitCode (..))
import           System.FilePath.Posix ((</>))
import           Text.Show

import qualified Data.List as L
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

chairmanOver :: (MonadTest m,  MonadResource m, MonadCatch m, HasCallStack) => H.Conf -> [String] -> m ()
chairmanOver H.Conf {..} allNodes = do
  -- Run chairman
  hChairmanProcesses <- forM allNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ "chairman-" <> node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ "chairman-" <> node <> ".stderr.log"
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

    (_, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procChairman
        [ "--timeout", "100"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--security-parameter", "2160"
        , "--testnet-magic", show @Int testnetMagic
        , "--slot-length", "20"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    return hProcess

  -- Check for chairman success
  forM_ (L.zip allNodes hChairmanProcesses) $ \(node, hProcess) -> do
    chairmanResult <- H.waitSecondsForProcess 110 hProcess

    case chairmanResult of
      Right ExitSuccess -> return ()
      _ -> do
        H.note_ $ "Failed with: " <> show chairmanResult
        H.noteM_ $ H.noteTempFile logDir $ "chairman-" <> node <> ".stdout.log"
        H.noteM_ $ H.noteTempFile logDir $ "chairman-" <> node <> ".stderr.log"
        H.failure
