{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Chairman.Chairman
  ( chairmanOver
  ) where

import           Control.Monad
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Data.String
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           GHC.Real
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Test.Base (Integration)
import           Prelude (Double)
import           System.Exit (ExitCode (..))
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath)
import           Text.Printf (printf)
import           Text.Show

import qualified Data.Time.Clock as DTC
import qualified Data.Time.Clock.POSIX as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Environment as IO
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Test.Process as H
import qualified Testnet.Conf as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

mkSprocket :: FilePath -> FilePath -> String -> Sprocket
mkSprocket tempBaseAbsPath socketDir node = Sprocket tempBaseAbsPath (socketDir </> node)

chairmanOver :: UTCTime -> NominalDiffTime -> Int -> H.Conf -> [String] -> Integration ()
chairmanOver startTime timeoutSeconds requiredProgress H.Conf {..} allNodes = do
  maybeChairman <- H.evalIO $ IO.lookupEnv "DISABLE_CHAIRMAN"

  deadline <- H.noteShow $ DTC.addUTCTime timeoutSeconds startTime

  when (maybeChairman /= Just "1") $ do
    nodeStdoutFile <- H.noteTempFile logDir $ "chairman" <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ "chairman" <> ".stderr.log"

    sprockets <- H.noteEach $ fmap (mkSprocket tempBaseAbsPath socketDir) allNodes

    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.evalIO $ IO.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.evalIO $ IO.openFile nodeStderrFile IO.WriteMode

    (_, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procChairman
        ( [ "--deadline", printf "%.3f" (fromRational @Double (toRational (DTC.utcTimeToPOSIXSeconds deadline)))
          , "--config", tempAbsPath </> "configuration.yaml"
          , "--security-parameter", "2160"
          , "--testnet-magic", show @Int testnetMagic
          , "--require-progress", show @Int requiredProgress
          ]
        <> (sprockets >>= (\sprocket -> ["--socket-path", IO.sprocketArgumentName sprocket]))
        ) <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    chairmanResult <- H.waitUntilForProcess (DTC.addUTCTime 20 deadline) hProcess

    case chairmanResult of
      Right ExitSuccess -> return ()
      _ -> do
        now <- H.evalIO DTC.getCurrentTime
        H.note_ $ "Chairman failed with: " <> show chairmanResult <> " at " <> show now
        H.cat nodeStdoutFile
        H.cat nodeStderrFile
        H.failure
