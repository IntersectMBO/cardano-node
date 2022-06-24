{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Assert
  ( readJsonLines
  , assertChainExtended
  ) where

import           Control.Lens ((^.))
import           Control.Monad (Monad (..))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson (Value)
import           Data.Bool (Bool (..))
import           Data.Eq (Eq (..))
import           Data.Function (($), (.))
import           Data.Functor ((<$>))
import           Data.Maybe (mapMaybe)
import           Data.Word (Word8)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           System.FilePath (FilePath)
import           System.IO (IO)
import           Test.Runtime (NodeLoggingFormat (..))

import qualified Data.Aeson as J
import qualified Data.Aeson.Lens as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Test.Process as H

newlineBytes :: Word8
newlineBytes = 10

readJsonLines :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m [Value]
readJsonLines fp = mapMaybe (J.decode @Value) . LBS.split newlineBytes <$> H.evalIO (LBS.readFile fp)

fileJsonGrep :: FilePath -> (Value -> Bool) -> IO Bool
fileJsonGrep fp f = do
  lines <- LBS.split newlineBytes <$> LBS.readFile fp
  let jsons = mapMaybe (J.decode @Value) lines
  return $ L.any f jsons

assertChainExtended :: (H.MonadTest m, MonadIO m)
  => DTC.UTCTime
  -> NodeLoggingFormat
  -> FilePath
  -> m ()
assertChainExtended deadline nodeLoggingFormat nodeStdoutFile =
  H.assertByDeadlineIOCustom "Chain not extended" deadline $ do
    case nodeLoggingFormat of
      NodeLoggingFormatAsText -> IO.fileContains "Chain extended, new tip" nodeStdoutFile
      NodeLoggingFormatAsJson -> fileJsonGrep nodeStdoutFile (\v -> v ^. J.key "data" . J.key "kind" . J._String == "")

