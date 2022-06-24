{-# LANGUAGE TypeApplications #-}

module Test.Assert
  ( readJsonLines
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Maybe (mapMaybe)
import GHC.Stack (HasCallStack)
import Hedgehog (MonadTest)
import System.FilePath (FilePath)

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import qualified Hedgehog as H

readJsonLines :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m [Value]
readJsonLines fp = mapMaybe (J.decode @Value) . LBS.split 10 <$> H.evalIO (LBS.readFile fp)
