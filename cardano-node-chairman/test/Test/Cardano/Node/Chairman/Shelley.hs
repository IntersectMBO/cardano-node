{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Node.Chairman.Shelley
  ( prepropChairman
  , tests
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (liftResourceT, register, resourceForkIO)
import           Data.Aeson
import           Data.Bool
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.List ((\\))
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String (String)
import           GHC.Float
import           Hedgehog (Property, discover)
import           Hedgehog.Extras.Stock.Aeson
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           System.Exit (ExitCode (..))
import           System.FilePath.Posix ((</>))
import           System.IO (IO)
import           Text.Read
import           Text.Show

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.FilePath.Posix as FP
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO
import qualified System.Process as IO
import qualified Test.Cardano.Process as H

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}

rewriteGenesisSpec :: Int -> Value -> Value
rewriteGenesisSpec supply =
  rewriteObject
    $ HM.insert "activeSlotsCoeff" (toJSON @Double 0.1)
    . HM.insert "securityParam" (toJSON @Int 10)
    . HM.insert "epochLength" (toJSON @Int 1500)
    . HM.insert "maxLovelaceSupply" (toJSON supply)
    . flip HM.adjust "protocolParams"
      ( rewriteObject (HM.insert "decentralisationParam" (toJSON @Double 0.7))
      )

alreadyDone :: STM.TVar Bool
alreadyDone = IO.unsafePerformIO $ STM.newTVarIO True
{-# NOINLINE alreadyDone #-}

prop_chairman :: Property
prop_chairman = prepropChairman alreadyDone

prepropChairman :: STM.TVar Bool -> Property
prepropChairman tvDone = H.propertyOnce . H.workspace "chairman" $ \_ -> do
  void . register . liftIO $ IO.appendFile "logs.txt" "Cleanup\n"

  void . liftResourceT . resourceForkIO $ do
    liftIO $ IO.appendFile "logs.txt" "Forked\n"
    liftIO . STM.atomically $ do
      done <- STM.readTVar tvDone
      unless done STM.retry
    liftIO $ IO.appendFile "logs.txt" "Thread done\n"

    liftIO $ IO.appendFile "logs.txt" "Done\n"
    return ()

  H.success

tests :: IO Bool
tests = H.checkParallel $$discover
