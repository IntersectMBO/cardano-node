{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Plutus.Script.TxInLockingPlutus
  ( hprop_plutus
  ) where

import           Control.Monad
import           Data.Bool (not)
import           Data.Function
import           Data.Functor ((<$>))
import           Data.Int
import           Data.List ((!!))
import           Data.Maybe
import           Data.Monoid
import           Hedgehog (Property, (===))
import           Prelude (head)
import           System.FilePath ((</>))
import           Text.Show (Show (..))

import qualified Data.List as L
import qualified Data.Text as T
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified Hedgehog.Internal.Property as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified Test.Base as H
import qualified Testnet.Cardano as H
import qualified Testnet.Conf as H

hprop_plutus :: Property
hprop_plutus = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  projectBase <- H.note =<< H.evalIO . IO.canonicalizePath =<< H.getProjectBase
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $ H.mkConf tempAbsBasePath' Nothing

  resultFile <- H.noteTempFile tempAbsPath "result.out"

  H.TestnetRuntime { H.bftSprockets, H.testnetMagic } <- H.testnet H.defaultTestnetOptions conf

  cardanoCli <- H.binFlex "cardano-cli" "CARDANO_CLI"

  path <- H.evalIO $ fromMaybe "" <$> IO.lookupEnv "PATH"

  let execConfig = H.ExecConfig
        { H.execConfigEnv = Last $ Just
          [ ("CARDANO_CLI", cardanoCli)
          , ("BASE", projectBase)
          , ("WORK", tempAbsPath)
          , ("UTXO_VKEY", tempAbsPath </> "shelley/utxo-keys/utxo1.vkey")
          , ("UTXO_SKEY", tempAbsPath </> "shelley/utxo-keys/utxo1.skey")
          , ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName (head bftSprockets))
          , ("TESTNET_MAGIC", show @Int testnetMagic)
          , ("PATH", path)
          , ("RESULT_FILE", resultFile)
          ]
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  scriptPath <- H.eval $ projectBase </> "scripts/plutus/example-txin-locking-plutus-script.sh"

  H.exec_ execConfig "bash"
    [ "-x"
    , scriptPath
    ]

  result <- T.pack <$> H.readFile resultFile

  L.filter (not . T.null) (T.splitOn " " (T.lines result !! 2)) !! 2 === "1000000"
