{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Testnet.Test.Api.TxSupplementalDatum
  ( hprop_tx_supp_datum
  )
where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Network as Net
import           Cardano.Api.Shelley

import           Cardano.CLI.Type.Common
import           Cardano.Crypto.Hash.Class (hashToStringAsHex)
import qualified Cardano.Ledger.Core as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Default.Class
import qualified Data.Text as Text
import           GHC.Exts (IsList (..))
import           Lens.Micro
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Configuration
import           Testnet.Components.Query
import           Testnet.Components.Query (findLargestUtxoWithAddress, findUtxosWithAddress,
                   getEpochStateView, waitForBlocks)
import           Testnet.Process.Run
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Types

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.TestWatchdog as H

hprop_tx_supp_datum :: Property
hprop_tx_supp_datum = integrationRetryWorkspace 2 "api-tx-supp-dat" $ \tempAbsBasePath' -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let ceo = ConwayEraOnwardsConway
      sbe = convert ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
      options = def{cardanoNodeEra = AnyShelleyBasedEra sbe}

  tr@TestnetRuntime
    { configurationFile
    , testnetMagic
    , testnetNodes=node0:_
    , wallets = wallet0 : _
    } <-
    cardanoTestnetDefault options def conf

  systemStart <- fmap SystemStart . H.noteShowM $ getStartTime tempAbsPath' tr
  epochStateView <- getEpochStateView configurationFile (nodeSocketPath node0)
  connectionInfo <- node0ConnectionInfo tr
  pparams <- getProtocolParams epochStateView ceo

  eraHistory <- (H.leftFail <=< H.leftFailM) . H.evalIO $ executeLocalStateQueryExpr connectionInfo Net.VolatileTip $
    queryEraHistory
  let epochInfo = toLedgerEpochInfo eraHistory

  H.failure

  let witnesses = undefined

  let content =
        defaultTxBodyContent sbe
          & setTxIns undefined
          & setTxOuts undefined
          & setTxProtocolParams (pure $ pure pparams)

  BalancedTxBody _ txBody _ _ <-
    H.leftFail $
      makeTransactionBodyAutoBalance
        sbe
        systemStart
        epochInfo
        pparams
        mempty
        mempty
        mempty
        undefined -- UTXO
        content
        undefined -- changeaddr
        Nothing -- keys override

  let tx = makeSignedTransaction witnesses txBody

  submitTxToNodeLocal connectionInfo (TxInMode sbe tx)

  H.failure
