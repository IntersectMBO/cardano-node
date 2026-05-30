{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- | One-shot @QueryUTxOByAddress@ via the N2C @LocalStateQuery@ mini-protocol.
-- Used at startup to discover the live UTxO set at the recycle addresses, so
-- the service can resume cleanly after a restart without any on-disk state.
--
-- Era handling: the era is detected via @QueryCurrentEra@ at runtime and the
-- UTxO query is issued in that era. Tx-centrifuge itself uses ConwayEra for
-- tx building, but the chain may be in a later era (e.g. Dijkstra) — issuing
-- a Conway-typed query against a Dijkstra-era node fails with @anachrony@
-- on the consensus side. Detecting and matching the era avoids that.
module Cardano.Benchmarking.TxCentrifuge.UTxOQuery
  ( queryUTxOsAtAddresses
  ) where

--------------------------------------------------------------------------------

-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
import Cardano.Api.Network qualified as Net
-------------------------
-- cardano-ledger-core --
-------------------------
import Cardano.Ledger.Coin qualified as L
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
------------------
-- transformers --
------------------
import Control.Monad.Trans.Except (runExceptT)

--------------------------------------------------------------------------------

-- | Query the local node for all UTxOs at the given addresses, in whatever
-- era the chain is currently in. Returns @(txin, lovelace)@ pairs grouped by
-- their owning address as 'Api.AddressAny' (era-agnostic). Addresses with no
-- UTxOs are omitted; callers should treat absence as zero.
queryUTxOsAtAddresses
  :: FilePath
  -- ^ Path to the local node's N2C socket.
  -> Api.NetworkId
  -> [Api.AddressInEra Api.ConwayEra]
  -- ^ Addresses to query. We accept Conway-typed addresses for caller
  -- convenience (tx-centrifuge derives them as such) but the underlying
  -- Shelley address bytes are era-agnostic, so the query works regardless
  -- of which Shelley-based era the chain is in.
  -> IO (Either String (Map.Map Api.AddressAny [(Api.TxIn, Integer)]))
queryUTxOsAtAddresses socketPath networkId addrs = do
  let connectInfo =
        Api.LocalNodeConnectInfo
          { Api.localConsensusModeParams = Api.CardanoModeParams (Api.EpochSlots 21600)
          , Api.localNodeNetworkId = networkId
          , Api.localNodeSocketPath = Api.File socketPath
          }
      addrSet = Set.fromList (map addressInEraToAny addrs)

  eEra <- runExceptT $
    Api.queryNodeLocalState connectInfo Net.VolatileTip Api.QueryCurrentEra
  case eEra of
    Left acqFailure ->
      pure $ Left $ "QueryCurrentEra acquire failed: " ++ show acqFailure
    Right (Api.AnyCardanoEra era) ->
      Api.caseByronOrShelleyBasedEra
        (pure $ Left
          "Node is in Byron era; UTxO discovery requires a Shelley-based era")
        (\sbe -> queryShelleyEra connectInfo sbe addrSet)
        era

--------------------------------------------------------------------------------

-- | Issue the UTxO query in the supplied Shelley-based era and convert the
-- result to an era-agnostic @Map AddressAny [(TxIn, Integer)]@.
queryShelleyEra
  :: forall era
   . Api.LocalNodeConnectInfo
  -> Api.ShelleyBasedEra era
  -> Set.Set Api.AddressAny
  -> IO (Either String (Map.Map Api.AddressAny [(Api.TxIn, Integer)]))
queryShelleyEra connectInfo sbe addrSet = do
  let query :: Api.QueryInMode (Either Api.EraMismatch (Api.UTxO era))
      query =
        Api.QueryInEra
          (Api.QueryInShelleyBasedEra sbe
            (Api.QueryUTxO (Api.QueryUTxOByAddress addrSet)))
  eResult <- runExceptT $
    Api.queryNodeLocalState connectInfo Net.VolatileTip query
  pure $ case eResult of
    Left acqFailure ->
      Left $ "LocalStateQuery acquire failed: " ++ show acqFailure
    Right (Left eraMismatch) ->
      Left $ "Era mismatch in UTxO query: " ++ show eraMismatch
    Right (Right (Api.UTxO utxoMap)) ->
      Right $ groupByAddressAny utxoMap

--------------------------------------------------------------------------------

-- | Bucket UTxOs by their destination address, projecting the era-tagged
-- 'AddressInEra' to era-agnostic 'AddressAny' so callers can match without
-- era acrobatics.
groupByAddressAny
  :: Map.Map Api.TxIn (Api.TxOut Api.CtxUTxO era)
  -> Map.Map Api.AddressAny [(Api.TxIn, Integer)]
groupByAddressAny = Map.foldlWithKey' step Map.empty
  where
    step acc txin (Api.TxOut addr val _datum _refScript) =
      let L.Coin amount = Api.txOutValueToLovelace val
          addrAny = addressInEraToAny addr
      in Map.insertWith (++) addrAny [(txin, amount)] acc

-- | Erase the era index of an 'AddressInEra' to obtain an 'AddressAny',
-- which is what 'QueryUTxOByAddress' expects and what we use for grouping.
addressInEraToAny :: Api.AddressInEra era -> Api.AddressAny
addressInEraToAny (Api.AddressInEra _ addr) = Api.toAddressAny addr
