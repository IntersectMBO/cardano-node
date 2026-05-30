{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

-- | One-shot @QueryUTxOByAddress@ via the N2C @LocalStateQuery@ mini-protocol.
-- Used at startup to discover the live UTxO set at the recycle addresses, so
-- the service can resume cleanly after a restart without any on-disk state.
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

-- | Query the local node for all UTxOs at the given Conway-era addresses.
--
-- Returns @(txin, lovelace)@ pairs grouped by their address. Addresses with
-- no UTxOs are omitted from the result; callers should treat absence as zero.
--
-- Hard-coded to 'Api.ConwayEra' to match the rest of tx-centrifuge.
queryUTxOsAtAddresses
  :: FilePath
  -- ^ Path to the local node's N2C socket.
  -> Api.NetworkId
  -> [Api.AddressInEra Api.ConwayEra]
  -> IO (Either String
          (Map.Map (Api.AddressInEra Api.ConwayEra) [(Api.TxIn, Integer)]))
queryUTxOsAtAddresses socketPath networkId addrs = do
  let connectInfo =
        Api.LocalNodeConnectInfo
          { Api.localConsensusModeParams = Api.CardanoModeParams (Api.EpochSlots 21600)
          , Api.localNodeNetworkId = networkId
          , Api.localNodeSocketPath = Api.File socketPath
          }
      addrSet = Set.fromList (map addressInEraToAny addrs)
      query =
        Api.QueryInEra
          (Api.QueryInShelleyBasedEra Api.ShelleyBasedEraConway
            (Api.QueryUTxO (Api.QueryUTxOByAddress addrSet)))
  eUtxoOrErr <- runExceptT $
    Api.queryNodeLocalState connectInfo Net.VolatileTip query
  pure $ case eUtxoOrErr of
    Left acqFailure ->
      Left $ "LocalStateQuery acquire failed: " ++ show acqFailure
    Right (Left eraMismatch) ->
      Left $ "Era mismatch (node is not in Conway era): " ++ show eraMismatch
    Right (Right (Api.UTxO utxoMap)) ->
      Right $ groupByAddress utxoMap

--------------------------------------------------------------------------------

-- | Bucket UTxOs by their destination address. The returned association list
-- preserves the original address-in-era wrapper so callers can match on the
-- exact address they queried for.
groupByAddress
  :: Map.Map Api.TxIn (Api.TxOut Api.CtxUTxO Api.ConwayEra)
  -> Map.Map (Api.AddressInEra Api.ConwayEra) [(Api.TxIn, Integer)]
groupByAddress = Map.foldlWithKey' step Map.empty
  where
    step acc txin (Api.TxOut addr val _datum _refScript) =
      let L.Coin amount = Api.txOutValueToLovelace val
      in Map.insertWith (++) addr [(txin, amount)] acc

-- | Erase the era index of an 'AddressInEra' to obtain an 'AddressAny',
-- which is what 'QueryUTxOByAddress' expects.
addressInEraToAny :: Api.AddressInEra era -> Api.AddressAny
addressInEraToAny (Api.AddressInEra _ addr) = Api.toAddressAny addr
