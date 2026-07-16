{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- | One-shot on-chain UTxO discovery over the NodeToClient LocalStateQuery
-- mini-protocol.
--
-- Given the local node's socket and a set of addresses, this asks the node for
-- the UTxOs currently sitting at those addresses. The generator uses it at
-- startup to find its live funds on chain (see @discoverInitialFunds@ in
-- @Main@), which makes a restart stateless: point it at the builders'
-- destination addresses and it recovers whatever the recycling loop left there.
--
-- The query era is detected at runtime ('Api.QueryCurrentEra') rather than
-- hardcoded, so it follows the chain across the Shelley-based eras cardano-api
-- supports (Shelley through Conway today). Results are keyed by the caller's
-- own 'Api.AddressInEra' values, so it looks funds up with the very addresses
-- it passed in and never touches the era-agnostic 'Api.AddressAny' projection
-- used internally to join the node's reply back to those addresses.
module Cardano.Benchmarking.TxCentrifuge.NodeToClient.UTxOQuery
  ( queryUTxOsAtAddresses
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Exception (SomeException, try)
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

--------------------------------------------------------------------------------

-- | Ask the local node for the UTxOs sitting at the given addresses.
--
-- Returns a map from each input address to the @(TxIn, lovelace)@ pairs found
-- there (only addresses with at least one UTxO appear). Yields 'Left' on any
-- failure: the socket is unreachable, the acquire fails, or the node reports an
-- era mismatch.
--
-- The input addresses are 'Api.ConwayEra'-typed for the caller's convenience
-- (the on-the-wire bytes are era-agnostic across Shelley-based eras). The query
-- itself runs in whatever era the node reports.
queryUTxOsAtAddresses
  :: FilePath
  -- ^ NodeToClient socket path of the local node to query.
  -> Api.NetworkId
  -> [Api.AddressInEra Api.ConwayEra]
  -> IO (Either String
          (Map.Map (Api.AddressInEra Api.ConwayEra) [(Api.TxIn, Integer)])
        )
queryUTxOsAtAddresses socketPath networkId addrs = do
  let connectInfo = Api.LocalNodeConnectInfo
        { Api.localConsensusModeParams =
            Api.CardanoModeParams (Api.EpochSlots 21600)
        , Api.localNodeNetworkId       = networkId
        , Api.localNodeSocketPath      = Api.File socketPath
        }
      -- Reverse index from the era-agnostic address projection back to the
      -- caller's era-typed address. The node replies with query-era addresses,
      -- so we join on the projection to recover the address the caller passed.
      -- Its key set is also the (deduplicated) query address set.
      inputByAny = Map.fromList [ (addressInEraToAny a, a) | a <- addrs ]
  -- 'Api.queryNodeLocalState' does not catch IOExceptions, so a missing or
  -- dead socket would crash rather than return 'Left'. Wrap it so the caller
  -- always gets a clean error to report.
  result <- try (runQuery connectInfo inputByAny)
  pure $ case result of
    Left ex ->
      Left $
        "local node connection failed (is the node running and the socket "
        ++ "path correct?): " ++ show (ex :: SomeException)
    Right r -> r

-- | Detect the node's current era, then run the UTxO query in that era.
runQuery
  :: Api.LocalNodeConnectInfo
  -> Map.Map Api.AddressAny (Api.AddressInEra Api.ConwayEra)
  -> IO (Either String
           (Map.Map (Api.AddressInEra Api.ConwayEra) [(Api.TxIn, Integer)])
        )
runQuery connectInfo inputByAny = do
  eEra <- Api.runExceptT $
    Api.queryNodeLocalState connectInfo Net.VolatileTip Api.QueryCurrentEra
  case eEra of
    Left acqFailure ->
      pure $ Left $ "QueryCurrentEra acquire failed: " ++ show acqFailure
    Right (Api.AnyCardanoEra era) ->
      Api.caseByronOrShelleyBasedEra
        (pure $ Left
          "node is in the Byron era, UTxO discovery needs a Shelley-based era"
        )
        (\sbe -> queryShelleyEra connectInfo sbe inputByAny)
        era

-- | Run the @QueryUTxOByAddress@ in the given (existential) Shelley-based era
-- and reduce the result to @(TxIn, lovelace)@ pairs keyed by the caller's input
-- address. The reduction happens inside the era scope because the era cannot
-- escape the 'Api.UTxO' type.
queryShelleyEra
  :: forall era
   . Api.LocalNodeConnectInfo
  -> Api.ShelleyBasedEra era
  -> Map.Map Api.AddressAny (Api.AddressInEra Api.ConwayEra)
  -> IO (Either String
          (Map.Map (Api.AddressInEra Api.ConwayEra) [(Api.TxIn, Integer)])
        )
queryShelleyEra connectInfo sbe inputByAny = do
  let query :: Api.QueryInMode (Either Api.EraMismatch (Api.UTxO era))
      query =
        Api.QueryInEra
          (Api.QueryInShelleyBasedEra sbe
            (Api.QueryUTxO (Api.QueryUTxOByAddress (Map.keysSet inputByAny)))
          )
  eResult <- Api.runExceptT $
    Api.queryNodeLocalState connectInfo Net.VolatileTip query
  pure $ case eResult of
    Left acqFailure ->
      Left $ "UTxO query acquire failed: " ++ show acqFailure
    Right (Left eraMismatch) ->
      Left $ "UTxO query era mismatch: " ++ show eraMismatch
    Right (Right (Api.UTxO utxoMap)) ->
      Right (groupByAddress inputByAny utxoMap)

-- | Group the node's @TxIn -> TxOut@ UTxO map by the caller's input address,
-- projecting each 'Api.TxOut' to its 'Api.AddressAny' and looking that up in
-- the reverse index to recover the 'Api.ConwayEra' address the caller passed.
-- A UTxO whose address is not in the index is dropped (cannot happen for a
-- by-address query, but keeps the fold total).
groupByAddress
  :: Map.Map Api.AddressAny (Api.AddressInEra Api.ConwayEra)
  -> Map.Map Api.TxIn (Api.TxOut Api.CtxUTxO era)
  -> Map.Map (Api.AddressInEra Api.ConwayEra) [(Api.TxIn, Integer)]
groupByAddress inputByAny = Map.foldlWithKey' step Map.empty
  where
    step acc txin (Api.TxOut addr val _datum _refScript) =
      case Map.lookup (addressInEraToAny addr) inputByAny of
        Just inAddr ->
          let L.Coin amount = Api.txOutValueToLovelace val
          in Map.insertWith (++) inAddr [(txin, amount)] acc
        Nothing -> acc

-- | Erase the era index of an 'Api.AddressInEra', giving the era-agnostic
-- 'Api.AddressAny'. Used internally to join the node's query-era reply back to
-- the caller's 'Api.ConwayEra' input addresses (the on-the-wire bytes match
-- across Shelley-based eras, so the projection is a stable join key).
addressInEraToAny :: Api.AddressInEra era -> Api.AddressAny
addressInEraToAny (Api.AddressInEra _ addr) = Api.toAddressAny addr
