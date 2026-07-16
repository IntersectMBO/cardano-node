{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.TxCentrifuge.Fund
  ( -- * A fund.
    Fund (..)
    -- * Creating funds.
  , loadFunds
  , discoverFunds
    -- * Utils.
  , genesisTxIn
  , deriveAddress
  , readSigningKey
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Data.Bifunctor (first)
import Data.IORef qualified as IORef
import Text.Read (readMaybe)
-----------
-- aeson --
-----------
import Data.Aeson qualified as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types qualified as Aeson
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
----------
-- text --
----------
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.NodeToClient.UTxOQuery qualified as UTxOQuery

--------------------------------------------------------------------------------
-- A fund.
--------------------------------------------------------------------------------

-- | A spendable fund: a UTxO reference, its Lovelace value, and the signing key
-- required to spend it.
data Fund = Fund
  { fundTxIn :: !Api.TxIn
    -- | Lovelace amount.
  , fundValue :: !Integer
    -- | Key to spend this UTxO.
  , fundSignKey :: !(Api.SigningKey Api.PaymentKey)
  }

--------------------------------------------------------------------------------
-- Creating funds.
--------------------------------------------------------------------------------

-- | Internal: JSON-parseable fund entry. Two variants:
--
-- * 'FundEntryPayment': a regular fund with an explicit UTxO reference.
--   @{ "tx_in": "txid#ix", "value": 1000000, "signing_key": "payment.skey" }@
--
-- * 'FundEntryGenesis': a genesis UTxO fund identified only by its key.
--   The TxIn is derived via 'Api.genesisUTxOPseudoTxIn' (always TxIx 0).
--   @{ "signing_key": "genesis.skey", "value": 1000000 }@
data FundEntry
  = FundEntryPayment !Api.TxIn !Integer !FilePath
  | FundEntryGenesis !Integer !FilePath

instance Aeson.FromJSON FundEntry where
  parseJSON = Aeson.withObject "Fund" $ \o -> do
    -- Common fields.
    val       <- o .:  "value"
    keyPath   <- o .:  "signing_key"
    -- If it has a "tx_in" field it is a 'FundEntryPayment'.
    mbTxInStr <- o .:? "tx_in"
    case mbTxInStr of
      Just txInStr -> do
        txIn <- parseTxIn txInStr
        pure (FundEntryPayment txIn val keyPath)
      Nothing -> do
        pure (FundEntryGenesis      val keyPath)

-- | Parse @"txid#ix"@ format. Both parts are required.
parseTxIn :: T.Text -> Aeson.Parser Api.TxIn
parseTxIn text =
  let (txIdHex, rest) = T.breakOn "#" text
  in case T.uncons rest of
    Just ('#', ds) ->
      case Api.deserialiseFromRawBytesHex @Api.TxId (T.encodeUtf8 txIdHex) of
        Left err   -> fail $ "Invalid TxId: " ++ show err
        Right txId -> case readMaybe (T.unpack ds) of
          Nothing -> fail $ "Invalid TxIx: expected an integer, got " ++ show ds
          Just ix -> pure $ Api.TxIn txId (Api.TxIx ix)
    _ -> fail "Invalid TxIn: expected \"txid#ix\" format"

-- | Load funds from a JSON file and return them as a list.
-- The JSON file should contain an array of fund objects, each with a
-- @"signing_key"@ field pointing to a @.skey@ file.
-- Signing keys are cached by path to avoid redundant disk reads.
--
-- For key-only entries (no @"txIn"@), the genesis UTxO pseudo-TxIn is derived
-- from the signing key using the provided 'Api.NetworkId'.
--
-- NOTE: the entire JSON array is decoded into memory before returning.
-- For very large fund files a streaming parser (e.g. json-stream) could yield
-- funds incrementally so the caller can start filling queues before the file
-- is fully read.
loadFunds :: Api.NetworkId -> FilePath -> IO (Either String [Fund])
loadFunds networkId path = do
  result <- Aeson.eitherDecodeFileStrict' path
  case result of
    Left err -> pure (Left err)
    Right (entries :: [FundEntry]) -> do
      keyCache <- IORef.newIORef Map.empty
      eFunds <- mapM (entryToFund networkId keyCache) entries
      case sequence eFunds of
        Left err -> pure (Left err)
        Right funds -> pure (Right funds)

-- | Convert a JSON entry to a Fund by loading its signing key (cached).
entryToFund
  :: Api.NetworkId
  -> IORef.IORef (Map.Map FilePath (Api.SigningKey Api.PaymentKey))
  -> FundEntry
  -> IO (Either String Fund)
entryToFund networkId cacheRef entry = do
  let keyPath = entryKeyPath entry
  cache <- IORef.readIORef cacheRef
  case Map.lookup keyPath cache of
    Just key -> pure $ Right $ mkFund key
    Nothing  -> do
      eKey <- readSigningKey keyPath
      case eKey of
        Left err  -> pure $ Left $
          "Failed to load signing key "
          ++ keyPath ++ ": " ++ err
        Right key -> do
          IORef.modifyIORef' cacheRef (Map.insert keyPath key)
          pure $ Right $ mkFund key
  where

    entryKeyPath :: FundEntry -> FilePath
    entryKeyPath (FundEntryPayment _ _ p) = p
    entryKeyPath (FundEntryGenesis   _ p) = p

    mkFund :: Api.SigningKey Api.PaymentKey -> Fund
    mkFund key = case entry of
      FundEntryPayment txIn val _ -> Fund txIn val key
      FundEntryGenesis      val _ -> Fund (genesisTxIn networkId key) val key

-- | Discover starting funds on chain. For each signing key, derive the address
-- it controls, ask the local node for the UTxOs there, and return one 'Fund'
-- per UTxO, tagged with the key that owns it so the spending transaction can
-- always be signed. Mirrors 'loadFunds': 'Right' with the (possibly empty)
-- funds, or 'Left' if a key cannot be read or the query fails.
--
-- Addresses are deduplicated first: two key files can control the same address,
-- and the node returns each UTxO once, so attributing a UTxO to more than one
-- key would build duplicate references that double spend the same output.
--
-- Point the keys at each builder's destination address (see
-- 'destination_signing_key'): the recycling loop keeps a builder's outputs
-- there, so discovery re-picks-up exactly the funds a previous run left,
-- making restart stateless.
discoverFunds
  :: Api.NetworkId
  -> FilePath   -- ^ NodeToClient socket path of the local node to query.
  -> [FilePath] -- ^ Signing key files to discover funds under.
  -> IO (Either String [Fund])
discoverFunds networkId socketPath keyPaths = do
  -- Read each key and derive the address it controls.
  eKeyAddrs <- mapM readKeyAddr keyPaths
  case sequence eKeyAddrs of
    Left err        -> pure (Left err)
    Right keyAddrs0 -> do
      -- Deduplicate by address so each on-chain UTxO is attributed to exactly
      -- one key. Keys deriving the same address share a key hash, so any one of
      -- them signs for it.
      let keyAddrs =
            Map.elems $ Map.fromList
              [ (addr, (skey, addr)) | (skey, addr) <- keyAddrs0 ]
      eUtxos <-
        UTxOQuery.queryUTxOsAtAddresses socketPath networkId (map snd keyAddrs)
      pure $ case eUtxos of
        Left err          -> Left err
        Right utxosByAddr -> Right
          [ Fund { fundTxIn = txin, fundValue = val, fundSignKey = skey }
          | (skey, addr) <- keyAddrs
          , (txin, val)  <- Map.findWithDefault [] addr utxosByAddr
          ]
  where
    readKeyAddr path = do
      eKey <- readSigningKey path
      pure $ case eKey of
        Left e     -> Left $ "signing key (" ++ path ++ "): " ++ e
        Right skey -> Right (skey, deriveAddress networkId skey)

--------------------------------------------------------------------------------
-- Utils.
--------------------------------------------------------------------------------

-- | Derive the genesis UTxO pseudo-TxIn from a payment signing key.
-- Casts to 'Api.GenesisUTxOKey' to compute the key hash expected by
-- 'Api.genesisUTxOPseudoTxIn'.
genesisTxIn :: Api.NetworkId -> Api.SigningKey Api.PaymentKey -> Api.TxIn
genesisTxIn networkId
  = Api.genesisUTxOPseudoTxIn networkId
  . Api.verificationKeyHash
  . Api.getVerificationKey
  . castToGenesisUTxOKey

-- | Cast a 'Api.PaymentKey' signing key to a 'Api.GenesisUTxOKey' signing key.
-- Both key types use the same underlying ed25519 representation; this cast
-- enables computing the genesis UTxO pseudo-TxIn via
-- 'Api.genesisUTxOPseudoTxIn'.
castToGenesisUTxOKey
  :: Api.SigningKey Api.PaymentKey
  -> Api.SigningKey Api.GenesisUTxOKey
castToGenesisUTxOKey (Api.PaymentSigningKey skey) =
  Api.GenesisUTxOSigningKey skey

-- | Derive the enterprise (no-stake) Shelley address controlled by a payment
-- signing key under the given network.
deriveAddress
  :: Api.NetworkId
  -> Api.SigningKey Api.PaymentKey
  -> Api.AddressInEra Api.ConwayEra
deriveAddress networkId signingKey =
  Api.shelleyAddressInEra
    (Api.shelleyBasedEra @Api.ConwayEra) $
  Api.makeShelleyAddress networkId
    (Api.PaymentCredentialByKey
      (Api.verificationKeyHash
        (Api.getVerificationKey signingKey)))
    Api.NoStakeAddress

-- | Read a signing key from a text envelope file.
-- Accepts both @PaymentSigningKey_ed25519@ and
-- @GenesisUTxOSigningKey_ed25519@ key types.
-- Genesis UTxO keys are cast to payment keys.
readSigningKey :: FilePath -> IO (Either String (Api.SigningKey Api.PaymentKey))
readSigningKey fp = do
  result <- Api.readFileTextEnvelopeAnyOf
    [ Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey) id
    , Api.FromSomeType
        (Api.AsSigningKey Api.AsGenesisUTxOKey)
        Api.castSigningKey
    ]
    (Api.File fp)
  pure $ first show result
