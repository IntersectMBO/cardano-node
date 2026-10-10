{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use map with tuple-section" -}

-- | This module provides means to secure funds that are given in genesis.
--   To secure a fund, the key locking the transaction output in genesis has to be provided.
module Cardano.TxGenerator.Genesis
  ( genesisInitialFunds
  , genesisInitialFundForKey
  , genesisTxInput
  , genesisExpenditure
  , genesisSecureInitialFund
  , genesisValidate
  )
where

import           Cardano.Api hiding (ShelleyGenesis)

import           Cardano.Crypto.Hash (Blake2b_256)
import qualified Cardano.Crypto.Hash.Class as Hash
import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Shelley.API (Addr (..))
import           Cardano.Ledger.Shelley.Genesis (InjectionData (..), ShelleyExtraConfig (..))
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils
import           Ouroboros.Consensus.Shelley.Node (validateGenesis)

import           Control.Exception (IOException, displayException, try)
import           Data.Aeson (eitherDecodeStrict')
import           Data.Bifunctor (bimap, second)
import qualified Data.ByteString as BS
import           Data.Function ((&))
import           Data.List (find)
import qualified Data.ListMap as ListMap (toList)
import           System.FilePath ((</>))


genesisValidate ::  ShelleyGenesis -> Either String ()
genesisValidate
  = validateGenesis

genesisSecureInitialFund :: forall era. IsShelleyBasedEra era =>
     FilePath
  -> ShelleyGenesis
  -> NetworkId
  -> SigningKey PaymentKey
  -> SigningKey PaymentKey
  -> TxGenTxParams
  -> IO (Either TxGenError (Tx era, Fund))
genesisSecureInitialFund shelleyGenesisDir shelleyGenesis networkId srcKey destKey TxGenTxParams{txParamFee, txParamTTL} = do
  eitherMaybeFund <- genesisInitialFundForKey @era shelleyGenesisDir shelleyGenesis networkId srcKey
  pure $ do
    maybeFund <- eitherMaybeFund
    case maybeFund of
      Nothing             -> Left $ TxGenError "genesisSecureInitialFund: no fund found for given key in genesis"
      Just (_, lovelace)  ->
        let
          txOutValue :: TxOutValue era
          txOutValue = lovelaceToTxOutValue (shelleyBasedEra @era) $ lovelace - txParamFee
        in genesisExpenditure networkId srcKey destAddr txOutValue txParamFee txParamTTL destKey
  where
    destAddr = keyAddress @era networkId destKey

genesisInitialFunds :: forall era. IsShelleyBasedEra era
  => FilePath
  -> ShelleyGenesis
  -> NetworkId
  -> IO (Either TxGenError [(AddressInEra era, L.Coin)])
genesisInitialFunds shelleyGenesisDir shelleyGenesis networkId = do
  eitherFunds <- embeddedInitialFunds shelleyGenesisDir shelleyGenesis
  pure $ do
    funds <- eitherFunds
    pure
      [ ( shelleyAddressInEra (shelleyBasedEra @era) $
            makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
        , coin
        )
      | (Addr _ pcr stref, coin) <- funds
      ]

-- | Resolves the genesis initial funds, accepting the legacy top-level
-- @initialFunds@, the inline @extraConfig.initialFunds.data@ injection and the
-- external-file @extraConfig.initialFunds.file@ injection that @cardano-cli
-- genesis create-testnet-data@ can emit.
-- tx-generator loads the genesis file eagerly unlike the node that streams via
-- cardano-ledger's 'Cardano.Ledger.Shelley.Genesis.foldInjectionData'/'HasFS'.
embeddedInitialFunds :: FilePath -> ShelleyGenesis -> IO (Either TxGenError [(Addr, L.Coin)])
embeddedInitialFunds shelleyGenesisDir shelleyGenesis =
  case sgExtraConfig shelleyGenesis of
    SNothing -> pure $ Right legacy
    SJust extraConfig -> case secInitialFunds extraConfig of
      NoInjection          -> pure $ Right legacy
      _ | not (null legacy) ->
          pure $ Left $ TxGenError "genesisInitialFunds: both initialFunds and extraConfig.initialFunds are populated; please use only one source"
      EmbeddedInjection lm -> pure $ Right (ListMap.toList lm)
      InjectionFromFile fsPath expectedHash -> do
        -- @shelleyGenesisDir@ must be the directory containing the Shelley
        -- genesis file, the prefix the "file" field resolves against.
        -- `fsPath` is of type `FsPath` from package "fs-api".
        -- We rely `show` rather than depend on the entire package.
        let path = shelleyGenesisDir </> show fsPath
        readResult <- try @IOException (BS.readFile path)
        pure $ case readResult of
          Left ioErr -> Left $ TxGenError $
            "genesisInitialFunds: failed to read injection file " ++ path ++ ": " ++ displayException ioErr
          Right content ->
            -- A file injection is read whole, hash-verified (Blake2b-256,
            -- matching what ledger checks) and aeson-decoded directly.
            let actualHash = Hash.hashWith id content :: Hash.Hash Blake2b_256 BS.ByteString
            in if actualHash /= expectedHash
               then Left $ TxGenError $ "genesisInitialFunds: hash mismatch for injection file " ++ path
               else bimap
                      (\err -> TxGenError $ "genesisInitialFunds: failed to parse injection file " ++ path ++ ": " ++ err)
                      ListMap.toList
                      (eitherDecodeStrict' content)
 where
  legacy = ListMap.toList $ sgInitialFunds shelleyGenesis

genesisInitialFundForKey :: forall era. IsShelleyBasedEra era
  => FilePath
  -> ShelleyGenesis
  -> NetworkId
  -> SigningKey PaymentKey
  -> IO (Either TxGenError (Maybe (AddressInEra era, L.Coin)))
genesisInitialFundForKey shelleyGenesisDir shelleyGenesis networkId key = do
  eitherFunds <- genesisInitialFunds shelleyGenesisDir shelleyGenesis networkId
  pure $ find (isTxOutForKey . fst) <$> eitherFunds
 where
  isTxOutForKey = (keyAddress networkId key ==)

genesisTxInput ::
     NetworkId
  -> SigningKey PaymentKey
  -> TxIn
genesisTxInput networkId
 = genesisUTxOPseudoTxIn networkId
    . verificationKeyHash
    . getVerificationKey
    . castKey

genesisExpenditure ::
     IsShelleyBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> TxOutValue era
  -> L.Coin
  -> SlotNo
  -> SigningKey PaymentKey
  -> Either TxGenError (Tx era, Fund)
genesisExpenditure networkId inputKey addr value fee ttl outputKey
  = second (\tx -> (tx, Fund $ InAnyCardanoEra cardanoEra $ fund tx)) eTx
 where
  eTx         = mkGenesisTransaction (castKey inputKey) ttl fee [pseudoTxIn] [txout]
  txout       = TxOut addr value TxOutDatumNone ReferenceScriptNone
  pseudoTxIn  = genesisTxInput networkId inputKey

  fund tx = FundInEra {
    _fundTxIn = TxIn (getTxId $ getTxBody tx) (TxIx 0)
  , _fundWitness = KeyWitness KeyWitnessForSpending
  , _fundVal  = value
  , _fundSigningKey = Just outputKey
  }

mkGenesisTransaction :: forall era .
     IsShelleyBasedEra era
  => SigningKey GenesisUTxOKey
  -> SlotNo
  -> L.Coin
  -> [TxIn]
  -> [TxOut CtxTx era]
  -> Either TxGenError (Tx era)
mkGenesisTransaction key ttl fee txins txouts
  = bimap
      ApiError
      (\b -> signShelleyTransaction (shelleyBasedEra @era) b [WitnessGenesisUTxOKey key])
      (createTransactionBody (shelleyBasedEra @era) txBodyContent)
 where
  txBodyContent = defaultTxBodyContent shelleyBasedEra
    & setTxIns (zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending)
    & setTxOuts txouts
    & setTxFee (mkTxFee fee)
    & setTxValidityLowerBound TxValidityNoLowerBound
    & setTxValidityUpperBound (mkTxValidityUpperBound ttl)

castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
castKey (PaymentSigningKey skey) = GenesisUTxOSigningKey skey
