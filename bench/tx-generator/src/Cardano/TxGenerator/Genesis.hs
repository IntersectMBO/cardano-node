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
  , genesisWithResolvedInitialFunds
  , genesisTxInput
  , genesisExpenditure
  , genesisSecureInitialFund
  , genesisValidate
  )
where

import           Cardano.Api hiding (ShelleyGenesis)

import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Shelley.API (Addr (..))
import           Cardano.Ledger.Shelley.Genesis (InjectionData (..), ShelleyExtraConfig (..),
                   foldInjectionData)
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils
import           Ouroboros.Consensus.Shelley.Node (validateGenesis)

import           Control.Exception (SomeException, displayException, evaluate, try)
import           Data.Bifunctor (bimap, second)
import           Data.Function ((&))
import           Data.List (find)
import qualified Data.ListMap as ListMap (fromList, toList)
import           System.FS.API.Types (MountPoint (..))
import           System.FS.IO (ioHasFS)


genesisValidate ::  ShelleyGenesis -> Either String ()
genesisValidate
  = validateGenesis

genesisSecureInitialFund :: forall era. IsShelleyBasedEra era =>
     NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> SigningKey PaymentKey
  -> TxGenTxParams
  -> Either TxGenError (Tx era, Fund)
genesisSecureInitialFund networkId genesis srcKey destKey TxGenTxParams{txParamFee, txParamTTL} = do
  mFund <- genesisInitialFundForKey @era networkId genesis srcKey
  case mFund of
    Nothing             -> Left $ TxGenError "genesisSecureInitialFund: no fund found for given key in genesis"
    Just (_, lovelace)  ->
      let
        txOutValue :: TxOutValue era
        txOutValue = lovelaceToTxOutValue (shelleyBasedEra @era) $ lovelace - txParamFee
      in genesisExpenditure networkId srcKey destAddr txOutValue txParamFee txParamTTL destKey
  where
    destAddr = keyAddress @era networkId destKey

genesisInitialFunds :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis
  -> Either TxGenError [(AddressInEra era, L.Coin)]
genesisInitialFunds networkId g = do
  funds <- embeddedInitialFunds g
  pure
    [ ( shelleyAddressInEra (shelleyBasedEra @era) $
          makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
      , coin
      )
    | (Addr _ pcr stref, coin) <- funds
    ]

-- | 'embeddedInitialFunds' resolves the genesis initial funds, accepting both the
-- legacy top-level @initialFunds@ and the newer @extraConfig.initialFunds@
-- injection that @cardano-cli genesis create-testnet-data@ emits.
embeddedInitialFunds :: ShelleyGenesis -> Either TxGenError [(Addr, L.Coin)]
embeddedInitialFunds g =
  case sgExtraConfig g of
    SNothing -> Right legacy
    SJust extraConfig -> case secInitialFunds extraConfig of
      NoInjection          -> Right legacy
      _ | not (null legacy) ->
          Left $ TxGenError "genesisInitialFunds: both initialFunds and extraConfig.initialFunds are populated; please use only one source"
      EmbeddedInjection lm -> Right (ListMap.toList lm)
      InjectionFromFile{}  ->
        Left $ TxGenError "genesisInitialFunds: unresolved file-based initial-funds injection; use genesisWithResolvedInitialFunds"
 where
  legacy = ListMap.toList $ sgInitialFunds g

-- | 'genesisWithResolvedInitialFunds' runs a pure computation on the genesis with a
-- file-based @extraConfig.initialFunds@ injection resolved into an embedded
-- one, so the pure functions in this module can consume it unchanged. Any other
-- genesis is passed to the computation as is.
--
-- The first argument is the directory containing the Shelley genesis file; the
-- node mounts its 'System.FS.API.HasFS' for injection files at that same
-- directory, and reading, hashing and parsing are delegated to ledger's
-- 'foldInjectionData', so resolution matches the node's.
--
-- The resolved genesis never escapes: the caller's genesis keeps its original
-- field, and the result is forced to WHNF before returning, so the (potentially
-- huge) resolved funds become garbage right after. However, if the result @a@
-- keeps a reference to the resolved genesis or its funds -- directly, or via a
-- thunk not yet evaluated -- they stay on the heap for as long as @a@ is
-- reachable; e.g. throughout the process lifetime when @a@ is stored in a
-- long-lived environment.
genesisWithResolvedInitialFunds :: FilePath -> ShelleyGenesis -> (ShelleyGenesis -> Either TxGenError a) -> IO (Either TxGenError a)
genesisWithResolvedInitialFunds shelleyGenesisDir g k =
  case sgExtraConfig g of
    SJust extraConfig | source@InjectionFromFile{} <- secInitialFunds extraConfig -> do
      let hasFS = ioHasFS (MountPoint shelleyGenesisDir)
      res <- try @SomeException $ foldInjectionData hasFS source (flip (:)) []
      case res of
        Left err    -> pure $ Left $ TxGenError $ "genesisWithResolvedInitialFunds: " ++ displayException err
        Right funds ->
          let embedded = EmbeddedInjection $ ListMap.fromList $ reverse funds
          in evaluate $ k g { sgExtraConfig = SJust extraConfig { secInitialFunds = embedded } }
    _ -> evaluate (k g)

genesisInitialFundForKey :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> Either TxGenError (Maybe (AddressInEra era, L.Coin))
genesisInitialFundForKey networkId genesis key
  = find (isTxOutForKey . fst) <$> genesisInitialFunds networkId genesis
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
