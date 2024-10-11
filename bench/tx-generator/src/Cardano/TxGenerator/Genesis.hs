{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use map" -}

-- | This module provides means to secure funds that are given in genesis.
--   To secure a fund, the key locking the transaction ouput in genesis has to be provided.
module Cardano.TxGenerator.Genesis
  ( genesisInitialFunds
  , genesisInitialFundForKey
  , genesisLoadDRepKeys
  , genesisLoadStakeKeys
  , genesisTxInput
  , genesisExpenditure
  , genesisSecureInitialFund
  , genesisValidate
  )
where

import           Cardano.Api
import           Cardano.Api.Shelley (ReferenceScript (..), fromShelleyPaymentCredential,
                   fromShelleyStakeReference)

import           Cardano.CLI.Types.Common (SigningKeyFile, VerificationKeyFile)
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Shelley.API (Addr (..), sgInitialFunds)
import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Setup.SigningKey (readDRepKeyFile, readStakeKeyFile)
import           Cardano.TxGenerator.Types
import           Cardano.TxGenerator.Utils
import           Ouroboros.Consensus.Shelley.Node (validateGenesis)

import           Data.Bifunctor (bimap, second)
import           Data.Char (isDigit)
import           Data.Function ((&))
import           Data.List (find, isPrefixOf, isSuffixOf)
import qualified Data.ListMap as ListMap (toList)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))


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
genesisSecureInitialFund networkId genesis srcKey destKey TxGenTxParams{txParamFee, txParamTTL}
  = case genesisInitialFundForKey @era networkId genesis srcKey of
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
  -> [(AddressInEra era, L.Coin)]
genesisInitialFunds networkId g
 = [ ( shelleyAddressInEra (shelleyBasedEra @era) $
          makeShelleyAddress networkId (fromShelleyPaymentCredential pcr) (fromShelleyStakeReference stref)
     , coin
     )
     | (Addr _ pcr stref, coin) <- ListMap.toList $ sgInitialFunds g
   ]

genesisInitialFundForKey :: forall era. IsShelleyBasedEra era
  => NetworkId
  -> ShelleyGenesis
  -> SigningKey PaymentKey
  -> Maybe (AddressInEra era, L.Coin)
genesisInitialFundForKey networkId genesis key
  = find (isTxOutForKey . fst) (genesisInitialFunds networkId genesis)
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
      (createAndValidateTransactionBody (shelleyBasedEra @era) txBodyContent)
 where
  txBodyContent = defaultTxBodyContent shelleyBasedEra
    & setTxIns (zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending)
    & setTxOuts txouts
    & setTxFee (mkTxFee fee)
    & setTxValidityLowerBound TxValidityNoLowerBound
    & setTxValidityUpperBound (mkTxValidityUpperBound ttl)

castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
castKey (PaymentSigningKey skey) = GenesisUTxOSigningKey skey

-- | This function assumes a directory structure as created by
--   cardano-cli's create-testnet-data command.
genesisLoadDRepKeys :: FilePath -> IO (Either TxGenError [SigningKey DRepKey])
genesisLoadDRepKeys genesisDir = runExceptT $ do
    dirContents <- handleIOExceptT IOError (listDirectory drepDir)
    let subDirs = filter dirWellFormed dirContents
    mapM loadFromDir ((drepDir </>) <$> subDirs)
  where
    asSigningKeyFile :: FilePath -> SigningKeyFile In
    asSigningKeyFile = File

    loadFromDir d = hoistEither =<< handleIOExceptT IOError
        (readDRepKeyFile $ asSigningKeyFile (d </> "drep.skey"))

    dirWellFormed = \case
      'd':'r':'e':'p' : nr@(_:_)  -> all isDigit nr
      _                           -> False

    drepDir = genesisDir </> "drep-keys"

genesisLoadStakeKeys :: FilePath -> IO (Either TxGenError [VerificationKey StakeKey])
genesisLoadStakeKeys genesisDir = runExceptT $ do
    dirContents <- handleIOExceptT IOError (listDirectory poolsDir)
    let fs = filter (\f -> "staking-reward" `isPrefixOf` f && ".vkey" `isSuffixOf` f) dirContents
    mapM loadFile fs
  where
    asVerificationKeyFile :: FilePath -> VerificationKeyFile In
    asVerificationKeyFile = File

    loadFile f = hoistEither =<< handleIOExceptT IOError
      (readStakeKeyFile $ asVerificationKeyFile $ poolsDir </> f)

    poolsDir = genesisDir </> "pools"
