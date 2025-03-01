{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Testnet.PlutusExample.PlutusVersion1.RedeemerContextScripts
  ( PV1CustomRedeemer(..)
  , pv1CustomRedeemerFromScriptData
  , scriptContextTestMintingScript
  , scriptContextTextPayingScript
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified PlutusLedgerApi.V1 as Plutus

import           Prelude hiding (($))

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import           Plutus.Script.Utils.Typed as Scripts
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AMap
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))
import qualified PlutusTx.Prelude as P

-- Description
-- PV1CustomRedeemer mimics the ScriptContext. PV1CustomRedeemer is built via reading
-- the transaction containing the script and the script itself just compares PV1CustomRedeemer
-- to the ScriptContext to be sure they are equivalent.
-- The overall aim is to make sure what is provided via ScriptContext (i.e. the transaction)
-- is what it's supposed to be. We check this by creating PV1CustomRedeemer based on
-- the actual transaction which is created via the create-script-context executable.

newtype MyCustomDatum = MyCustomDatum Integer

data PV1CustomRedeemer
  = PV1CustomRedeemer
      { mCrOutputs       :: [Plutus.TxOut]
      , mCrInputs        :: [Plutus.TxInInfo]
      , mCrMint          :: Plutus.Value
      , mCrValidRange    :: Plutus.POSIXTimeRange
      , mCrFee           :: Plutus.Value
      , mCrDatums        :: [(Plutus.DatumHash, Plutus.Datum)]
      , mCrCerts         :: [Plutus.DCert]
      , mCrSignatories   :: [Plutus.PubKeyHash]
      , mCrScriptPurpose :: Maybe Plutus.ScriptPurpose
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''MyCustomDatum
PlutusTx.unstableMakeIsData ''PV1CustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum-> PV1CustomRedeemer -> Plutus.ScriptContext -> Bool
mkValidator _datum (PV1CustomRedeemer txouts txins minted txValidRange _fee datumsAndHashes certs signatories mPurpose) scriptContext =
  -- Minted field is equivalent
  Plutus.txInfoMint txInfo P.== minted P.&&
  -- Validity range is equivalent
  Plutus.txInfoValidRange txInfo P.== txValidRange P.&&
  -- Datums and datum hashes are equivalent
  Plutus.txInfoData txInfo P.== datumsAndHashes P.&&
  -- Required tx signers are equivalent
  Plutus.txInfoSignatories txInfo P.== signatories P.&&
  -- Payment tx out is equivalent
  AMap.member paymentOutputFromRedeemer scriptContextOutputsMap P.&&
  -- Txins are equivalent
  (AMap.member txinA scriptContextTxinsMap P.&& AMap.member txinB scriptContextTxinsMap) P.&&
  -- Check if tx inputs are equivalent
  AMap.member singleRedeemerCert scriptContextCertsMap P.&&
  -- Check if the script purposes are equivalent
  case mPurpose of
    Just sPurp -> sPurp P.== sPurpose
    Nothing    -> PlutusTx.Prelude.error ()
 where
   scriptContextCertsMap :: AMap.Map Plutus.DCert Integer
   scriptContextCertsMap = AMap.fromList P.$ P.zip (Plutus.txInfoDCert txInfo) [1]

   singleRedeemerCert :: Plutus.DCert
   singleRedeemerCert = P.head certs

   txinA :: Plutus.TxInInfo
   txinA = P.head redeemerTxins

   txinB :: Plutus.TxInInfo
   txinB = P.head $ P.reverse redeemerTxins

   redeemerTxins :: [Plutus.TxInInfo]
   redeemerTxins = txins

   scriptContextTxins :: [Plutus.TxInInfo]
   scriptContextTxins = Plutus.txInfoInputs txInfo

   scriptContextTxinsMap :: AMap.Map Plutus.TxInInfo Integer
   scriptContextTxinsMap = AMap.fromList P.$ P.zip scriptContextTxins [1,2 :: Integer]

   -- This is paid to the dummy address. We can't compute the change address amount
   -- because the redeemer we computed is based on an older tx which affects the fee
   -- and therefore the change address amount.
   paymentOutputFromRedeemer :: Plutus.Value
   paymentOutputFromRedeemer = P.head $ P.reverse redeemerValues

   redeemerValues :: [Plutus.Value]
   redeemerValues = P.map Plutus.txOutValue txouts

   scriptContextOutputValues :: [Plutus.Value]
   scriptContextOutputValues = P.map Plutus.txOutValue $ Plutus.txInfoOutputs txInfo

   scriptContextOutputsMap :: AMap.Map Plutus.Value Integer
   scriptContextOutputsMap = AMap.fromList P.$ P.zip scriptContextOutputValues [1,2 :: Integer]

   txInfo :: Plutus.TxInfo
   txInfo = Plutus.scriptContextTxInfo scriptContext

   sPurpose :: Plutus.ScriptPurpose
   sPurpose = Plutus.scriptContextPurpose scriptContext

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator mkValidator

plutusV1RedeemerContextTestScript :: Plutus.Script
plutusV1RedeemerContextTestScript = Plutus.unValidatorScript validator

pv1RedeemerContextTestScriptBs :: SBS.ShortByteString
pv1RedeemerContextTestScriptBs = SBS.toShort . LB.toStrict $ serialise plutusV1RedeemerContextTestScript

scriptContextTextPayingScript :: PlutusScript PlutusScriptV1
scriptContextTextPayingScript = PlutusScriptSerialised pv1RedeemerContextTestScriptBs


-- Minting script that checks the minting value, validty interval and
-- required signers in the ScriptContext is equivalent to what's in the
-- redeemer.

{-# INLINABLE mkPolicy #-}
mkPolicy :: PV1CustomRedeemer -> Plutus.ScriptContext -> Bool
mkPolicy (PV1CustomRedeemer _ _ minted txValidRange _fee _ _ signatories mPurpose) scriptContext =
  -- Minted value is equivalent
  minted P.== Plutus.txInfoMint txInfo P.&&
  -- Validity range is equivalent
  Plutus.txInfoValidRange txInfo P.== txValidRange P.&&
  -- Required signers are equivalent
  AMap.member singleSignatory scriptContextSignatoriesMap P.&&

  case mPurpose of
    Just sPurp -> sPurp P.== sPurpose
    Nothing    -> PlutusTx.Prelude.error ()
 where
   sPurpose :: Plutus.ScriptPurpose
   sPurpose = Plutus.scriptContextPurpose scriptContext

   scriptContextSignatoriesMap :: AMap.Map Plutus.PubKeyHash Integer
   scriptContextSignatoriesMap = AMap.fromList P.$ P.zip (Plutus.txInfoSignatories txInfo) [1]

   singleSignatory :: Plutus.PubKeyHash
   singleSignatory = P.head signatories

   txInfo :: Plutus.TxInfo
   txInfo = Plutus.scriptContextTxInfo scriptContext

mintingScriptContextTextPolicy :: Plutus.MintingPolicy
mintingScriptContextTextPolicy = Plutus.mkMintingPolicyScript
           $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.mkUntypedMintingPolicy mkPolicy

plutusV1RedeemerContextTestMintingScript :: Plutus.Script
plutusV1RedeemerContextTestMintingScript =
  Plutus.unMintingPolicyScript mintingScriptContextTextPolicy

scriptContextTextMintingValidator :: Plutus.Validator
scriptContextTextMintingValidator =
  Plutus.Validator plutusV1RedeemerContextTestMintingScript

scriptContextTextMintingScript :: LB.ByteString
scriptContextTextMintingScript = serialise scriptContextTextMintingValidator

scriptContextTestMintingScript :: PlutusScript PlutusScriptV1
scriptContextTestMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptContextTextMintingScript

-- Helpers

pv1CustomRedeemerFromScriptData :: ScriptData -> Either String PV1CustomRedeemer
pv1CustomRedeemerFromScriptData sDat =
  let bIData = PlutusTx.dataToBuiltinData $ toPlutusData sDat
  in case PlutusTx.fromBuiltinData bIData of
      Just mCRedeem -> Right mCRedeem
      Nothing       -> Left "Could not decode PV1CustomRedeemer from ScriptData"
