{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Testnet.PlutusExample.PlutusVersion2.RedeemerContextEquivalence
  ( PV2CustomRedeemer (..)
  , v2ScriptContextEquivalenceScript
  , v2ScriptContextEquivalenceSbs
  , v2mintEquivScript
  , v2mintEquivScriptShortBs
  ) where

import           Cardano.Api.Shelley

import qualified PlutusLedgerApi.V2 as V2
import           PlutusLedgerApi.V2.Contexts as V2

import           Prelude hiding (($), (&&))

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import           Plutus.Script.Utils.Typed as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude as PlutusPrelude hiding (Semigroup (..), unless, (.))

newtype MyCustomDatumV2 = MyCustomDatumV2 Integer

data PV2CustomRedeemer
  = PV2CustomRedeemer
      { pv2Inputs        :: [V2.TxInInfo]
      , pv2RefInputs     :: [V2.TxInInfo]
      , pv2Outputs       :: [V2.TxOut]
      , pv2Fee           :: V2.Value
      , pv2Mint          :: V2.Value
      , pv2DCert         :: [V2.DCert]
      , pv2Wdrl          :: V2.Map V2.StakingCredential Integer
      , pv2ValidRange    :: V2.POSIXTimeRange
      , pv2Signatories   :: [V2.PubKeyHash]
      , pv2Redeemers     :: V2.Map ScriptPurpose V2.Redeemer
      , pv2Data          :: V2.Map V2.DatumHash V2.Datum
      , pv2ScriptPurpose :: Maybe V2.ScriptPurpose
      } deriving (Prelude.Eq, Show)

PlutusTx.unstableMakeIsData ''MyCustomDatumV2
PlutusTx.unstableMakeIsData ''PV2CustomRedeemer

-- @(PV2CustomRedeemer inputs refInputs outputs fee mint dCert wdrl validRange signatories redeemers data)

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatumV2 -> PV2CustomRedeemer -> V2.ScriptContext -> Bool
mkValidator _ redeemer scriptContext =
  -- These all work fine
  inputsAreEquivalent redeemer txInfo PlutusPrelude.&&
  referenceInputsAreEquivalent redeemer txInfo PlutusPrelude.&&
  certsAreEquivalent redeemer txInfo PlutusPrelude.&&
  reqSignersAreEquivalent redeemer txInfo PlutusPrelude.&&
  datumHashMapsAreEquivalent redeemer txInfo PlutusPrelude.&&
  outputsAreEquivalent redeemer txInfo PlutusPrelude.&&
  correctNumberOfRedeemers redeemer txInfo
  -- These below are failing
  -- validtyIntervalsAreEquivalent redeemer txInfo
  -- Inequality for validity interval doesnt work. Also the interval reported by the script context is a little ahead of
  -- what is in the transaction
  -- TODO: You can't check the fee with the build command due to how it's constructed
  -- These below have not been tested
  -- withdrawalsAreEquivalent redeemer txInfo
 where
  txInfo :: V2.TxInfo
  txInfo = V2.scriptContextTxInfo scriptContext

  inputsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  inputsAreEquivalent (PV2CustomRedeemer inputs _ _ _ _ _ _ _ _ _ _ _) tInfo =
    PlutusPrelude.map txInInfoResolved (V2.txInfoInputs tInfo) PlutusPrelude.==
    PlutusPrelude.map txInInfoResolved inputs

  referenceInputsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  referenceInputsAreEquivalent (PV2CustomRedeemer _ refInputs _ _ _ _ _ _ _ _ _ _) tInfo =
    PlutusPrelude.map txInInfoResolved (V2.txInfoReferenceInputs tInfo) PlutusPrelude.==
    PlutusPrelude.map txInInfoResolved refInputs

  outputsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  outputsAreEquivalent (PV2CustomRedeemer _ _ outputs _ _ _ _ _ _ _ _ _) tInfo =
    let scOuts = V2.txInfoOutputs tInfo
        scOutAddrs = PlutusPrelude.map V2.txOutAddress scOuts
        scOutValue = PlutusPrelude.map V2.txOutValue scOuts
        scOutDatums = PlutusPrelude.map V2.txOutDatum scOuts
        scOutReferenceScripts = PlutusPrelude.map V2.txOutReferenceScript scOuts

        redeemerOutAddrs = PlutusPrelude.map V2.txOutAddress outputs
        redeemerOutValue = PlutusPrelude.map V2.txOutValue outputs
        redeemerOutDatums = PlutusPrelude.map V2.txOutDatum outputs
        redeemerOutReferenceScripts = PlutusPrelude.map V2.txOutReferenceScript outputs
    in (scOutAddrs PlutusPrelude.== redeemerOutAddrs) PlutusPrelude.&&
       (scOutDatums PlutusPrelude.== redeemerOutDatums) PlutusPrelude.&&
       (scOutReferenceScripts PlutusPrelude.== redeemerOutReferenceScripts) PlutusPrelude.&&
       -- We want to see if out tx out specified in our tx is equal to one of the txouts in the
       -- script context. So we have a total of 4 outputs when we combine the outputs in the script
       -- context and the redeemer. This would be the two "normal" outputs and the two "change outputs"
       (PlutusPrelude.length (scOutValue PlutusPrelude.++ redeemerOutValue) PlutusPrelude.== 4) PlutusPrelude.&&
       -- You would expect calling nub on the combined values, we should expect a length of 2. However
       -- the change outputs will be different because of how we construct the redeemer. Essentially we
       -- use an idential tx to generate our redeemer (and the redeemer in this tx is a default redeemer with nothing in it)
       -- and then we add that redeemer to a new transaction built with the `build` command. The problem is
       -- the fee and the change outputs created from the initial tx will be different because the size of
       -- the total tx is now different. Therefore we expect the length to be 3 since only the "normal"
       -- txouts are equivalent but the change outputs are different!
       (PlutusPrelude.length (nub $ scOutValue PlutusPrelude.++ redeemerOutValue) PlutusPrelude.== 3)

  certsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  certsAreEquivalent (PV2CustomRedeemer _ _ _ _ _ certs _ _ _ _ _ _) tInfo =
    V2.txInfoDCert tInfo PlutusPrelude.== certs

  --validtyIntervalsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  --validtyIntervalsAreEquivalent (PV2CustomRedeemer _ _ _ _ _ _ _ validInterval _ _ _) tInfo =
  --   (V2.txInfoValidRange tInfo) PlutusPrelude./= validInterval
    -- V2.ivFrom (V2.txInfoValidRange tInfo) PlutusPrelude.== V2.ivFrom validInterval Fails

  reqSignersAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  reqSignersAreEquivalent (PV2CustomRedeemer _ _ _ _ _ _ _ _ reqSigners _ _ _) tInfo =
    V2.txInfoSignatories tInfo PlutusPrelude.== reqSigners

  datumHashMapsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  datumHashMapsAreEquivalent (PV2CustomRedeemer _ _ _ _ _ _ _ _ _ _ datumHashMap _) tInfo =
    V2.txInfoData tInfo PlutusPrelude.== datumHashMap

  correctNumberOfRedeemers :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  correctNumberOfRedeemers (PV2CustomRedeemer _ _ _ _ _ _ _ _ _ redeemers _ _) tInfo =
    PlutusPrelude.length (V2.txInfoRedeemers tInfo) PlutusPrelude.== PlutusPrelude.length redeemers

  -- TODO: not done yet
  --withdrawalsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
  --withdrawalsAreEquivalent (PV2CustomRedeemer _ _ _ _ _ _ wdrwls _ _ _ _) tInfo =
  -- V2.txInfoWdrl tInfo PlutusPrelude.== wdrwls
  -- TODO: Also need to do separate minting script

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator mkValidator

v2ScriptContextEquivalencePlutusScript :: Plutus.Script
v2ScriptContextEquivalencePlutusScript = Plutus.unValidatorScript validator

v2ScriptContextEquivalenceSbs :: SBS.ShortByteString
v2ScriptContextEquivalenceSbs =
  SBS.toShort . LBS.toStrict $ serialise v2ScriptContextEquivalencePlutusScript

v2ScriptContextEquivalenceScript :: PlutusScript PlutusScriptV2
v2ScriptContextEquivalenceScript = PlutusScriptSerialised v2ScriptContextEquivalenceSbs

-- Mint field and script purpose equivalence equivalence

{-# INLINABLE mkMintEquivalenceValidator #-}
mkMintEquivalenceValidator :: PV2CustomRedeemer -> V2.ScriptContext -> Bool
mkMintEquivalenceValidator redeemer scriptContext =
  -- Minted value is equivalent
  mintingFieldsAreEquivalent redeemer txInfo PlutusPrelude.&&
  -- Script purpose is equivalent
  scriptPurposeIsEquivalent scriptContext redeemer
 where
   txInfo :: V2.TxInfo
   txInfo = V2.scriptContextTxInfo scriptContext

   mintingFieldsAreEquivalent :: PV2CustomRedeemer -> V2.TxInfo -> Bool
   mintingFieldsAreEquivalent (PV2CustomRedeemer _ _ _ _ mint _ _ _ _ _ _ _) tInfo =
    V2.txInfoMint tInfo  PlutusPrelude.== mint

   scriptPurposeIsEquivalent :: V2.ScriptContext -> PV2CustomRedeemer -> Bool
   scriptPurposeIsEquivalent sc (PV2CustomRedeemer _ _ _ _ _ _ _ _ _ _ _ mScPurpose) =
    case mScPurpose of
      Just sPurp -> V2.scriptContextPurpose sc PlutusPrelude.== sPurp
      Nothing    -> PlutusPrelude.error ()

policy :: Plutus.MintingPolicy
policy = Plutus.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.mkUntypedMintingPolicy mkMintEquivalenceValidator

plutusMintEquivScript :: Plutus.Script
plutusMintEquivScript =
  Plutus.unMintingPolicyScript policy

mintEquivValidator :: Plutus.Validator
mintEquivValidator = Plutus.Validator plutusMintEquivScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise mintEquivValidator

v2mintEquivScript :: PlutusScript PlutusScriptV2
v2mintEquivScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

v2mintEquivScriptShortBs :: SBS.ShortByteString
v2mintEquivScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor
