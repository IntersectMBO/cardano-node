{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Validate
  ( TxAuxScriptsValidationError(..)
  , TxCertificatesValidationError(..)
  , TxFeeValidationError(..)
  , TxProtocolParametersValidationError
  , TxScriptValidityValidationError(..)
  , TxUpdateProposalValidationError(..)
  , TxValidityLowerBoundValidationError(..)
  , TxValidityUpperBoundValidationError(..)
  , TxRequiredSignersValidationError
  , TxReturnCollateralValidationError(..)
  , TxTotalCollateralValidationError(..)
  , TxWithdrawalsValidationError(..)
  , validateProtocolParameters
  , validateScriptSupportedInEra
  , validateTxAuxScripts
  , validateTxCertificates
  , validateTxFee
  , validateRequiredSigners
  , validateTxReturnCollateral
  , validateTxScriptValidity
  , validateTxTotalCollateral
  , validateTxUpdateProposal
  , validateTxValidityUpperBound
  , validateTxValidityLowerBound
  , validateTxWithdrawals
  ) where

import           Prelude

import           Cardano.Api
import           Cardano.Api.Pretty
import           Cardano.Api.Shelley

import           Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import           Data.Maybe

data ScriptLanguageValidationError
  = ScriptLanguageValidationError AnyScriptLanguage AnyCardanoEra
  deriving Show

instance Error ScriptLanguageValidationError where
  displayError (ScriptLanguageValidationError lang era) =
    "The script language " <> pretty lang <> " is not supported in the " <>
    pretty era <> " era."

validateScriptSupportedInEra
  :: CardanoEra era
  -> ScriptInAnyLang
  -> Either ScriptLanguageValidationError (ScriptInEra era)
validateScriptSupportedInEra era script@(ScriptInAnyLang lang _) =
  case toScriptInEra era script of
    Nothing -> Left $ ScriptLanguageValidationError
                        (AnyScriptLanguage lang) (anyCardanoEra era)
    Just script' -> pure script'


data TxFeeValidationError
  = TxFeatureImplicitFeesE AnyCardanoEra -- ^ Expected an explicit fee
  | TxFeatureExplicitFeesE AnyCardanoEra -- ^ Expected an implicit fee
  deriving Show

instance Error TxFeeValidationError where
  displayError (TxFeatureImplicitFeesE era) =
    "Implicit transaction fee not supported in " <> pretty (renderEra era)
  displayError (TxFeatureExplicitFeesE era) =
    "Explicit transaction fee not supported in " <> pretty (renderEra era)

validateTxFee :: CardanoEra era
              -> Maybe Lovelace
              -> Either TxFeeValidationError (TxFee era)
validateTxFee era mfee =
    case (txFeesExplicitInEra era, mfee) of
      (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
      (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)

      (Right _, Nothing) -> Left . TxFeatureImplicitFeesE
                                 $ getIsCardanoEraConstraint era
                                 $ AnyCardanoEra era
      (Left  _, Just _)  -> Left . TxFeatureExplicitFeesE
                                 $ getIsCardanoEraConstraint era
                                 $ AnyCardanoEra era

newtype TxTotalCollateralValidationError
  = TxTotalCollateralNotSupported AnyCardanoEra
  deriving Show

instance Error TxTotalCollateralValidationError where
  displayError (TxTotalCollateralNotSupported era) =
    "Transaction collateral not supported in " <> pretty (renderEra era)

validateTxTotalCollateral :: CardanoEra era
                          -> Maybe Lovelace
                          -> Either TxTotalCollateralValidationError (TxTotalCollateral era)
validateTxTotalCollateral _ Nothing = return TxTotalCollateralNone
validateTxTotalCollateral era (Just coll) =
  case totalAndReturnCollateralSupportedInEra era of
    Just supp -> return $ TxTotalCollateral supp coll
    Nothing -> Left $ TxTotalCollateralNotSupported
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era

newtype TxReturnCollateralValidationError
  = TxReturnCollateralNotSupported AnyCardanoEra
  deriving Show

instance Error TxReturnCollateralValidationError where
  displayError (TxReturnCollateralNotSupported era) =
    "Transaction return collateral not supported in " <> pretty (renderEra era)

validateTxReturnCollateral :: CardanoEra era
                           -> Maybe (TxOut CtxTx era)
                           -> Either TxReturnCollateralValidationError (TxReturnCollateral CtxTx era)
validateTxReturnCollateral _ Nothing = return TxReturnCollateralNone
validateTxReturnCollateral era (Just retColTxOut) = do
  case totalAndReturnCollateralSupportedInEra era of
    Just supp -> return $ TxReturnCollateral supp retColTxOut
    Nothing -> Left $ TxReturnCollateralNotSupported
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era

newtype TxValidityLowerBoundValidationError
  = TxValidityLowerBoundNotSupported AnyCardanoEra
  deriving Show

instance Error TxValidityLowerBoundValidationError where
  displayError (TxValidityLowerBoundNotSupported era) =
    "Transaction validity lower bound not supported in " <> pretty (renderEra era)


validateTxValidityLowerBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> Either TxValidityLowerBoundValidationError (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound era (Just slot) =
    case validityLowerBoundSupportedInEra era of
      Nothing -> Left $ TxValidityLowerBoundNotSupported
                      $ getIsCardanoEraConstraint era
                      $ AnyCardanoEra era
      Just supported -> return (TxValidityLowerBound supported slot)

newtype TxValidityUpperBoundValidationError
  = TxValidityUpperBoundNotSupported AnyCardanoEra
  deriving Show

instance Error TxValidityUpperBoundValidationError where
  displayError (TxValidityUpperBoundNotSupported era) =
    "Transaction validity upper bound must be specified in " <> pretty (renderEra era)

validateTxValidityUpperBound
  :: CardanoEra era
  -> Maybe SlotNo
  -> Either TxValidityUpperBoundValidationError (TxValidityUpperBound era)
validateTxValidityUpperBound era Nothing =
  case validityNoUpperBoundSupportedInEra era of
    Nothing -> Left $ TxValidityUpperBoundNotSupported
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era
    Just supported -> return (TxValidityNoUpperBound supported)
validateTxValidityUpperBound era (Just slot) =
  case validityUpperBoundSupportedInEra era of
    Nothing -> Left $ TxValidityUpperBoundNotSupported
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era
    Just supported -> return (TxValidityUpperBound supported slot)

data TxAuxScriptsValidationError
  = TxAuxScriptsNotSupportedInEra AnyCardanoEra
  | TxAuxScriptsLanguageError ScriptLanguageValidationError
  deriving Show

instance Error TxAuxScriptsValidationError where
  displayError (TxAuxScriptsNotSupportedInEra era) =
    "Transaction auxiliary scripts are not supported in " <> pretty (renderEra era)
  displayError (TxAuxScriptsLanguageError e) =
    "Transaction auxiliary scripts error: " <> displayError e

validateTxAuxScripts
  :: CardanoEra era
  -> [ScriptInAnyLang]
  -> Either TxAuxScriptsValidationError (TxAuxScripts era)
validateTxAuxScripts _ [] = return TxAuxScriptsNone
validateTxAuxScripts era scripts =
  case auxScriptsSupportedInEra era of
    Nothing -> Left $ TxAuxScriptsNotSupportedInEra
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era
    Just supported -> do
      scriptsInEra <- mapM (first TxAuxScriptsLanguageError . validateScriptSupportedInEra era) scripts
      return $ TxAuxScripts supported scriptsInEra

newtype TxRequiredSignersValidationError
  = TxRequiredSignersValidationError AnyCardanoEra
  deriving Show

instance Error TxRequiredSignersValidationError where
  displayError (TxRequiredSignersValidationError e) =
    "Transaction required signers are not supported in " <> pretty (renderEra e)

validateRequiredSigners
  :: CardanoEra era
  -> [Hash PaymentKey]
  -> Either TxRequiredSignersValidationError (TxExtraKeyWitnesses era)
validateRequiredSigners _ [] = return TxExtraKeyWitnessesNone
validateRequiredSigners era reqSigs =
  case extraKeyWitnessesSupportedInEra era of
    Nothing -> Left $ TxRequiredSignersValidationError
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era
    Just supported -> return $ TxExtraKeyWitnesses supported reqSigs

newtype TxWithdrawalsValidationError
  = TxWithdrawalsNotSupported AnyCardanoEra
  deriving Show

instance Error TxWithdrawalsValidationError where
  displayError (TxWithdrawalsNotSupported e) =
    "Transaction withdrawals are not supported in " <> pretty (renderEra e)

validateTxWithdrawals
  :: forall era.
     CardanoEra era
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> Either TxWithdrawalsValidationError (TxWithdrawals BuildTx era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals =
  case withdrawalsSupportedInEra era of
    Nothing -> Left $ TxWithdrawalsNotSupported
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era
    Just supported -> do
      let convWithdrawals = map convert withdrawals
      return (TxWithdrawals supported convWithdrawals)
 where
  convert
    :: (StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))
    -> (StakeAddress, Lovelace, BuildTxWith BuildTx (Witness WitCtxStake era))
  convert (sAddr, ll, mScriptWitnessFiles) =
    case mScriptWitnessFiles of
      Just sWit -> do
        (sAddr, ll, BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit)
      Nothing -> (sAddr,ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

newtype TxCertificatesValidationError
  = TxCertificatesValidationNotSupported AnyCardanoEra
  deriving Show

instance Error TxCertificatesValidationError where
  displayError (TxCertificatesValidationNotSupported e) =
    "Transaction certificates are not supported in " <> pretty (renderEra e)

validateTxCertificates
  :: forall era.
     CardanoEra era
  -> [(Certificate, Maybe (ScriptWitness WitCtxStake era))]
  -> Either TxCertificatesValidationError (TxCertificates BuildTx era)
validateTxCertificates _ [] = return TxCertificatesNone
validateTxCertificates era certsAndScriptWitnesses =
  case certificatesSupportedInEra era of
    Nothing -> Left $ TxCertificatesValidationNotSupported
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era
    Just supported -> do
      let certs = map fst certsAndScriptWitnesses
          reqWits = Map.fromList $ mapMaybe convert certsAndScriptWitnesses
      return $ TxCertificates supported certs $ BuildTxWith reqWits
  where
   -- We get the stake credential witness for a certificate that requires it.
   -- NB: Only stake address deregistration and delegation requires
   -- witnessing (witness can be script or key)
   deriveStakeCredentialWitness
     :: Certificate
     -> Maybe StakeCredential
   deriveStakeCredentialWitness cert = do
     case cert of
       StakeAddressDeregistrationCertificate sCred -> Just sCred
       StakeAddressPoolDelegationCertificate sCred _ -> Just sCred
       _ -> Nothing

   convert
     :: (Certificate, Maybe (ScriptWitness WitCtxStake era))
     -> Maybe (StakeCredential, Witness WitCtxStake era)
   convert (cert, mScriptWitnessFiles) = do
     sCred <- deriveStakeCredentialWitness cert
     case mScriptWitnessFiles of
       Just sWit -> do
         Just ( sCred
              , ScriptWitness ScriptWitnessForStakeAddr sWit
              )
       Nothing -> Just (sCred, KeyWitness KeyWitnessForStakeAddr)

newtype TxProtocolParametersValidationError
  = ProtocolParametersNotSupported AnyCardanoEra
  deriving Show

instance Error TxProtocolParametersValidationError where
  displayError (ProtocolParametersNotSupported e) =
    "Transaction protocol parameters are not supported in " <> pretty (renderEra e)

validateProtocolParameters
  :: CardanoEra era
  -> Maybe ProtocolParameters
  -> Either TxProtocolParametersValidationError (BuildTxWith BuildTx (Maybe ProtocolParameters))
validateProtocolParameters _ Nothing = return (BuildTxWith Nothing)
validateProtocolParameters era (Just pparams) =
    case scriptDataSupportedInEra era of
      Nothing -> Left $ ProtocolParametersNotSupported
                      $ getIsCardanoEraConstraint era
                      $ AnyCardanoEra era
      Just _  -> return . BuildTxWith $ Just pparams

newtype TxUpdateProposalValidationError
  = TxUpdateProposalNotSupported AnyCardanoEra
  deriving Show

instance Error TxUpdateProposalValidationError where
  displayError (TxUpdateProposalNotSupported e) =
    "Transaction update proposal is not supported in " <> pretty (renderEra e)

validateTxUpdateProposal
  :: CardanoEra era
  -> Maybe UpdateProposal
  -> Either TxUpdateProposalValidationError (TxUpdateProposal era)
validateTxUpdateProposal _ Nothing = return TxUpdateProposalNone
validateTxUpdateProposal era (Just prop) =
    case updateProposalSupportedInEra era of
      Nothing -> Left $ TxUpdateProposalNotSupported
                      $ getIsCardanoEraConstraint era
                      $ AnyCardanoEra era
      Just supported -> return $ TxUpdateProposal supported prop

newtype TxScriptValidityValidationError
  = ScriptValidityNotSupported AnyCardanoEra
  deriving Show

instance Error TxScriptValidityValidationError where
  displayError (ScriptValidityNotSupported e) =
    "Transaction script validity is not supported in " <> pretty (renderEra e)

validateTxScriptValidity
  :: CardanoEra era
  -> Maybe ScriptValidity
  -> Either TxScriptValidityValidationError (TxScriptValidity era)
validateTxScriptValidity _ Nothing = pure TxScriptValidityNone
validateTxScriptValidity era (Just scriptValidity) =
  case txScriptValiditySupportedInCardanoEra era of
    Nothing -> Left $ ScriptValidityNotSupported
                    $ getIsCardanoEraConstraint era
                    $ AnyCardanoEra era
    Just supported -> pure $ TxScriptValidity supported scriptValidity
