{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Process.Cli.Transaction
  ( mkSimpleSpendOutputsOnlyTx
  , mkSpendOutputsOnlyTx
  , signTx
  , submitTx
  , failToSubmitTx
  , retrieveTransactionId
  , SignedTx
  , TxBody
  , TxOutAddress (..)
  , VoteFile
  )
where

import           Cardano.Api hiding (Certificate, TxBody)
import           Cardano.Api.Experimental (Some (..))
import           Cardano.Api.Ledger (Coin (unCoin))

import           Prelude

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Data.List (isInfixOf)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           GHC.IO.Exception (ExitCode (..))
import           GHC.Stack
import           System.FilePath ((</>))

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H

import           Testnet.Components.Query (EpochStateView, findLargestUtxoForPaymentKey)
import           Testnet.Process.Run (execCli')
import           Testnet.Start.Types (anyEraToString)
import           Testnet.Types

-- Transaction signing
data VoteFile

data TxBody

data SignedTx

data ScriptJSON

data TxOutAddress
  = PubKeyAddress PaymentKeyInfo
  | -- | The output will be created at the script address.
    ScriptAddress (File ScriptJSON In)

-- | Calls @cardano-cli@ to build a simple ADA transfer transaction to
-- the specified outputs of the specified amount of ADA. Destination
-- address may be specified as a 'PaymentKeyInfo' or with a script file.
-- For each output, an extra optional script file may be provided, and
-- if provided, the script provided will be published in that output
-- as a reference script.
--
-- Returns the generated @File TxBody In@ file path to the created unsigned
-- transaction file.
mkSpendOutputsOnlyTx
  :: (HasCallStack, Typeable era, H.MonadAssertion m, MonadTest m, MonadCatch m, MonadIO m)
  => H.ExecConfig
  -- ^ Specifies the CLI execution configuration.
  -> EpochStateView
  -- ^ Current epoch state view for transaction building. It can be obtained
  -- using the 'getEpochStateView' function.
  -> ShelleyBasedEra era
  -- ^ Witness for the current Cardano era.
  -> FilePath
  -- ^ Base directory path where the unsigned transaction file will be stored.
  -> String
  -- ^ Prefix for the output unsigned transaction file name. The extension will be @.txbody@.
  -> PaymentKeyInfo
  -- ^ Payment key pair used for paying the transaction.
  -> [(TxOutAddress, Coin, Maybe (File ScriptJSON In))]
  -- ^ List of tuples with transaction output addresses, amounts, and reference scripts.
  -> m (File TxBody In)
mkSpendOutputsOnlyTx execConfig epochStateView sbe work prefix srcWallet txOutputs = do
  txIn <- findLargestUtxoForPaymentKey epochStateView sbe srcWallet
  fixedTxOuts :: [String] <- computeTxOuts
  void $ execCli' execConfig $
           mconcat
             [ [ anyEraToString cEra
               , "transaction", "build"
               , "--change-address", srcAddress
               , "--tx-in", T.unpack $ renderTxIn txIn
               ]
             , fixedTxOuts
             , [ "--out-file", unFile txBody
               ]
             ]
  return txBody
 where
  era = toCardanoEra sbe
  cEra = AnyCardanoEra era
  txBody = File (work </> prefix <> ".txbody")
  srcAddress = T.unpack $ paymentKeyInfoAddr srcWallet
  computeTxOuts =
    concat <$> sequence
      [ case txOut of
          PubKeyAddress dstWallet ->
            return ["--tx-out", T.unpack (paymentKeyInfoAddr dstWallet) <> "+" ++ show (unCoin amount)]
          ScriptAddress (File referenceScriptJSON) -> do
            scriptAddress <-
              execCli'
                execConfig
                [ anyEraToString cEra
                , "address", "build"
                , "--payment-script-file", referenceScriptJSON
                ]
            return
              ( ["--tx-out", scriptAddress <> "+" ++ show (unCoin amount)]
                  <> maybe [] (\(File newRefScript) -> ["--tx-out-reference-script-file", newRefScript]) mNewRefScript
              )
      | (txOut, amount, mNewRefScript) <- txOutputs
      ]

-- | Calls @cardano-cli@ to build a simple ADA transfer transaction to
-- transfer to the specified recipient the specified amount of ADA.
--
-- Returns the generated @File TxBody In@ file path to the created unsigned
-- transaction file.
mkSimpleSpendOutputsOnlyTx
  :: HasCallStack
  => Typeable era
  => H.MonadAssertion m
  => MonadTest m
  => MonadCatch m
  => MonadIO m
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> EpochStateView -- ^ Current epoch state view for transaction building. It can be obtained
                    -- using the 'getEpochStateView' function.
  -> ShelleyBasedEra era -- ^ Witness for the current Cardano era.
  -> FilePath -- ^ Base directory path where the unsigned transaction file will be stored.
  -> String -- ^ Prefix for the output unsigned transaction file name. The extension will be @.txbody@.
  -> PaymentKeyInfo -- ^ Payment key pair used for paying the transaction.
  -> PaymentKeyInfo -- ^ Payment key of the recipient of the transaction.
  -> Coin -- ^ Amount of ADA to transfer (in Lovelace).
  -> m (File TxBody In)
mkSimpleSpendOutputsOnlyTx execConfig epochStateView sbe work prefix srcWallet dstWallet amount =
  mkSpendOutputsOnlyTx execConfig epochStateView sbe work prefix srcWallet [(PubKeyAddress dstWallet, amount, Nothing)]

-- | Calls @cardano-cli@ to signs a transaction body using the specified key pairs.
--
-- This function takes five parameters:
--
-- Returns the generated @File SignedTx In@ file path to the signed transaction file.
signTx
  :: MonadTest m
  => MonadCatch m
  => MonadIO m
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> AnyCardanoEra -- ^ Specifies the current Cardano era.
  -> FilePath -- ^ Base directory path where the signed transaction file will be stored.
  -> String -- ^ Prefix for the output signed transaction file name. The extension will be @.tx@.
  -> File TxBody In -- ^ Transaction body to be signed, obtained using 'createCertificatePublicationTxBody' or similar.
  -> [Some KeyPair] -- ^ List of key pairs used for signing the transaction.
  -> m (File SignedTx In)
signTx execConfig cEra work prefix txBody signatoryKeyPairs = do
  let signedTx = File (work </> prefix <> ".tx")
  void $ execCli' execConfig $
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", unFile txBody
    ] ++ (concat [["--signing-key-file", signingKeyFp kp] | Some kp <- signatoryKeyPairs]) ++
    [ "--out-file", unFile signedTx
    ]
  return signedTx

-- | Submits a signed transaction using @cardano-cli@.
submitTx
  :: HasCallStack
  => MonadTest m
  => MonadCatch m
  => MonadIO m
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> AnyCardanoEra -- ^ Specifies the current Cardano era.
  -> File SignedTx In -- ^ Signed transaction to be submitted, obtained using 'signTx'.
  -> m ()
submitTx execConfig cEra signedTx =
  void $ execCli' execConfig
    [ anyEraToString cEra, "transaction", "submit"
    , "--tx-file", unFile signedTx
    ]

-- | Attempts to submit a transaction that is expected to fail using @cardano-cli@.
--
-- If the submission fails (the expected behavior), the function succeeds.
-- If the submission succeeds unexpectedly, it raises a failure message that is
-- meant to be caught by @Hedgehog@.
failToSubmitTx
  :: MonadTest m
  => MonadCatch m
  => MonadIO m
  => HasCallStack
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> AnyCardanoEra -- ^ Specifies the current Cardano era.
  -> File SignedTx In -- ^ Signed transaction to be submitted, obtained using 'signTx'.
  -> String -- ^ Substring of the error to check for to ensure submission failed for
            -- the right reason.
  -> m ()
failToSubmitTx execConfig cEra signedTx reasonForFailure = withFrozenCallStack $ do
  (exitCode, _, stderr) <- H.execFlexAny' execConfig "cardano-cli" "CARDANO_CLI"
                                     [ anyEraToString cEra, "transaction", "submit"
                                     , "--tx-file", unFile signedTx
                                     ]
  case exitCode of -- Did it fail?
    ExitSuccess -> H.failMessage callStack "Transaction submission was expected to fail but it succeeded"
    _ -> if reasonForFailure `isInfixOf` stderr -- Did it fail for the expected reason?
         then return ()
         else H.failMessage callStack $ "Transaction submission failed for the wrong reason (not " ++
                                            show reasonForFailure ++ "): " ++ stderr

-- | Retrieves the transaction ID (governance action ID) from a signed
-- transaction file using @cardano-cli@.
--
-- Returns the transaction ID (governance action ID) as a 'String'.
retrieveTransactionId
  :: HasCallStack
  => MonadTest m
  => MonadCatch m
  => MonadIO m
  => H.ExecConfig -- ^ Specifies the CLI execution configuration.
  -> File SignedTx In -- ^ Signed transaction to be submitted, obtained using 'signTx'.
  -> m String
retrieveTransactionId execConfig signedTxBody = do
  txidOutput <- execCli' execConfig
    [ "latest", "transaction", "txid"
    , "--tx-file", unFile signedTxBody
    ]
  return $ mconcat $ lines txidOutput

