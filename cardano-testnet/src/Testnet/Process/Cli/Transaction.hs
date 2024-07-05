{-# LANGUAGE DataKinds #-}

module Testnet.Process.Cli.Transaction
  ( signTx
  , submitTx
  , failToSubmitTx
  , retrieveTransactionId
  , SignedTx
  , TxBody
  , VoteFile
  ) where

import           Cardano.Api hiding (Certificate, TxBody)

import           Prelude

import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch)
import           Data.List (isInfixOf)
import           GHC.IO.Exception (ExitCode (..))
import           GHC.Stack
import           System.FilePath ((</>))

import           Testnet.Process.Run (execCli')
import           Testnet.Start.Types (anyEraToString)
import           Testnet.Types

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H

-- Transaction signing
data VoteFile

data TxBody

data SignedTx

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
  -> [SomeKeyPair] -- ^ List of key pairs used for signing the transaction.
  -> m (File SignedTx In)
signTx execConfig cEra work prefix txBody signatoryKeyPairs = do
  let signedTx = File (work </> prefix <> ".tx")
  void $ execCli' execConfig $
    [ anyEraToString cEra, "transaction", "sign"
    , "--tx-body-file", unFile txBody
    ] ++ (concat [["--signing-key-file", signingKeyFp kp] | SomeKeyPair kp <- signatoryKeyPairs]) ++
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
    [ "transaction", "txid"
    , "--tx-file", unFile signedTxBody
    ]
  return $ mconcat $ lines txidOutput

