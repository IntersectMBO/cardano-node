{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Shelley.Parsers
  ( -- * CLI command parser
    parseShelleyCommands

    -- * CLI command and flag types
  , module Cardano.CLI.Shelley.Commands

    -- * Field parser and renderers
  , parseTxIn
  , renderTxIn
  ) where

import           Cardano.Prelude hiding (All, Any, option)
import           Prelude (String)

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Mary.ValueParser (parseValue)
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (InputFormat (..), PaymentVerifier (..),
                   StakeVerifier (..), VerificationKeyOrFile (..), VerificationKeyOrHashOrFile (..),
                   VerificationKeyTextOrFile (..), deserialiseInput, renderInputDecodeError)
import           Cardano.CLI.Types
import qualified Cardano.Ledger.BaseTypes as Shelley
import           Control.Monad.Fail (fail)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeOrError)
import           Network.Socket (PortNumber)
import           Options.ApplicativeAlt hiding (help, str)
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IP as IP
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson.Parser
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Options.ApplicativeAlt as Opt
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Text.Parsec.Token as Parsec

import qualified Shelley.Spec.Ledger.TxBody as Shelley

{- HLINT ignore "Use <$>" -}

--
-- Shelley CLI command parsers
--

parseShelleyCommands :: Parser ann ShelleyCommand
parseShelleyCommands =
  Opt.hsubparser $
    mconcat
      [ Opt.metavar "Era based commands"
      , Opt.commandGroup "Era based commands"
      , Opt.command "address"
          (Opt.info (AddressCmd <$> pAddressCmd) $ Opt.progDesc "Payment address commands")
      , Opt.command "stake-address"
          (Opt.info (StakeAddressCmd <$> pStakeAddressCmd) $ Opt.progDesc "Stake address commands")
      , Opt.command "key"
          (Opt.info (KeyCmd <$> pKeyCmd) $ Opt.progDesc "Key utility commands")
      , Opt.command "transaction"
          (Opt.info (TransactionCmd <$> pTransaction) $ Opt.progDesc "Transaction commands")
      , Opt.command "node"
          (Opt.info (NodeCmd <$> pNodeCmd) $ Opt.progDesc "Node operation commands")
      , Opt.command "stake-pool"
          (Opt.info (PoolCmd <$> pPoolCmd) $ Opt.progDesc "Stake pool commands")
      , Opt.command "query"
          (Opt.info (QueryCmd <$> pQueryCmd) . Opt.progDesc $
             mconcat
               [ "Node query commands. Will query the local node whose Unix domain socket "
               , "is obtained from the CARDANO_NODE_SOCKET_PATH enviromnent variable."
               ]
            )
      , Opt.command "genesis"
          (Opt.info (GenesisCmd <$> pGenesisCmd) $ Opt.progDesc "Genesis block commands")
      , Opt.command "governance"
          (Opt.info (GovernanceCmd <$> pGovernanceCmd) $ Opt.progDesc "Governance commands")
      , Opt.command "text-view"
          (Opt.info (TextViewCmd <$> pTextViewCmd) . Opt.progDesc $
             mconcat
               [ "Commands for dealing with Shelley TextView files. "
               , "Transactions, addresses etc are stored on disk as TextView files."
               ]
            )

      ]

pTextViewCmd :: Parser ann TextViewCmd
pTextViewCmd =
  asum
    [ subParser "decode-cbor"
        (Opt.info (TextViewInfo <$> pCBORInFile <*> pMaybeOutputFile)
          $ Opt.progDesc "Print a TextView file as decoded CBOR."
          )
    ]

pCBORInFile :: Parser ann FilePath
pCBORInFile =
  Opt.strOption
    (  Opt.long "in-file"
    <> Opt.metavar "FILE"
    <> Opt.help "CBOR input file."
    <> Opt.completer (Opt.bashCompleter "file")
    )
  <|>
  Opt.strOption
    (  Opt.long "file"
    <> Opt.internal
    )

pAddressCmd :: Parser ann AddressCmd
pAddressCmd =
   asum
     [ subParser "key-gen"
         (Opt.info pAddressKeyGen $ Opt.progDesc "Create an address key pair.")
     , subParser "key-hash"
         (Opt.info pAddressKeyHash $ Opt.progDesc "Print the hash of an address key.")
     , subParser "build"
         (Opt.info pAddressBuild $ Opt.progDesc "Build a Shelley payment address, with optional delegation to a stake address.")
     , subParser "build-script"
         (Opt.info pAddressBuildScript $ Opt.progDesc "Build a Shelley script address. (deprecated; use 'build' instead with '--payment-script-file')")
     , subParser "info"
         (Opt.info pAddressInfo $ Opt.progDesc "Print information about an address.")
     ]
  where
    pAddressKeyGen :: Parser ann AddressCmd
    pAddressKeyGen = AddressKeyGen <$> pAddressKeyType
                                   <*> pVerificationKeyFile Output
                                   <*> pSigningKeyFile Output

    pAddressKeyHash :: Parser ann AddressCmd
    pAddressKeyHash =
      AddressKeyHash
        <$> pPaymentVerificationKeyTextOrFile
        <*> pMaybeOutputFile

    pAddressBuild :: Parser ann AddressCmd
    pAddressBuild = AddressBuild
      <$> pPaymentVerifier
      <*> Opt.optional pStakeVerifier
      <*> pNetworkId
      <*> pMaybeOutputFile

    pAddressBuildScript :: Parser ann AddressCmd
    pAddressBuildScript = AddressBuildMultiSig
      <$> pScript
      <*> pNetworkId
      <*> pMaybeOutputFile

    pAddressInfo :: Parser ann AddressCmd
    pAddressInfo = AddressInfo <$> pAddress <*> pMaybeOutputFile

pPaymentVerifier :: Parser ann PaymentVerifier
pPaymentVerifier =
        PaymentVerifierKey <$> pPaymentVerificationKeyTextOrFile
    <|> PaymentVerifierScriptFile <$>
          pScriptFor "payment-script-file" Nothing
                     "Filepath of the payment script."

pStakeVerifier :: Parser ann StakeVerifier
pStakeVerifier =
        StakeVerifierKey <$> pStakeVerificationKeyOrFile
    <|> StakeVerifierScriptFile <$>
          pScriptFor "stake-script-file" Nothing
                     "Filepath of the staking script."

pPaymentVerificationKeyTextOrFile :: Parser ann VerificationKeyTextOrFile
pPaymentVerificationKeyTextOrFile =
        VktofVerificationKeyText <$> pPaymentVerificationKeyText
    <|> VktofVerificationKeyFile <$> pPaymentVerificationKeyFile

pPaymentVerificationKeyText :: Parser ann Text
pPaymentVerificationKeyText =
  Text.pack <$>
    Opt.strOption
      (  Opt.long "payment-verification-key"
      <> Opt.metavar "STRING"
      <> Opt.help "Payment verification key (Bech32-encoded)"
      )

pPaymentVerificationKeyFile :: Parser ann VerificationKeyFile
pPaymentVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "payment-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the payment verification key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
        (  Opt.long "verification-key-file"
        <> Opt.internal
        )
    )

pScript :: Parser ann ScriptFile
pScript = pScriptFor "script-file" Nothing "Filepath of the script."

pScriptFor :: String -> Maybe String -> String -> Parser ann ScriptFile
pScriptFor name Nothing help =
  ScriptFile <$> Opt.strOption
    (  Opt.long name
    <> Opt.metavar "FILE"
    <> Opt.help help
    <> Opt.completer (Opt.bashCompleter "file")
    )

pScriptFor name (Just deprecated) help =
      pScriptFor name Nothing help
  <|> ScriptFile <$> Opt.strOption
        (  Opt.long deprecated
        <> Opt.internal
        )

pScriptWitnessFiles :: forall witctx ann.
                       WitCtx witctx
                    -> String
                    -> Maybe String
                    -> String
                    -> Parser ann (ScriptWitnessFiles witctx)
pScriptWitnessFiles witctx scriptFlagPrefix scriptFlagPrefixDeprecated help =
    toScriptWitnessFiles
      <$> pScriptFor (scriptFlagPrefix ++ "-script-file")
                     ((++ "-script-file") <$> scriptFlagPrefixDeprecated)
                     ("The file containing the script to witness " ++ help)
      <*> optional ((,,) <$> pScriptDatumOrFile
                         <*> pScriptRedeemerOrFile
                         <*> pExecutionUnits)
  where
    toScriptWitnessFiles :: ScriptFile
                         -> Maybe (ScriptDatumOrFile witctx,
                                   ScriptRedeemerOrFile,
                                   ExecutionUnits)
                         -> ScriptWitnessFiles witctx
    toScriptWitnessFiles sf Nothing        = SimpleScriptWitnessFile  sf
    toScriptWitnessFiles sf (Just (d,r,e)) = PlutusScriptWitnessFiles sf d r e

    pScriptDatumOrFile :: Parser ann (ScriptDatumOrFile witctx)
    pScriptDatumOrFile =
      case witctx of
        WitCtxTxIn  -> ScriptDatumOrFileForTxIn <$>
                         pScriptDataOrFile (scriptFlagPrefix ++ "-datum")
        WitCtxMint  -> pure NoScriptDatumOrFileForMint
        WitCtxStake -> pure NoScriptDatumOrFileForStake

    pScriptRedeemerOrFile :: Parser ann ScriptDataOrFile
    pScriptRedeemerOrFile = pScriptDataOrFile (scriptFlagPrefix ++ "-redeemer")

    pExecutionUnits :: Parser ann ExecutionUnits
    pExecutionUnits =
      uncurry ExecutionUnits <$>
      Opt.option Opt.auto
        (  Opt.long (scriptFlagPrefix ++ "-execution-units")
        <> Opt.metavar "(INT, INT)"
        <> Opt.help "The time and space units needed by the script."
        )

pScriptDataOrFile :: String -> Parser ann ScriptDataOrFile
pScriptDataOrFile dataFlagPrefix =
      ScriptDataFile  <$> pScriptDataFile
  <|> ScriptDataValue <$> pScriptDataValue
  where
    pScriptDataFile =
      Opt.strOption
        (  Opt.long (dataFlagPrefix ++ "-file")
        <> Opt.metavar "FILE"
        <> Opt.help "The JSON file containing the script data."
        )

    pScriptDataValue =
      Opt.option readerScriptData
        (  Opt.long (dataFlagPrefix ++ "-value")
        <> Opt.metavar "JSON VALUE"
        <> Opt.help "The JSON value for the script data. Supported JSON data types: string, number, object & array."
        )

    readerScriptData = do
      v <- readerJSON
      case scriptDataFromJson ScriptDataJsonNoSchema v of
        Left err -> fail (displayError err)
        Right sd -> return sd


pStakeAddressCmd :: Parser ann StakeAddressCmd
pStakeAddressCmd =
    asum
      [ subParser "key-gen"
          (Opt.info pStakeAddressKeyGen $ Opt.progDesc "Create a stake address key pair")
      , subParser "build"
          (Opt.info pStakeAddressBuild $ Opt.progDesc "Build a stake address")
      , subParser "key-hash"
          (Opt.info pStakeAddressKeyHash $ Opt.progDesc "Print the hash of a stake address key.")
      , subParser "registration-certificate"
          (Opt.info pStakeAddressRegistrationCert $ Opt.progDesc "Create a stake address registration certificate")
      , subParser "deregistration-certificate"
          (Opt.info pStakeAddressDeregistrationCert $ Opt.progDesc "Create a stake address deregistration certificate")
      , subParser "delegation-certificate"
          (Opt.info pStakeAddressDelegationCert $ Opt.progDesc "Create a stake address delegation certificate")
      ]
  where
    pStakeAddressKeyGen :: Parser ann StakeAddressCmd
    pStakeAddressKeyGen = StakeAddressKeyGen
                            <$> pVerificationKeyFile Output
                            <*> pSigningKeyFile Output

    pStakeAddressKeyHash :: Parser ann StakeAddressCmd
    pStakeAddressKeyHash = StakeAddressKeyHash <$> pStakeVerificationKeyOrFile <*> pMaybeOutputFile

    pStakeAddressBuild :: Parser ann StakeAddressCmd
    pStakeAddressBuild = StakeAddressBuild <$> pStakeVerificationKeyOrFile
                                           <*> pNetworkId
                                           <*> pMaybeOutputFile

    pStakeAddressRegistrationCert :: Parser ann StakeAddressCmd
    pStakeAddressRegistrationCert = StakeRegistrationCert
                                      <$> pStakeVerifier
                                      <*> pOutputFile

    pStakeAddressDeregistrationCert :: Parser ann StakeAddressCmd
    pStakeAddressDeregistrationCert = StakeCredentialDeRegistrationCert
                                        <$> pStakeVerifier
                                        <*> pOutputFile

    pStakeAddressDelegationCert :: Parser ann StakeAddressCmd
    pStakeAddressDelegationCert = StakeCredentialDelegationCert
                                    <$> pStakeVerifier
                                    <*> pStakePoolVerificationKeyOrHashOrFile
                                    <*> pOutputFile

pKeyCmd :: Parser ann KeyCmd
pKeyCmd =
  asum
    [ subParser "verification-key" $
        Opt.info pKeyGetVerificationKey $
          Opt.progDesc $ "Get a verification key from a signing key. This "
                      ++ " supports all key types."
    , subParser "non-extended-key" $
        Opt.info pKeyNonExtendedKey $
          Opt.progDesc $ "Get a non-extended verification key from an "
                      ++ "extended verification key. This supports all "
                      ++ "extended key types."
    , subParser "convert-byron-key" $
        Opt.info pKeyConvertByronKey $
          Opt.progDesc $ "Convert a Byron payment, genesis or genesis "
                      ++ "delegate key (signing or verification) to a "
                      ++ "corresponding Shelley-format key."
    , subParser "convert-byron-genesis-vkey" $
        Opt.info pKeyConvertByronGenesisVKey $
          Opt.progDesc $ "Convert a Base64-encoded Byron genesis "
                      ++ "verification key to a Shelley genesis "
                      ++ "verification key"
    , subParser "convert-itn-key" $
        Opt.info pKeyConvertITNKey $
          Opt.progDesc $ "Convert an Incentivized Testnet (ITN) non-extended "
                      ++ "(Ed25519) signing or verification key to a "
                      ++ "corresponding Shelley stake key"
    , subParser "convert-itn-extended-key" $
        Opt.info pKeyConvertITNExtendedKey $
          Opt.progDesc $ "Convert an Incentivized Testnet (ITN) extended "
                      ++ "(Ed25519Extended) signing key to a corresponding "
                      ++ "Shelley stake signing key"
    , subParser "convert-itn-bip32-key" $
        Opt.info pKeyConvertITNBip32Key $
          Opt.progDesc $ "Convert an Incentivized Testnet (ITN) BIP32 "
                      ++ "(Ed25519Bip32) signing key to a corresponding "
                      ++ "Shelley stake signing key"
    , subParser "convert-cardano-address-key" $
        Opt.info pKeyConvertCardanoAddressSigningKey $
          Opt.progDesc $ "Convert a cardano-address extended signing key "
                      ++ "to a corresponding Shelley-format key."
    ]
  where
    pKeyGetVerificationKey :: Parser ann KeyCmd
    pKeyGetVerificationKey =
      KeyGetVerificationKey
        <$> pSigningKeyFile      Input
        <*> pVerificationKeyFile Output

    pKeyNonExtendedKey :: Parser ann KeyCmd
    pKeyNonExtendedKey =
      KeyNonExtendedKey
        <$> pExtendedVerificationKeyFile Input
        <*> pVerificationKeyFile Output

    pKeyConvertByronKey :: Parser ann KeyCmd
    pKeyConvertByronKey =
      KeyConvertByronKey
        <$> optional pPassword
        <*> pByronKeyType
        <*> pByronKeyFile
        <*> pOutputFile

    pPassword :: Parser ann Text
    pPassword = Opt.strOption
                  (  Opt.long "password"
                  <> Opt.metavar "TEXT"
                  <> Opt.help "Password for signing key (if applicable)."
                  )

    pByronKeyType :: Parser ann ByronKeyType
    pByronKeyType =
          Opt.flag' (ByronPaymentKey NonLegacyByronKeyFormat)
            (  Opt.long "byron-payment-key-type"
            <> Opt.help "Use a Byron-era payment key."
            )
      <|> Opt.flag' (ByronPaymentKey LegacyByronKeyFormat)
            (  Opt.long "legacy-byron-payment-key-type"
            <> Opt.help "Use a Byron-era payment key, in legacy SL format."
            )
      <|> Opt.flag' (ByronGenesisKey NonLegacyByronKeyFormat)
            (  Opt.long "byron-genesis-key-type"
            <> Opt.help "Use a Byron-era genesis key."
            )
      <|> Opt.flag' (ByronGenesisKey LegacyByronKeyFormat)
            (  Opt.long "legacy-byron-genesis-key-type"
            <> Opt.help "Use a Byron-era genesis key, in legacy SL format."
            )
      <|> Opt.flag' (ByronDelegateKey NonLegacyByronKeyFormat)
            (  Opt.long "byron-genesis-delegate-key-type"
            <> Opt.help "Use a Byron-era genesis delegate key."
            )
      <|> Opt.flag' (ByronDelegateKey LegacyByronKeyFormat)
            (  Opt.long "legacy-byron-genesis-delegate-key-type"
            <> Opt.help "Use a Byron-era genesis delegate key, in legacy SL format."
            )

    pByronKeyFile :: Parser ann SomeKeyFile
    pByronKeyFile =
          (ASigningKeyFile      <$> pByronSigningKeyFile)
      <|> (AVerificationKeyFile <$> pByronVerificationKeyFile)

    pByronSigningKeyFile :: Parser ann SigningKeyFile
    pByronSigningKeyFile =
      SigningKeyFile <$>
        Opt.strOption
          (  Opt.long "byron-signing-key-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input filepath of the Byron-format signing key."
          <> Opt.completer (Opt.bashCompleter "file")
          )

    pByronVerificationKeyFile :: Parser ann VerificationKeyFile
    pByronVerificationKeyFile =
      VerificationKeyFile <$>
        Opt.strOption
          (  Opt.long "byron-verification-key-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input filepath of the Byron-format verification key."
          <> Opt.completer (Opt.bashCompleter "file")
          )

    pKeyConvertByronGenesisVKey :: Parser ann KeyCmd
    pKeyConvertByronGenesisVKey =
      KeyConvertByronGenesisVKey
        <$> pByronGenesisVKeyBase64
        <*> pOutputFile

    pByronGenesisVKeyBase64 :: Parser ann VerificationKeyBase64
    pByronGenesisVKeyBase64 =
      VerificationKeyBase64 <$>
        Opt.strOption
          (  Opt.long "byron-genesis-verification-key"
          <> Opt.metavar "BASE64"
          <> Opt.help "Base64 string for the Byron genesis verification key."
          )

    pKeyConvertITNKey :: Parser ann KeyCmd
    pKeyConvertITNKey =
      KeyConvertITNStakeKey
        <$> pITNKeyFIle
        <*> pOutputFile

    pKeyConvertITNExtendedKey :: Parser ann KeyCmd
    pKeyConvertITNExtendedKey =
      KeyConvertITNExtendedToStakeKey
        <$> pITNSigningKeyFile
        <*> pOutputFile

    pKeyConvertITNBip32Key :: Parser ann KeyCmd
    pKeyConvertITNBip32Key =
      KeyConvertITNBip32ToStakeKey
        <$> pITNSigningKeyFile
        <*> pOutputFile

    pITNKeyFIle :: Parser ann SomeKeyFile
    pITNKeyFIle = pITNSigningKeyFile
              <|> pITNVerificationKeyFile

    pITNSigningKeyFile :: Parser ann SomeKeyFile
    pITNSigningKeyFile =
      ASigningKeyFile . SigningKeyFile <$>
        Opt.strOption
          (  Opt.long "itn-signing-key-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Filepath of the ITN signing key."
          <> Opt.completer (Opt.bashCompleter "file")
          )

    pITNVerificationKeyFile :: Parser ann SomeKeyFile
    pITNVerificationKeyFile =
      AVerificationKeyFile . VerificationKeyFile <$>
        Opt.strOption
          (  Opt.long "itn-verification-key-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Filepath of the ITN verification key."
          <> Opt.completer (Opt.bashCompleter "file")
          )

    pKeyConvertCardanoAddressSigningKey :: Parser ann KeyCmd
    pKeyConvertCardanoAddressSigningKey =
      KeyConvertCardanoAddressSigningKey
        <$> pCardanoAddressKeyType
        <*> pSigningKeyFile Input
        <*> pOutputFile

    pCardanoAddressKeyType :: Parser ann CardanoAddressKeyType
    pCardanoAddressKeyType =
          Opt.flag' CardanoAddressShelleyPaymentKey
            (  Opt.long "shelley-payment-key"
            <> Opt.help "Use a Shelley-era extended payment key."
            )
      <|> Opt.flag' CardanoAddressShelleyStakeKey
            (  Opt.long "shelley-stake-key"
            <> Opt.help "Use a Shelley-era extended stake key."
            )
      <|> Opt.flag' CardanoAddressIcarusPaymentKey
            (  Opt.long "icarus-payment-key"
            <> Opt.help "Use a Byron-era extended payment key formatted in the Icarus style."
            )
      <|> Opt.flag' CardanoAddressByronPaymentKey
            (  Opt.long "byron-payment-key"
            <> Opt.help "Use a Byron-era extended payment key formatted in the deprecated Byron style."
            )

pTransaction :: Parser ann TransactionCmd
pTransaction =
  asum
    [ subParser "build-raw"
        (Opt.info pTransactionBuild $ Opt.progDesc "Build a transaction (low-level, inconvenient)")
    , subParser "sign"
        (Opt.info pTransactionSign $ Opt.progDesc "Sign a transaction")
    , subParser "witness"
        (Opt.info pTransactionCreateWitness $ Opt.progDesc "Create a transaction witness")
    , subParser "assemble"
        (Opt.info pTransactionAssembleTxBodyWit
          $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction")
    , pSignWitnessBackwardCompatible
    , subParser "submit"
        (Opt.info pTransactionSubmit . Opt.progDesc $
           mconcat
             [ "Submit a transaction to the local node whose Unix domain socket "
             , "is obtained from the CARDANO_NODE_SOCKET_PATH enviromnent variable."
             ]
          )
    , subParser "policyid"
        (Opt.info pTransactionPolicyId $ Opt.progDesc "Calculate the PolicyId from the monetary policy script.")
    , subParser "calculate-min-fee"
        (Opt.info pTransactionCalculateMinFee $ Opt.progDesc "Calculate the minimum fee for a transaction")
    , subParser "calculate-min-value"
        (Opt.info pTransactionCalculateMinValue $ Opt.progDesc "Calculate the minimum value for a transaction")

    , subParser "hash-script-data"
        (Opt.info pTxHashScriptData $ Opt.progDesc "Calculate the hash of script data")
    , subParser "txid"
        (Opt.info pTransactionId $ Opt.progDesc "Print a transaction identifier")
    , subParser "view" $
        Opt.info pTransactionView $ Opt.progDesc "Print a transaction"
    ]
 where
  assembleInfo :: ParserInfo ann TransactionCmd
  assembleInfo =
    Opt.info pTransactionAssembleTxBodyWit
      $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

  pSignWitnessBackwardCompatible :: Parser ann TransactionCmd
  pSignWitnessBackwardCompatible =
    Opt.subparser
      $ Opt.command "sign-witness" assembleInfo <> Opt.internal

  pTransactionBuild :: Parser ann TransactionCmd
  pTransactionBuild = TxBuildRaw <$> pCardanoEra
                                 <*> some pTxIn
                                 <*> many pTxInCollateral
                                 <*> many pTxOut
                                 <*> optional pMintMultiAsset
                                 <*> optional pInvalidBefore
                                 <*> optional pInvalidHereafter
                                 <*> optional pTxFee
                                 <*> many pCertificateFile
                                 <*> many pWithdrawal
                                 <*> pTxMetadataJsonSchema
                                 <*> many (pScriptFor
                                             "auxiliary-script-file"
                                             Nothing
                                             "Filepath of auxiliary script(s)")
                                 <*> many pMetadataFile
                                 <*> optional pProtocolParamsSourceSpec
                                 <*> optional pUpdateProposalFile
                                 <*> pTxBodyFile Output

  pTransactionSign  :: Parser ann TransactionCmd
  pTransactionSign = TxSign <$> pTxBodyFile Input
                            <*> pSomeWitnessSigningData
                            <*> optional pNetworkId
                            <*> pTxFile Output

  pTransactionCreateWitness :: Parser ann TransactionCmd
  pTransactionCreateWitness = TxCreateWitness
                                <$> pTxBodyFile Input
                                <*> pWitnessSigningData
                                <*> optional pNetworkId
                                <*> pOutputFile

  pTransactionAssembleTxBodyWit :: Parser ann TransactionCmd
  pTransactionAssembleTxBodyWit = TxAssembleTxBodyWitness
                                    <$> pTxBodyFile Input
                                    <*> some pWitnessFile
                                    <*> pOutputFile

  pTransactionSubmit :: Parser ann TransactionCmd
  pTransactionSubmit = TxSubmit <$> pConsensusModeParams
                                <*> pNetworkId
                                <*> pTxSubmitFile

  pTransactionPolicyId :: Parser ann TransactionCmd
  pTransactionPolicyId = TxMintedPolicyId <$> pScript

  pTransactionCalculateMinFee :: Parser ann TransactionCmd
  pTransactionCalculateMinFee =
    TxCalculateMinFee
      <$> pTxBodyFile Input
      <*> optional pNetworkId
      <*> pProtocolParamsSourceSpec
      <*> pTxInCount
      <*> pTxOutCount
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount

  pTransactionCalculateMinValue :: Parser ann TransactionCmd
  pTransactionCalculateMinValue = TxCalculateMinValue
    <$> pProtocolParamsSourceSpec
    <*> pMultiAsset

  pProtocolParamsSourceSpec :: Parser ann ProtocolParamsSourceSpec
  pProtocolParamsSourceSpec =
    ParamsFromGenesis <$>
      pGenesisFile
        "[TESTING] The genesis file to take initial protocol parameters from.  For test clusters only, since the parameters are going to be obsolete for production clusters."
    <|>
    ParamsFromFile <$> pProtocolParamsFile

  pTxHashScriptData :: Parser ann TransactionCmd
  pTxHashScriptData = TxHashScriptData <$> pScriptDataOrFile "script-data"

  pTransactionId  :: Parser ann TransactionCmd
  pTransactionId = TxGetTxId <$> pInputTxFile

  pTransactionView :: Parser ann TransactionCmd
  pTransactionView = TxView <$> pInputTxFile

pNodeCmd :: Parser ann NodeCmd
pNodeCmd =
  asum
    [ subParser "key-gen"
        (Opt.info pKeyGenOperator $
           Opt.progDesc "Create a key pair for a node operator's offline \
                       \ key and a new certificate issue counter")
    , subParser "key-gen-KES"
        (Opt.info pKeyGenKES $
           Opt.progDesc "Create a key pair for a node KES operational key")
    , subParser "key-gen-VRF"
        (Opt.info pKeyGenVRF $
           Opt.progDesc "Create a key pair for a node VRF operational key")
    , subParser "key-hash-VRF"
        (Opt.info pKeyHashVRF $
           Opt.progDesc "Print hash of a node's operational VRF key.")
    , subParser "new-counter"
        (Opt.info pNewCounter $
           Opt.progDesc "Create a new certificate issue counter")
    , subParser "issue-op-cert"
        (Opt.info pIssueOpCert $
           Opt.progDesc "Issue a node operational certificate")
    ]
  where
    pKeyGenOperator :: Parser ann NodeCmd
    pKeyGenOperator =
      NodeKeyGenCold <$> pColdVerificationKeyFile
                     <*> pColdSigningKeyFile
                     <*> pOperatorCertIssueCounterFile

    pKeyGenKES :: Parser ann NodeCmd
    pKeyGenKES =
      NodeKeyGenKES <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pKeyGenVRF :: Parser ann NodeCmd
    pKeyGenVRF =
      NodeKeyGenVRF <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pKeyHashVRF :: Parser ann NodeCmd
    pKeyHashVRF =
      NodeKeyHashVRF <$> pVerificationKeyOrFile AsVrfKey <*> pMaybeOutputFile

    pNewCounter :: Parser ann NodeCmd
    pNewCounter =
      NodeNewCounter <$> pColdVerificationKeyOrFile
                     <*> pCounterValue
                     <*> pOperatorCertIssueCounterFile

    pCounterValue :: Parser ann Word
    pCounterValue =
        Opt.option Opt.auto
          (  Opt.long "counter-value"
          <> Opt.metavar "INT"
          <> Opt.help "The next certificate issue counter value to use."
          )

    pIssueOpCert :: Parser ann NodeCmd
    pIssueOpCert =
      NodeIssueOpCert <$> pKesVerificationKeyOrFile
                      <*> pColdSigningKeyFile
                      <*> pOperatorCertIssueCounterFile
                      <*> pKesPeriod
                      <*> pOutputFile


pPoolCmd :: Parser ann PoolCmd
pPoolCmd =
  asum
      [ subParser "registration-certificate"
          (Opt.info pStakePoolRegistrationCert $ Opt.progDesc "Create a stake pool registration certificate")
      , subParser "deregistration-certificate"
          (Opt.info pStakePoolRetirementCert $ Opt.progDesc "Create a stake pool deregistration certificate")
      , subParser "id"
          (Opt.info pId $
             Opt.progDesc "Build pool id from the offline key")
      , subParser "metadata-hash"
          (Opt.info pPoolMetadataHashSubCmd $ Opt.progDesc "Print the hash of pool metadata.")
      ]
  where
    pId :: Parser ann PoolCmd
    pId = PoolGetId <$> pStakePoolVerificationKeyOrFile <*> pOutputFormat

    pPoolMetadataHashSubCmd :: Parser ann PoolCmd
    pPoolMetadataHashSubCmd = PoolMetadataHash <$> pPoolMetadataFile <*> pMaybeOutputFile


pQueryCmd :: Parser ann QueryCmd
pQueryCmd =
  asum
    [ subParser "protocol-parameters"
        (Opt.info pQueryProtocolParameters $ Opt.progDesc "Get the node's current protocol parameters")
    , subParser "tip"
        (Opt.info pQueryTip $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)")
    , subParser "stake-distribution"
        (Opt.info pQueryStakeDistribution $ Opt.progDesc "Get the node's current aggregated stake distribution")
    , subParser "stake-address-info"
        (Opt.info pQueryStakeAddressInfo $ Opt.progDesc "Get the current delegations and \
                                                        \reward accounts filtered by stake \
                                                        \address.")
    , subParser "utxo"
        (Opt.info pQueryUTxO $ Opt.progDesc "Get a portion of the current UTxO: \
                                            \by tx in, by address or the whole.")
    , subParser "ledger-state"
        (Opt.info pQueryLedgerState $ Opt.progDesc "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)")
    , subParser "protocol-state"
        (Opt.info pQueryProtocolState $ Opt.progDesc "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)")
    , subParser "stake-snapshot"
        (Opt.info pQueryStakeSnapshot $ Opt.progDesc "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)")
    , subParser "pool-params"
        (Opt.info pQueryPoolParams $ Opt.progDesc "Dump the pool parameters (Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)")
    ]
  where
    pQueryProtocolParameters :: Parser ann QueryCmd
    pQueryProtocolParameters =
      QueryProtocolParameters'
        <$> pConsensusModeParams
        <*> pNetworkId
        <*> pMaybeOutputFile

    pQueryTip :: Parser ann QueryCmd
    pQueryTip = QueryTip
                  <$> pConsensusModeParams
                  <*> pNetworkId
                  <*> pMaybeOutputFile

    pQueryUTxO :: Parser ann QueryCmd
    pQueryUTxO =
      QueryUTxO'
        <$> pConsensusModeParams
        <*> pQueryUTxOFilter
        <*> pNetworkId
        <*> pMaybeOutputFile

    pQueryStakeDistribution :: Parser ann QueryCmd
    pQueryStakeDistribution =
      QueryStakeDistribution'
        <$> pConsensusModeParams
        <*> pNetworkId
        <*> pMaybeOutputFile

    pQueryStakeAddressInfo :: Parser ann QueryCmd
    pQueryStakeAddressInfo =
      QueryStakeAddressInfo
        <$> pConsensusModeParams
        <*> pFilterByStakeAddress
        <*> pNetworkId
        <*> pMaybeOutputFile

    pQueryLedgerState :: Parser ann QueryCmd
    pQueryLedgerState = QueryDebugLedgerState'
                          <$> pConsensusModeParams
                          <*> pNetworkId
                          <*> pMaybeOutputFile

    pQueryProtocolState :: Parser ann QueryCmd
    pQueryProtocolState = QueryProtocolState'
                            <$> pConsensusModeParams
                            <*> pNetworkId
                            <*> pMaybeOutputFile

    pQueryStakeSnapshot :: Parser ann QueryCmd
    pQueryStakeSnapshot = QueryStakeSnapshot'
      <$> pConsensusModeParams
      <*> pNetworkId
      <*> pStakePoolVerificationKeyHash

    pQueryPoolParams :: Parser ann QueryCmd
    pQueryPoolParams = QueryPoolParams'
      <$> pConsensusModeParams
      <*> pNetworkId
      <*> pStakePoolVerificationKeyHash


pGovernanceCmd :: Parser ann GovernanceCmd
pGovernanceCmd =
 asum
   [ subParser "create-mir-certificate"
       (Opt.info (pMIRPayStakeAddresses <|> mirCertParsers) $
         Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate")
   , subParser "create-genesis-key-delegation-certificate"
       (Opt.info pGovernanceGenesisKeyDelegationCertificate $
         Opt.progDesc "Create a genesis key delegation certificate")
   , subParser "create-update-proposal"
       (Opt.info pUpdateProposal $
         Opt.progDesc "Create an update proposal")
   ]
  where
    mirCertParsers :: Parser ann GovernanceCmd
    mirCertParsers = asum
      [ subParser "stake-addresses" (Opt.info pMIRPayStakeAddresses $
          Opt.progDesc "Create an MIR certificate to pay stake addresses")
      , subParser "transfer-to-treasury" (Opt.info pMIRTransferToTreasury $
          Opt.progDesc "Create an MIR certificate to transfer from the reserves pot\
                       \ to the treasury pot")
      , subParser "transfer-to-rewards" (Opt.info pMIRTransferToReserves $
          Opt.progDesc "Create an MIR certificate to transfer from the treasury pot\
                       \ to the reserves pot")
      ]

    pMIRPayStakeAddresses :: Parser ann GovernanceCmd
    pMIRPayStakeAddresses = GovernanceMIRPayStakeAddressesCertificate
                              <$> pMIRPot
                              <*> some pStakeAddress
                              <*> some pRewardAmt
                              <*> pOutputFile

    pMIRTransferToTreasury :: Parser ann GovernanceCmd
    pMIRTransferToTreasury = GovernanceMIRTransfer
                               <$> pTransferAmt
                               <*> pOutputFile
                               <*> pure TransferToTreasury

    pMIRTransferToReserves :: Parser ann GovernanceCmd
    pMIRTransferToReserves = GovernanceMIRTransfer
                               <$> pTransferAmt
                               <*> pOutputFile
                               <*> pure TransferToReserves

    pGovernanceGenesisKeyDelegationCertificate :: Parser ann GovernanceCmd
    pGovernanceGenesisKeyDelegationCertificate =
      GovernanceGenesisKeyDelegationCertificate
        <$> pGenesisVerificationKeyOrHashOrFile
        <*> pGenesisDelegateVerificationKeyOrHashOrFile
        <*> pVrfVerificationKeyOrHashOrFile
        <*> pOutputFile

    pMIRPot :: Parser ann Shelley.MIRPot
    pMIRPot =
          Opt.flag' Shelley.ReservesMIR
            (  Opt.long "reserves"
            <> Opt.help "Use the reserves pot."
            )
      <|> Opt.flag' Shelley.TreasuryMIR
            (  Opt.long "treasury"
            <> Opt.help "Use the treasury pot."
            )

    pUpdateProposal :: Parser ann GovernanceCmd
    pUpdateProposal = GovernanceUpdateProposal
                        <$> pOutputFile
                        <*> pEpochNoUpdateProp
                        <*> some pGenesisVerificationKeyFile
                        <*> pProtocolParametersUpdate

pTransferAmt :: Parser ann Lovelace
pTransferAmt =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "transfer"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The amount to transfer."
      )

pRewardAmt :: Parser ann Lovelace
pRewardAmt =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "reward"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The reward for the relevant reward account."
      )

pGenesisCmd :: Parser ann GenesisCmd
pGenesisCmd =
  asum
    [ subParser "key-gen-genesis"
        (Opt.info pGenesisKeyGen $
           Opt.progDesc "Create a Shelley genesis key pair")
    , subParser "key-gen-delegate"
        (Opt.info pGenesisDelegateKeyGen $
           Opt.progDesc "Create a Shelley genesis delegate key pair")
    , subParser "key-gen-utxo"
        (Opt.info pGenesisUTxOKeyGen $
           Opt.progDesc "Create a Shelley genesis UTxO key pair")
    , subParser "key-hash"
        (Opt.info pGenesisKeyHash $
           Opt.progDesc "Print the identifier (hash) of a public key")
    , subParser "get-ver-key"
        (Opt.info pGenesisVerKey $
           Opt.progDesc "Derive the verification key from a signing key")
    , subParser "initial-addr"
        (Opt.info pGenesisAddr $
           Opt.progDesc "Get the address for an initial UTxO based on the verification key")
    , subParser "initial-txin"
        (Opt.info pGenesisTxIn $
           Opt.progDesc "Get the TxIn for an initial UTxO based on the verification key")
    , subParser "create"
        (Opt.info pGenesisCreate $
           Opt.progDesc ("Create a Shelley genesis file from a genesis "
                      ++ "template and genesis/delegation/spending keys."))
    , subParser "create-staked"
        (Opt.info pGenesisCreateStaked $
           Opt.progDesc ("Create a staked Shelley genesis file from a genesis "
                      ++ "template and genesis/delegation/spending keys."))
    , subParser "hash"
        (Opt.info pGenesisHash $
           Opt.progDesc "Compute the hash of a genesis file")
    ]
  where
    pGenesisKeyGen :: Parser ann GenesisCmd
    pGenesisKeyGen =
      GenesisKeyGenGenesis <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pGenesisDelegateKeyGen :: Parser ann GenesisCmd
    pGenesisDelegateKeyGen =
      GenesisKeyGenDelegate <$> pVerificationKeyFile Output
                            <*> pSigningKeyFile Output
                            <*> pOperatorCertIssueCounterFile

    pGenesisUTxOKeyGen :: Parser ann GenesisCmd
    pGenesisUTxOKeyGen =
      GenesisKeyGenUTxO <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pGenesisKeyHash :: Parser ann GenesisCmd
    pGenesisKeyHash =
      GenesisCmdKeyHash <$> pVerificationKeyFile Input

    pGenesisVerKey :: Parser ann GenesisCmd
    pGenesisVerKey =
      GenesisVerKey <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pGenesisAddr :: Parser ann GenesisCmd
    pGenesisAddr =
      GenesisAddr <$> pVerificationKeyFile Input <*> pNetworkId <*> pMaybeOutputFile

    pGenesisTxIn :: Parser ann GenesisCmd
    pGenesisTxIn =
      GenesisTxIn <$> pVerificationKeyFile Input <*> pNetworkId <*> pMaybeOutputFile

    pGenesisCreate :: Parser ann GenesisCmd
    pGenesisCreate =
      GenesisCreate <$> pGenesisDir
                    <*> pGenesisNumGenesisKeys
                    <*> pGenesisNumUTxOKeys
                    <*> pMaybeSystemStart
                    <*> pInitialSupplyNonDelegated
                    <*> pNetworkId

    pGenesisCreateStaked :: Parser ann GenesisCmd
    pGenesisCreateStaked =
      GenesisCreateStaked
        <$> pGenesisDir
        <*> pGenesisNumGenesisKeys
        <*> pGenesisNumUTxOKeys
        <*> pGenesisNumPools
        <*> pGenesisNumStDelegs
        <*> pMaybeSystemStart
        <*> pInitialSupplyNonDelegated
        <*> pInitialSupplyDelegated
        <*> pNetworkId
        <*> pBulkPoolCredFiles
        <*> pBulkPoolsPerFile
        <*> pStuffedUtxoCount

    pGenesisHash :: Parser ann GenesisCmd
    pGenesisHash =
      GenesisHashFile <$> pGenesisFile "The genesis file."

    pGenesisDir :: Parser ann GenesisDir
    pGenesisDir =
      GenesisDir <$>
        Opt.strOption
          (  Opt.long "genesis-dir"
          <> Opt.metavar "DIR"
          <> Opt.help "The genesis directory containing the genesis template and required genesis/delegation/spending keys."
          )

    pMaybeSystemStart :: Parser ann (Maybe SystemStart)
    pMaybeSystemStart =
      Opt.optional $
        SystemStart . convertTime <$>
          Opt.strOption
            (  Opt.long "start-time"
            <> Opt.metavar "UTC-TIME"
            <> Opt.help "The genesis start time in YYYY-MM-DDThh:mm:ssZ format. If unspecified, will be the current time +30 seconds."
            )

    pGenesisNumGenesisKeys :: Parser ann Word
    pGenesisNumGenesisKeys =
        Opt.option Opt.auto
          (  Opt.long "gen-genesis-keys"
          <> Opt.metavar "INT"
          <> Opt.help "The number of genesis keys to make [default is 0]."
          <> Opt.value 0
          )

    pGenesisNumUTxOKeys :: Parser ann Word
    pGenesisNumUTxOKeys =
        Opt.option Opt.auto
          (  Opt.long "gen-utxo-keys"
          <> Opt.metavar "INT"
          <> Opt.help "The number of UTxO keys to make [default is 0]."
          <> Opt.value 0
          )

    pGenesisNumPools :: Parser ann Word
    pGenesisNumPools =
        Opt.option Opt.auto
          (  Opt.long "gen-pools"
          <> Opt.metavar "INT"
          <> Opt.help "The number of stake pool credential sets to make [default is 0]."
          <> Opt.value 0
          )

    pGenesisNumStDelegs :: Parser ann Word
    pGenesisNumStDelegs =
        Opt.option Opt.auto
          (  Opt.long "gen-stake-delegs"
          <> Opt.metavar "INT"
          <> Opt.help "The number of stake delegator credential sets to make [default is 0]."
          <> Opt.value 0
          )

    pStuffedUtxoCount :: Parser ann Word
    pStuffedUtxoCount =
        Opt.option Opt.auto
          (  Opt.long "num-stuffed-utxo"
          <> Opt.metavar "INT"
          <> Opt.help "The number of fake UTxO entries to generate [default is 0]."
          <> Opt.value 0
          )

    convertTime :: String -> UTCTime
    convertTime =
      parseTimeOrError False defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")

    pInitialSupplyNonDelegated :: Parser ann (Maybe Lovelace)
    pInitialSupplyNonDelegated =
      Opt.optional $
      Lovelace <$>
        Opt.option Opt.auto
          (  Opt.long "supply"
          <> Opt.metavar "LOVELACE"
          <> Opt.help "The initial coin supply in Lovelace which will be evenly distributed across initial, non-delegating stake holders."
          )

    pInitialSupplyDelegated :: Parser ann Lovelace
    pInitialSupplyDelegated =
      fmap (Lovelace . fromMaybe 0) $ Opt.optional $
        Opt.option Opt.auto
          (  Opt.long "supply-delegated"
          <> Opt.metavar "LOVELACE"
          <> Opt.help "The initial coin supply in Lovelace which will be evenly distributed across initial, delegating stake holders."
          <> Opt.value 0
          )

    pBulkPoolCredFiles :: Parser ann Word
    pBulkPoolCredFiles =
        Opt.option Opt.auto
          (  Opt.long "bulk-pool-cred-files"
          <> Opt.metavar "INT"
          <> Opt.help "Generate bulk pool credential files [default is 0]."
          <> Opt.value 0
          )

    pBulkPoolsPerFile :: Parser ann Word
    pBulkPoolsPerFile =
        Opt.option Opt.auto
          (  Opt.long "bulk-pools-per-file"
          <> Opt.metavar "INT"
          <> Opt.help "Each bulk pool to contain this many pool credential sets [default is 0]."
          <> Opt.value 0
          )


--
-- Shelley CLI flag parsers
--

data FileDirection
  = Input
  | Output
  deriving (Eq, Show)

pAddressKeyType :: Parser ann AddressKeyType
pAddressKeyType =
    Opt.flag' AddressKeyShelley
      (  Opt.long "normal-key"
      <> Opt.help "Use a normal Shelley-era key (default)."
      )
  <|>
    Opt.flag' AddressKeyShelleyExtended
      (  Opt.long "extended-key"
      <> Opt.help "Use an extended ed25519 Shelley-era key."
      )
  <|>
    Opt.flag' AddressKeyByron
      (  Opt.long "byron-key"
      <> Opt.help "Use a Byron-era key."
      )
  <|>
    pure AddressKeyShelley


pProtocolParamsFile :: Parser ann ProtocolParamsFile
pProtocolParamsFile =
  ProtocolParamsFile <$>
    Opt.strOption
      (  Opt.long "protocol-params-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the JSON-encoded protocol parameters file"
      <> Opt.completer (Opt.bashCompleter "file")
      )

pCertificateFile :: Parser ann (CertificateFile,
                            Maybe (ScriptWitnessFiles WitCtxStake))
pCertificateFile =
  (,) <$> (CertificateFile
             <$> (  Opt.strOption
                      (  Opt.long "certificate-file"
                      <> Opt.metavar "CERTIFICATEFILE"
                      <> Opt.help helpText
                      <> Opt.completer (Opt.bashCompleter "file")
                      )
                  <|>
                     Opt.strOption (Opt.long "certificate" <> Opt.internal)
                  )
          )
      <*> optional (pScriptWitnessFiles
                      WitCtxStake
                      "certificate" Nothing
                      "the use of the certificate.")
 where
   helpText = "Filepath of the certificate. This encompasses all \
              \types of certificates (stake pool certificates, \
              \stake key certificates etc). Optionally specify a script witness."



pPoolMetadataFile :: Parser ann PoolMetadataFile
pPoolMetadataFile =
  PoolMetadataFile <$>
    Opt.strOption
      (  Opt.long "pool-metadata-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the pool metadata."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pTxMetadataJsonSchema :: Parser ann TxMetadataJsonSchema
pTxMetadataJsonSchema =
    (  Opt.flag' ()
        (  Opt.long "json-metadata-no-schema"
        <> Opt.help "Use the \"no schema\" conversion from JSON to tx metadata."
        )
    *> pure TxMetadataJsonNoSchema
    )
  <|>
    (  Opt.flag' ()
        (  Opt.long "json-metadata-detailed-schema"
        <> Opt.help "Use the \"detailed schema\" conversion from JSON to tx metadata."
        )
    *> pure TxMetadataJsonDetailedSchema
    )
  <|>
    -- Default to the no-schema conversion.
    pure TxMetadataJsonNoSchema

pMetadataFile :: Parser ann MetadataFile
pMetadataFile =
      MetadataFileJSON <$>
        ( Opt.strOption
            (  Opt.long "metadata-json-file"
            <> Opt.metavar "FILE"
            <> Opt.help "Filepath of the metadata file, in JSON format."
            <> Opt.completer (Opt.bashCompleter "file")
            )
        <|>
          Opt.strOption
            (  Opt.long "metadata-file" -- backward compat name
            <> Opt.internal
            )
        )
  <|>
      MetadataFileCBOR <$>
        Opt.strOption
          (  Opt.long "metadata-cbor-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Filepath of the metadata, in raw CBOR format."
          <> Opt.completer (Opt.bashCompleter "file")
          )

pWithdrawal :: Parser ann (StakeAddress,
                       Lovelace,
                       Maybe (ScriptWitnessFiles WitCtxStake))
pWithdrawal =
    (\(stakeAddr,lovelace) maybeScriptFp -> (stakeAddr, lovelace, maybeScriptFp))
      <$> Opt.option (readerFromParsecParser parseWithdrawal)
            (  Opt.long "withdrawal"
            <> Opt.metavar "WITHDRAWAL"
            <> Opt.help helpText
            )
      <*> optional (pScriptWitnessFiles
                      WitCtxStake
                      "withdrawal" Nothing
                      "the withdrawal of rewards.")
 where
   helpText = "The reward withdrawal as StakeAddress+Lovelace where \
              \StakeAddress is the Bech32-encoded stake address \
              \followed by the amount in Lovelace. Optionally specify \
              \a script witness."

   parseWithdrawal :: Parsec.Parser (StakeAddress, Lovelace)
   parseWithdrawal =
     (,) <$> parseStakeAddress <* Parsec.char '+' <*> parseLovelace


pUpdateProposalFile :: Parser ann UpdateProposalFile
pUpdateProposalFile =
  UpdateProposalFile <$>
  ( Opt.strOption
     (  Opt.long "update-proposal-file"
     <> Opt.metavar "FILE"
     <> Opt.help "Filepath of the update proposal."
     <> Opt.completer (Opt.bashCompleter "file")
     )
  <|>
    Opt.strOption
      (  Opt.long "update-proposal"
      <> Opt.internal
      )
  )


pColdSigningKeyFile :: Parser ann SigningKeyFile
pColdSigningKeyFile =
  SigningKeyFile <$>
    ( Opt.strOption
        (  Opt.long "cold-signing-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the cold signing key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
      (  Opt.long "signing-key-file"
      <> Opt.internal
      )
    )

pSomeWitnessSigningData :: Parser ann [WitnessSigningData]
pSomeWitnessSigningData =
  some $
      KeyWitnessSigningData
        <$>
          ( SigningKeyFile <$>
              Opt.strOption
                (  Opt.long "signing-key-file"
                <> Opt.metavar "FILE"
                <> Opt.help "Input filepath of the signing key (one or more)."
                <> Opt.completer (Opt.bashCompleter "file")
                )
          )
        <*>
          optional pByronAddress

pSigningKeyFile :: FileDirection -> Parser ann SigningKeyFile
pSigningKeyFile fdir =
  SigningKeyFile <$>
    Opt.strOption
      (  Opt.long "signing-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help (show fdir ++ " filepath of the signing key.")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pWitnessSigningData :: Parser ann WitnessSigningData
pWitnessSigningData =
    KeyWitnessSigningData
      <$>
        ( SigningKeyFile <$>
            Opt.strOption
              (  Opt.long "signing-key-file"
              <> Opt.metavar "FILE"
              <> Opt.help "Filepath of the signing key to be used in witness construction."
              <> Opt.completer (Opt.bashCompleter "file")
              )
        )
      <*>
        optional pByronAddress

pKesPeriod :: Parser ann KESPeriod
pKesPeriod =
  KESPeriod <$>
    Opt.option Opt.auto
      (  Opt.long "kes-period"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The start of the KES key validity period."
      )

pEpochNo :: Parser ann EpochNo
pEpochNo =
  EpochNo <$>
    Opt.option Opt.auto
      (  Opt.long "epoch"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The epoch number."
      )


pEpochNoUpdateProp :: Parser ann EpochNo
pEpochNoUpdateProp =
  EpochNo <$>
    Opt.option Opt.auto
      (  Opt.long "epoch"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The epoch number in which the update proposal is valid."
      )

pGenesisFile :: String -> Parser ann GenesisFile
pGenesisFile desc =
  GenesisFile <$>
    Opt.strOption
      (  Opt.long "genesis"
      <> Opt.metavar "FILE"
      <> Opt.help desc
      <> Opt.completer (Opt.bashCompleter "file")
      )

pOperatorCertIssueCounterFile :: Parser ann OpCertCounterFile
pOperatorCertIssueCounterFile =
  OpCertCounterFile <$>
    ( Opt.strOption
        (  Opt.long "operational-certificate-issue-counter-file"
        <> Opt.metavar "FILE"
        <> Opt.help "The file with the issue counter for the operational certificate."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
        (  Opt.long "operational-certificate-issue-counter"
        <> Opt.internal
        )
    )


pOutputFormat :: Parser ann OutputFormat
pOutputFormat =
  Opt.option readOutputFormat
    (  Opt.long "output-format"
    <> Opt.metavar "STRING"
    <> Opt.help "Optional output format. Accepted output formats are \"hex\" \
                \and \"bech32\" (default is \"bech32\")."
    <> Opt.value OutputFormatBech32
    )


pMaybeOutputFile :: Parser ann (Maybe OutputFile)
pMaybeOutputFile =
  optional $
    OutputFile <$>
      Opt.strOption
        (  Opt.long "out-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Optional output file. Default is to write to stdout."
        <> Opt.completer (Opt.bashCompleter "file")
        )

pOutputFile :: Parser ann OutputFile
pOutputFile =
  OutputFile <$>
    Opt.strOption
      (  Opt.long "out-file"
      <> Opt.metavar "FILE"
      <> Opt.help "The output file."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pColdVerificationKeyOrFile :: Parser ann ColdVerificationKeyOrFile
pColdVerificationKeyOrFile =
  ColdStakePoolVerificationKey <$> pStakePoolVerificationKey
    <|> ColdGenesisDelegateVerificationKey <$> pGenesisDelegateVerificationKey
    <|> ColdVerificationKeyFile <$> pColdVerificationKeyFile

pColdVerificationKeyFile :: Parser ann VerificationKeyFile
pColdVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "cold-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the cold verification key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
        (  Opt.long "verification-key-file"
        <> Opt.internal
        )
    )

pVerificationKey
  :: forall keyrole ann.
     SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Parser ann (VerificationKey keyrole)
pVerificationKey asType =
  Opt.option
    (readVerificationKey asType)
      (  Opt.long "verification-key"
      <> Opt.metavar "STRING"
      <> Opt.help "Verification key (Bech32 or hex-encoded)."
      )

pVerificationKeyOrFile
  :: SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Parser ann (VerificationKeyOrFile keyrole)
pVerificationKeyOrFile asType =
  VerificationKeyValue <$> pVerificationKey asType
    <|> VerificationKeyFilePath <$> pVerificationKeyFile Input

pVerificationKeyFile :: FileDirection -> Parser ann VerificationKeyFile
pVerificationKeyFile fdir =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help (show fdir ++ " filepath of the verification key.")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pExtendedVerificationKeyFile :: FileDirection -> Parser ann VerificationKeyFile
pExtendedVerificationKeyFile fdir =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "extended-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help (show fdir ++ " filepath of the ed25519-bip32 verification key.")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pGenesisVerificationKeyFile :: Parser ann VerificationKeyFile
pGenesisVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "genesis-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the genesis verification key."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pGenesisVerificationKeyHash :: Parser ann (Hash GenesisKey)
pGenesisVerificationKeyHash =
    Opt.option
      (Opt.eitherReader deserialiseFromHex)
        (  Opt.long "genesis-verification-key-hash"
        <> Opt.metavar "STRING"
        <> Opt.help "Genesis verification key hash (hex-encoded)."
        )
  where
    deserialiseFromHex :: String -> Either String (Hash GenesisKey)
    deserialiseFromHex =
      maybe (Left "Invalid genesis verification key hash.") Right
        . deserialiseFromRawBytesHex (AsHash AsGenesisKey)
        . BSC.pack

pGenesisVerificationKey :: Parser ann (VerificationKey GenesisKey)
pGenesisVerificationKey =
    Opt.option
      (Opt.eitherReader deserialiseFromHex)
        (  Opt.long "genesis-verification-key"
        <> Opt.metavar "STRING"
        <> Opt.help "Genesis verification key (hex-encoded)."
        )
  where
    deserialiseFromHex :: String -> Either String (VerificationKey GenesisKey)
    deserialiseFromHex =
      maybe (Left "Invalid genesis verification key.") Right
        . deserialiseFromRawBytesHex (AsVerificationKey AsGenesisKey)
        . BSC.pack

pGenesisVerificationKeyOrFile :: Parser ann (VerificationKeyOrFile GenesisKey)
pGenesisVerificationKeyOrFile =
  VerificationKeyValue <$> pGenesisVerificationKey
    <|> VerificationKeyFilePath <$> pGenesisVerificationKeyFile

pGenesisVerificationKeyOrHashOrFile :: Parser ann (VerificationKeyOrHashOrFile GenesisKey)
pGenesisVerificationKeyOrHashOrFile =
  VerificationKeyOrFile <$> pGenesisVerificationKeyOrFile
    <|> VerificationKeyHash <$> pGenesisVerificationKeyHash

pGenesisDelegateVerificationKeyFile :: Parser ann VerificationKeyFile
pGenesisDelegateVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "genesis-delegate-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the genesis delegate verification key."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pGenesisDelegateVerificationKeyHash :: Parser ann (Hash GenesisDelegateKey)
pGenesisDelegateVerificationKeyHash =
    Opt.option
      (Opt.eitherReader deserialiseFromHex)
        (  Opt.long "genesis-delegate-verification-key-hash"
        <> Opt.metavar "STRING"
        <> Opt.help "Genesis delegate verification key hash (hex-encoded)."
        )
  where
    deserialiseFromHex :: String -> Either String (Hash GenesisDelegateKey)
    deserialiseFromHex =
      maybe (Left "Invalid genesis delegate verification key hash.") Right
        . deserialiseFromRawBytesHex (AsHash AsGenesisDelegateKey)
        . BSC.pack

pGenesisDelegateVerificationKey :: Parser ann (VerificationKey GenesisDelegateKey)
pGenesisDelegateVerificationKey =
    Opt.option
      (Opt.eitherReader deserialiseFromHex)
        (  Opt.long "genesis-delegate-verification-key"
        <> Opt.metavar "STRING"
        <> Opt.help "Genesis delegate verification key (hex-encoded)."
        )
  where
    deserialiseFromHex
      :: String
      -> Either String (VerificationKey GenesisDelegateKey)
    deserialiseFromHex =
      maybe (Left "Invalid genesis delegate verification key.") Right
        . deserialiseFromRawBytesHex (AsVerificationKey AsGenesisDelegateKey)
        . BSC.pack

pGenesisDelegateVerificationKeyOrFile
  :: Parser ann (VerificationKeyOrFile GenesisDelegateKey)
pGenesisDelegateVerificationKeyOrFile =
  VerificationKeyValue <$> pGenesisDelegateVerificationKey
    <|> VerificationKeyFilePath <$> pGenesisDelegateVerificationKeyFile

pGenesisDelegateVerificationKeyOrHashOrFile
  :: Parser ann (VerificationKeyOrHashOrFile GenesisDelegateKey)
pGenesisDelegateVerificationKeyOrHashOrFile =
  VerificationKeyOrFile <$> pGenesisDelegateVerificationKeyOrFile
    <|> VerificationKeyHash <$> pGenesisDelegateVerificationKeyHash

pKesVerificationKeyOrFile :: Parser ann (VerificationKeyOrFile KesKey)
pKesVerificationKeyOrFile =
  VerificationKeyValue <$> pKesVerificationKey
    <|> VerificationKeyFilePath <$> pKesVerificationKeyFile

pKesVerificationKey :: Parser ann (VerificationKey KesKey)
pKesVerificationKey =
    Opt.option
      (Opt.eitherReader deserialiseVerKey)
        (  Opt.long "kes-verification-key"
        <> Opt.metavar "STRING"
        <> Opt.help "A Bech32 or hex-encoded hot KES verification key."
        )
  where
    asType :: AsType (VerificationKey KesKey)
    asType = AsVerificationKey AsKesKey

    deserialiseVerKey :: String -> Either String (VerificationKey KesKey)
    deserialiseVerKey str =
      case deserialiseFromBech32 asType (Text.pack str) of
        Right res -> Right res

        -- The input was valid Bech32, but some other error occurred.
        Left err@(Bech32UnexpectedPrefix _ _) -> Left (displayError err)
        Left err@(Bech32DataPartToBytesError _) -> Left (displayError err)
        Left err@(Bech32DeserialiseFromBytesError _) -> Left (displayError err)
        Left err@(Bech32WrongPrefix _ _) -> Left (displayError err)

        -- The input was not valid Bech32. Attempt to deserialise it as hex.
        Left (Bech32DecodingError _) ->
          case deserialiseFromRawBytesHex asType (BSC.pack str) of
            Just res' -> Right res'
            Nothing -> Left "Invalid stake pool verification key."

pKesVerificationKeyFile :: Parser ann VerificationKeyFile
pKesVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "kes-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the hot KES verification key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
    Opt.strOption
        (  Opt.long "hot-kes-verification-key-file"
        <> Opt.internal
        )
    )

pNetworkId :: Parser ann NetworkId
pNetworkId =
  pMainnet <|> fmap Testnet pTestnetMagic
 where
   pMainnet :: Parser ann NetworkId
   pMainnet =
    Opt.flag' Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

pTestnetMagic :: Parser ann NetworkMagic
pTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

pTxSubmitFile :: Parser ann FilePath
pTxSubmitFile =
  Opt.strOption
    (  Opt.long "tx-file"
    <> Opt.metavar "FILE"
    <> Opt.help "Filepath of the transaction you intend to submit."
    <> Opt.completer (Opt.bashCompleter "file")
    )

pCardanoEra :: Parser ann AnyCardanoEra
pCardanoEra = asum
  [ Opt.flag' (AnyCardanoEra ByronEra)
      (  Opt.long "byron-era"
      <> Opt.help "Specify the Byron era"
      )
  , Opt.flag' (AnyCardanoEra ShelleyEra)
      (  Opt.long "shelley-era"
      <> Opt.help "Specify the Shelley era"
      )
  , Opt.flag' (AnyCardanoEra AllegraEra)
      (  Opt.long "allegra-era"
      <> Opt.help "Specify the Allegra era"
      )
  , Opt.flag' (AnyCardanoEra MaryEra)
      (  Opt.long "mary-era"
      <> Opt.help "Specify the Mary era (default)"
      )
  , Opt.flag' (AnyCardanoEra AlonzoEra)
      (  Opt.long "alonzo-era"
      <> Opt.help "Specify the Alonzo era"
      )

    -- Default for now:
  , pure (AnyCardanoEra MaryEra)
  ]

pTxIn :: Parser ann (TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))
pTxIn =
     (,) <$> Opt.option (readerFromParsecParser parseTxIn)
               (  Opt.long "tx-in"
                <> Opt.metavar "TX-IN"
               <> Opt.help "TxId#TxIx"
               )
         <*> optional (pScriptWitnessFiles
                         WitCtxTxIn
                         "tx-in" (Just "txin")
                         "the spending of the transaction input.")

pTxInCollateral :: Parser ann TxIn
pTxInCollateral =
    Opt.option (readerFromParsecParser parseTxIn)
      (  Opt.long "tx-in-collateral"
      <> Opt.metavar "TX-IN"
      <> Opt.help "TxId#TxIx"
      )

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

renderTxIn :: TxIn -> Text
renderTxIn (TxIn txid (TxIx txix)) =
  mconcat
    [ serialiseToRawBytesHexText txid
    , "#"
    , Text.pack (show txix)
    ]

parseTxId :: Parsec.Parser TxId
parseTxId = do
  str <- Parsec.many1 Parsec.hexDigit Parsec.<?> "transaction id (hexadecimal)"
  case deserialiseFromRawBytesHex AsTxId (BSC.pack str) of
    Just addr -> return addr
    Nothing -> fail $ "Incorrect transaction id format:: " ++ show str

parseTxIx :: Parsec.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> decimal


pTxOut :: Parser ann TxOutAnyEra
pTxOut =
        Opt.option (readerFromParsecParser parseTxOutAnyEra)
          (  Opt.long "tx-out"
          <> Opt.metavar "ADDRESS VALUE"
          -- TODO alonzo: Update the help text to describe the new syntax as well.
          <> Opt.help "The transaction output as Address+Lovelace where Address is \
                      \the Bech32-encoded address followed by the amount in \
                      \Lovelace."
          )
    <*> optional pDatumHash


pDatumHash :: Parser ann (Hash ScriptData)
pDatumHash  =
  Opt.option (readerFromParsecParser parseHashScriptData)
    (  Opt.long "tx-out-datum-hash"
    <> Opt.metavar "HASH"
    <> Opt.help "Required datum hash for tx inputs intended \
               \to be utilizied by a Plutus script."
    )
  where
    parseHashScriptData :: Parsec.Parser (Hash ScriptData)
    parseHashScriptData = do
      str <- Parsec.many1 Parsec.hexDigit Parsec.<?> "script data hash"
      case deserialiseFromRawBytesHex (AsHash AsScriptData) (BSC.pack str) of
        Just sdh -> return sdh
        Nothing  -> fail $ "Invalid datum hash: " ++ show str


pMultiAsset :: Parser ann Value
pMultiAsset =
  Opt.option
    (readerFromParsecParser parseValue)
      (  Opt.long "multi-asset"
      <> Opt.metavar "VALUE"
      <> Opt.help "Multi-asset value(s) with the multi-asset cli syntax"
      )

pMintMultiAsset :: Parser ann (Value, [ScriptWitnessFiles WitCtxMint])
pMintMultiAsset =
  (,) <$> Opt.option
            (readerFromParsecParser parseValue)
              (  Opt.long "mint"
              <> Opt.metavar "VALUE"
              <> Opt.help helpText
              )
      <*> some (pScriptWitnessFiles
                  WitCtxMint
                  "mint" (Just "minting")
                  "the minting of assets for a particular policy Id.")

 where
   helpText = "Mint multi-asset value(s) with the multi-asset cli syntax. \
               \You must specifiy a script witness."

pInvalidBefore :: Parser ann SlotNo
pInvalidBefore =
  SlotNo <$>
    ( Opt.option Opt.auto
       (  Opt.long "invalid-before"
       <> Opt.metavar "SLOT"
       <> Opt.help "Time that transaction is valid from (in slots)."
       )
    <|>
      Opt.option Opt.auto
        (  Opt.long "lower-bound"
        <> Opt.metavar "SLOT"
        <> Opt.help "Time that transaction is valid from (in slots) \
                    \(deprecated; use --invalid-before instead)."
        <> Opt.internal
        )
    )

pInvalidHereafter :: Parser ann SlotNo
pInvalidHereafter =
  SlotNo <$>
    ( Opt.option Opt.auto
        (  Opt.long "invalid-hereafter"
        <> Opt.metavar "SLOT"
        <> Opt.help "Time that transaction is valid until (in slots)."
        )
    <|>
      Opt.option Opt.auto
        (  Opt.long "upper-bound"
        <> Opt.metavar "SLOT"
        <> Opt.help "Time that transaction is valid until (in slots) \
                    \(deprecated; use --invalid-hereafter instead)."
       <> Opt.internal
        )
    <|>
      Opt.option Opt.auto
        (  Opt.long "ttl"
        <> Opt.metavar "SLOT"
        <> Opt.help "Time to live (in slots) (deprecated; use --invalid-hereafter instead)."
        <> Opt.internal
        )
    )

pTxFee :: Parser ann Lovelace
pTxFee =
  Lovelace . (fromIntegral :: Natural -> Integer) <$>
    Opt.option Opt.auto
      (  Opt.long "fee"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The fee amount in Lovelace."
      )

pWitnessFile :: Parser ann WitnessFile
pWitnessFile =
  WitnessFile <$>
    Opt.strOption
      (  Opt.long "witness-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the witness"
      <> Opt.completer (Opt.bashCompleter "file")
      )

pTxBodyFile :: FileDirection -> Parser ann TxBodyFile
pTxBodyFile fdir =
    TxBodyFile <$>
      (  Opt.strOption
           (  Opt.long optName
           <> Opt.metavar "FILE"
           <> Opt.help (show fdir ++ " filepath of the JSON TxBody.")
           <> Opt.completer (Opt.bashCompleter "file")
           )
      <|>
         Opt.strOption
           (  Opt.long "tx-body-file"
           <> Opt.internal
           )
      )
  where
    optName =
      case fdir of
        Input -> "tx-body-file"
        Output -> "out-file"


pTxFile :: FileDirection -> Parser ann TxFile
pTxFile fdir =
    TxFile <$>
      (  Opt.strOption
           (  Opt.long optName
           <> Opt.metavar "FILE"
           <> Opt.help (show fdir ++ " filepath of the JSON Tx.")
           <> Opt.completer (Opt.bashCompleter "file")
           )
      <|>
         Opt.strOption
           (  Opt.long "tx-file"
           <> Opt.internal
           )
      )
  where
    optName =
      case fdir of
        Input -> "tx-file"
        Output -> "out-file"

pInputTxFile :: Parser ann InputTxFile
pInputTxFile =
  InputTxBodyFile <$> pTxBodyFile Input <|> InputTxFile <$> pTxFile Input

pTxInCount :: Parser ann TxInCount
pTxInCount =
  TxInCount <$>
    Opt.option Opt.auto
      (  Opt.long "tx-in-count"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of transaction inputs."
      )

pTxOutCount :: Parser ann TxOutCount
pTxOutCount =
  TxOutCount <$>
    Opt.option Opt.auto
      (  Opt.long "tx-out-count"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of transaction outputs."
      )

pTxShelleyWitnessCount :: Parser ann TxShelleyWitnessCount
pTxShelleyWitnessCount =
  TxShelleyWitnessCount <$>
    Opt.option Opt.auto
      (  Opt.long "witness-count"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of Shelley key witnesses."
      )

pTxByronWitnessCount :: Parser ann TxByronWitnessCount
pTxByronWitnessCount =
  TxByronWitnessCount <$>
    Opt.option Opt.auto
      (  Opt.long "byron-witness-count"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of Byron key witnesses (default is 0)."
      <> Opt.value 0
      )

pQueryUTxOFilter :: Parser ann QueryUTxOFilter
pQueryUTxOFilter =
      pQueryUTxOWhole
  <|> pQueryUTxOByAddress
  <|> pQueryUTxOByTxIn
  where
    pQueryUTxOWhole =
      Opt.flag' QueryUTxOWhole
        (  Opt.long "whole-utxo"
        <> Opt.help "Return the whole UTxO (only appropriate on small testnets)."
        )

    pQueryUTxOByAddress :: Parser ann QueryUTxOFilter
    pQueryUTxOByAddress = QueryUTxOByAddress . Set.fromList <$> some pByAddress

    pByAddress :: Parser ann AddressAny
    pByAddress =
        Opt.option (readerFromParsecParser parseAddressAny)
          (  Opt.long "address"
          <> Opt.metavar "ADDRESS"
          <> Opt.help "Filter by Cardano address(es) (Bech32-encoded)."
          )

    pQueryUTxOByTxIn :: Parser ann QueryUTxOFilter
    pQueryUTxOByTxIn = QueryUTxOByTxIn . Set.fromList <$> some pByTxIn

    pByTxIn :: Parser ann TxIn
    pByTxIn =
      Opt.option (readerFromParsecParser parseTxIn)
        (  Opt.long "tx-in"
        <> Opt.metavar "TX-IN"
        <> Opt.help "Filter by transaction input (TxId#TxIx)."
        )

pFilterByStakeAddress :: Parser ann StakeAddress
pFilterByStakeAddress =
    Opt.option (readerFromParsecParser parseStakeAddress)
      (  Opt.long "address"
      <> Opt.metavar "ADDRESS"
      <> Opt.help "Filter by Cardano stake address (Bech32-encoded)."
      )

pByronAddress :: Parser ann (Address ByronAddr)
pByronAddress =
    Opt.option
      (Opt.eitherReader deserialise)
        (  Opt.long "address"
        <> Opt.metavar "STRING"
        <> Opt.help "Byron address (Base58-encoded)."
        )
  where
    deserialise :: String -> Either String (Address ByronAddr)
    deserialise =
      maybe (Left "Invalid Byron address.") Right
        . deserialiseAddress AsByronAddress
        . Text.pack

pAddress :: Parser ann Text
pAddress =
  Text.pack <$>
    Opt.strOption
      (  Opt.long "address"
      <> Opt.metavar "ADDRESS"
      <> Opt.help "A Cardano address"
      )

pStakeAddress :: Parser ann StakeAddress
pStakeAddress =
    Opt.option (readerFromParsecParser parseStakeAddress)
      (  Opt.long "stake-address"
      <> Opt.metavar "ADDRESS"
      <> Opt.help "Target stake address (bech32 format)."
      )

pStakeVerificationKeyOrFile :: Parser ann (VerificationKeyOrFile StakeKey)
pStakeVerificationKeyOrFile =
  VerificationKeyValue <$> pStakeVerificationKey
    <|> VerificationKeyFilePath <$> pStakeVerificationKeyFile

pStakeVerificationKey :: Parser ann (VerificationKey StakeKey)
pStakeVerificationKey =
  Opt.option
    (readVerificationKey AsStakeKey)
      (  Opt.long "stake-verification-key"
      <> Opt.metavar "STRING"
      <> Opt.help "Stake verification key (Bech32 or hex-encoded)."
      )

pStakeVerificationKeyFile :: Parser ann VerificationKeyFile
pStakeVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "stake-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the staking verification key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
        (  Opt.long "staking-verification-key-file"
        <> Opt.internal
        )
    )


pStakePoolVerificationKeyFile :: Parser ann VerificationKeyFile
pStakePoolVerificationKeyFile =
  VerificationKeyFile <$>
    (  Opt.strOption
         (  Opt.long "cold-verification-key-file"
         <> Opt.metavar "FILE"
         <> Opt.help "Filepath of the stake pool verification key."
         <> Opt.completer (Opt.bashCompleter "file")
         )
    <|>
       Opt.strOption
         (  Opt.long "stake-pool-verification-key-file"
         <> Opt.internal
         )
    )

pStakePoolVerificationKeyHash :: Parser ann (Hash StakePoolKey)
pStakePoolVerificationKeyHash =
    Opt.option
      (Opt.maybeReader pBech32OrHexStakePoolId)
        (  Opt.long "stake-pool-id"
        <> Opt.metavar "STAKE-POOL-ID"
        <> Opt.help "Stake pool ID/verification key hash (either \
                    \Bech32-encoded or hex-encoded)."
        )
  where
    pBech32OrHexStakePoolId :: String -> Maybe (Hash StakePoolKey)
    pBech32OrHexStakePoolId str =
      pBech32StakePoolId str <|> pHexStakePoolId str

    pHexStakePoolId :: String -> Maybe (Hash StakePoolKey)
    pHexStakePoolId =
      deserialiseFromRawBytesHex (AsHash AsStakePoolKey) . BSC.pack

    pBech32StakePoolId :: String -> Maybe (Hash StakePoolKey)
    pBech32StakePoolId =
      either (const Nothing) Just
        . deserialiseFromBech32 (AsHash AsStakePoolKey)
        . Text.pack

pStakePoolVerificationKey :: Parser ann (VerificationKey StakePoolKey)
pStakePoolVerificationKey =
  Opt.option
    (readVerificationKey AsStakePoolKey)
      (  Opt.long "stake-pool-verification-key"
      <> Opt.metavar "STRING"
      <> Opt.help "Stake pool verification key (Bech32 or hex-encoded)."
      )

pStakePoolVerificationKeyOrFile
  :: Parser ann (VerificationKeyOrFile StakePoolKey)
pStakePoolVerificationKeyOrFile =
  VerificationKeyValue <$> pStakePoolVerificationKey
    <|> VerificationKeyFilePath <$> pStakePoolVerificationKeyFile

pStakePoolVerificationKeyOrHashOrFile
  :: Parser ann (VerificationKeyOrHashOrFile StakePoolKey)
pStakePoolVerificationKeyOrHashOrFile =
  VerificationKeyOrFile <$> pStakePoolVerificationKeyOrFile
    <|> VerificationKeyHash <$> pStakePoolVerificationKeyHash

pVrfVerificationKeyFile :: Parser ann VerificationKeyFile
pVrfVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "vrf-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the VRF verification key."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pVrfVerificationKeyHash :: Parser ann (Hash VrfKey)
pVrfVerificationKeyHash =
    Opt.option
      (Opt.eitherReader deserialiseFromHex)
        (  Opt.long "vrf-verification-key-hash"
        <> Opt.metavar "STRING"
        <> Opt.help "VRF verification key hash (hex-encoded)."
        )
  where
    deserialiseFromHex :: String -> Either String (Hash VrfKey)
    deserialiseFromHex =
      maybe (Left "Invalid VRF verification key hash.") Right
        . deserialiseFromRawBytesHex (AsHash AsVrfKey)
        . BSC.pack

pVrfVerificationKey :: Parser ann (VerificationKey VrfKey)
pVrfVerificationKey =
  Opt.option
    (readVerificationKey AsVrfKey)
      (  Opt.long "vrf-verification-key"
      <> Opt.metavar "STRING"
      <> Opt.help "VRF verification key (Bech32 or hex-encoded)."
      )

pVrfVerificationKeyOrFile :: Parser ann (VerificationKeyOrFile VrfKey)
pVrfVerificationKeyOrFile =
  VerificationKeyValue <$> pVrfVerificationKey
    <|> VerificationKeyFilePath <$> pVrfVerificationKeyFile

pVrfVerificationKeyOrHashOrFile :: Parser ann (VerificationKeyOrHashOrFile VrfKey)
pVrfVerificationKeyOrHashOrFile =
  VerificationKeyOrFile <$> pVrfVerificationKeyOrFile
    <|> VerificationKeyHash <$> pVrfVerificationKeyHash

pRewardAcctVerificationKeyFile :: Parser ann VerificationKeyFile
pRewardAcctVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "pool-reward-account-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the reward account stake verification key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
        (  Opt.long "reward-account-verification-key-file"
        <> Opt.internal
        )
    )

pRewardAcctVerificationKey :: Parser ann (VerificationKey StakeKey)
pRewardAcctVerificationKey =
  Opt.option
    (readVerificationKey AsStakeKey)
      (  Opt.long "pool-reward-account-verification-key"
      <> Opt.metavar "STRING"
      <> Opt.help "Reward account stake verification key (Bech32 or hex-encoded)."
      )

pRewardAcctVerificationKeyOrFile :: Parser ann (VerificationKeyOrFile StakeKey)
pRewardAcctVerificationKeyOrFile =
  VerificationKeyValue <$> pRewardAcctVerificationKey
    <|> VerificationKeyFilePath <$> pRewardAcctVerificationKeyFile

pPoolOwnerVerificationKeyFile :: Parser ann VerificationKeyFile
pPoolOwnerVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "pool-owner-stake-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the pool owner stake verification key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
          (  Opt.long "pool-owner-staking-verification-key"
          <> Opt.internal
          )
    )

pPoolOwnerVerificationKey :: Parser ann (VerificationKey StakeKey)
pPoolOwnerVerificationKey =
  Opt.option
    (readVerificationKey AsStakeKey)
      (  Opt.long "pool-owner-verification-key"
      <> Opt.metavar "STRING"
      <> Opt.help "Pool owner stake verification key (Bech32 or hex-encoded)."
      )

pPoolOwnerVerificationKeyOrFile :: Parser ann (VerificationKeyOrFile StakeKey)
pPoolOwnerVerificationKeyOrFile =
  VerificationKeyValue <$> pPoolOwnerVerificationKey
    <|> VerificationKeyFilePath <$> pPoolOwnerVerificationKeyFile

pPoolPledge :: Parser ann Lovelace
pPoolPledge =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "pool-pledge"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The stake pool's pledge."
      )


pPoolCost :: Parser ann Lovelace
pPoolCost =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "pool-cost"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The stake pool's cost."
      )

pPoolMargin :: Parser ann Rational
pPoolMargin =
    Opt.option readRationalUnitInterval
      (  Opt.long "pool-margin"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "The stake pool's margin."
      )

pPoolRelay :: Parser ann StakePoolRelay
pPoolRelay = pSingleHostAddress <|> pSingleHostName <|> pMultiHostName

pMultiHostName :: Parser ann StakePoolRelay
pMultiHostName =
  StakePoolRelayDnsSrvRecord <$> pDNSName
 where
  pDNSName :: Parser ann ByteString
  pDNSName = Opt.option (Opt.eitherReader eDNSName)
               (  Opt.long "multi-host-pool-relay"
               <> Opt.metavar "STRING"
               <> Opt.help "The stake pool relay's DNS name that corresponds to \
                            \an SRV DNS record"
               )

pSingleHostName :: Parser ann StakePoolRelay
pSingleHostName =
  StakePoolRelayDnsARecord <$> pDNSName <*> optional pPort
 where
  pDNSName :: Parser ann ByteString
  pDNSName = Opt.option (Opt.eitherReader eDNSName)
               (  Opt.long "single-host-pool-relay"
               <> Opt.metavar "STRING"
               <> Opt.help "The stake pool relay's DNS name that corresponds to an\
                            \ A or AAAA DNS record"
               )

eDNSName :: String -> Either String ByteString
eDNSName str =
  -- We're using 'Shelley.textToDns' to validate the string.
  case Shelley.textToDns (toS str) of
    Nothing -> Left "DNS name is more than 64 bytes"
    Just dnsName -> Right . Text.encodeUtf8 . Shelley.dnsToText $ dnsName

pSingleHostAddress :: Parser ann StakePoolRelay
pSingleHostAddress = singleHostAddress
  <$> optional pIpV4
  <*> optional pIpV6
  <*> pPort
 where
  singleHostAddress :: Maybe IP.IPv4 -> Maybe IP.IPv6 -> PortNumber -> StakePoolRelay
  singleHostAddress ipv4 ipv6 port =
    case (ipv4, ipv6) of
      (Nothing, Nothing) ->
        panic "Please enter either an IPv4 or IPv6 address for the pool relay"
      (Just i4, Nothing) ->
        StakePoolRelayIp (Just i4) Nothing (Just port)
      (Nothing, Just i6) ->
        StakePoolRelayIp Nothing (Just i6) (Just port)
      (Just i4, Just i6) ->
        StakePoolRelayIp (Just i4) (Just i6) (Just port)



pIpV4 :: Parser ann IP.IPv4
pIpV4 = Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM ann IP.IPv4)
          (  Opt.long "pool-relay-ipv4"
          <> Opt.metavar "STRING"
          <> Opt.help "The stake pool relay's IPv4 address"
          )

pIpV6 :: Parser ann IP.IPv6
pIpV6 = Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM ann IP.IPv6)
           (  Opt.long "pool-relay-ipv6"
           <> Opt.metavar "STRING"
           <> Opt.help "The stake pool relay's IPv6 address"
           )

pPort :: Parser ann PortNumber
pPort = Opt.option (fromInteger <$> Opt.eitherReader readEither)
           (  Opt.long "pool-relay-port"
           <> Opt.metavar "INT"
           <> Opt.help "The stake pool relay's port"
           )

pStakePoolMetadataReference :: Parser ann (Maybe StakePoolMetadataReference)
pStakePoolMetadataReference =
  optional $
    StakePoolMetadataReference
      <$> pStakePoolMetadataUrl
      <*> pStakePoolMetadataHash

pStakePoolMetadataUrl :: Parser ann Text
pStakePoolMetadataUrl =
  Opt.option (readURIOfMaxLength 64)
    (  Opt.long "metadata-url"
    <> Opt.metavar "URL"
    <> Opt.help "Pool metadata URL (maximum length of 64 characters)."
    )

pStakePoolMetadataHash :: Parser ann (Hash StakePoolMetadata)
pStakePoolMetadataHash =
    Opt.option
      (Opt.maybeReader metadataHash)
        (  Opt.long "metadata-hash"
        <> Opt.metavar "HASH"
        <> Opt.help "Pool metadata hash."
        )
  where
    metadataHash :: String -> Maybe (Hash StakePoolMetadata)
    metadataHash = deserialiseFromRawBytesHex (AsHash AsStakePoolMetadata)
                 . BSC.pack

pStakePoolRegistrationCert :: Parser ann PoolCmd
pStakePoolRegistrationCert =
  PoolRegistrationCert
    <$> pStakePoolVerificationKeyOrFile
    <*> pVrfVerificationKeyOrFile
    <*> pPoolPledge
    <*> pPoolCost
    <*> pPoolMargin
    <*> pRewardAcctVerificationKeyOrFile
    <*> some pPoolOwnerVerificationKeyOrFile
    <*> many pPoolRelay
    <*> pStakePoolMetadataReference
    <*> pNetworkId
    <*> pOutputFile

pStakePoolRetirementCert :: Parser ann PoolCmd
pStakePoolRetirementCert =
  PoolRetirementCert
    <$> pStakePoolVerificationKeyOrFile
    <*> pEpochNo
    <*> pOutputFile


pProtocolParametersUpdate :: Parser ann ProtocolParametersUpdate
pProtocolParametersUpdate =
  ProtocolParametersUpdate
    <$> optional pProtocolVersion
    <*> optional pDecentralParam
    <*> optional pExtraEntropy
    <*> optional pMaxBlockHeaderSize
    <*> optional pMaxBodySize
    <*> optional pMaxTransactionSize
    <*> optional pMinFeeConstantFactor
    <*> optional pMinFeeLinearFactor
    <*> optional pMinUTxOValue
    <*> optional pKeyRegistDeposit
    <*> optional pPoolDeposit
    <*> optional pMinPoolCost
    <*> optional pEpochBoundRetirement
    <*> optional pNumberOfPools
    <*> optional pPoolInfluence
    <*> optional pMonetaryExpansion
    <*> optional pTreasuryExpansion
    <*> optional pUTxOCostPerWord
    <*> pure mempty -- TODO alonzo: separate support for cost model files
    <*> optional pExecutionUnitPrices
    <*> optional pMaxTxExecutionUnits
    <*> optional pMaxBlockExecutionUnits
    <*> optional pMaxValueSize
    <*> optional pCollateralPercent
    <*> optional pMaxCollateralInputs

pMinFeeLinearFactor :: Parser ann Natural
pMinFeeLinearFactor =
    Opt.option Opt.auto
      (  Opt.long "min-fee-linear"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The linear factor for the minimum fee calculation."
      )

pMinFeeConstantFactor :: Parser ann Natural
pMinFeeConstantFactor =
    Opt.option Opt.auto
      (  Opt.long "min-fee-constant"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The constant factor for the minimum fee calculation."
      )

pMinUTxOValue :: Parser ann Lovelace
pMinUTxOValue =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "min-utxo-value"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The minimum allowed UTxO value (Shelley to Mary eras)."
      )

pMinPoolCost :: Parser ann Lovelace
pMinPoolCost =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "min-pool-cost"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The minimum allowed cost parameter for stake pools."
      )

pMaxBodySize :: Parser ann Natural
pMaxBodySize =
    Opt.option Opt.auto
      (  Opt.long "max-block-body-size"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Maximal block body size."
      )

pMaxTransactionSize :: Parser ann Natural
pMaxTransactionSize =
    Opt.option Opt.auto
      (  Opt.long "max-tx-size"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Maximum transaction size."
      )

pMaxBlockHeaderSize :: Parser ann Natural
pMaxBlockHeaderSize =
    Opt.option Opt.auto
      (  Opt.long "max-block-header-size"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Maximum block header size."
      )

pKeyRegistDeposit :: Parser ann Lovelace
pKeyRegistDeposit =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "key-reg-deposit-amt"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Key registration deposit amount."
      )

pPoolDeposit :: Parser ann Lovelace
pPoolDeposit =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "pool-reg-deposit"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The amount of a pool registration deposit."
      )

pEpochBoundRetirement :: Parser ann EpochNo
pEpochBoundRetirement =
    EpochNo <$>
    Opt.option Opt.auto
      (  Opt.long "pool-retirement-epoch-boundary"
      <> Opt.metavar "INT"
      <> Opt.help "Epoch bound on pool retirement."
      )

pNumberOfPools :: Parser ann Natural
pNumberOfPools =
    Opt.option Opt.auto
      (  Opt.long "number-of-pools"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Desired number of pools."
      )

pPoolInfluence :: Parser ann Rational
pPoolInfluence =
    Opt.option readRational
      (  Opt.long "pool-influence"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "Pool influence."
      )

pTreasuryExpansion :: Parser ann Rational
pTreasuryExpansion =
    Opt.option readRationalUnitInterval
      (  Opt.long "treasury-expansion"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "Treasury expansion."
      )

pMonetaryExpansion :: Parser ann Rational
pMonetaryExpansion =
    Opt.option readRationalUnitInterval
      (  Opt.long "monetary-expansion"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "Monetary expansion."
      )

pDecentralParam :: Parser ann Rational
pDecentralParam =
    Opt.option readRationalUnitInterval
      (  Opt.long "decentralization-parameter"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "Decentralization parameter."
      )

pExtraEntropy :: Parser ann (Maybe PraosNonce)
pExtraEntropy =
      Opt.option (Just <$> readerFromParsecParser parsePraosNonce)
        (  Opt.long "extra-entropy"
        <> Opt.metavar "HEX"
        <> Opt.help "Praos extra entropy, as a hex byte string."
        )
  <|> Opt.flag' Nothing
        (  Opt.long "reset-extra-entropy"
        <> Opt.help "Reset the Praos extra entropy to none."
        )
  where
    parsePraosNonce :: Parsec.Parser PraosNonce
    parsePraosNonce = makePraosNonce <$> parseEntropyBytes

    parseEntropyBytes :: Parsec.Parser ByteString
    parseEntropyBytes = either fail return
                      . B16.decode . BSC.pack
                    =<< Parsec.many1 Parsec.hexDigit

pUTxOCostPerWord :: Parser ann Lovelace
pUTxOCostPerWord =
    Opt.option (readerFromParsecParser parseLovelace)
      (  Opt.long "utxo-cost-per-word"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "Cost in lovelace per unit of UTxO storage (from Alonzo era)."
      )

pExecutionUnitPrices :: Parser ann ExecutionUnitPrices
pExecutionUnitPrices = ExecutionUnitPrices
  <$> Opt.option readRational
      (  Opt.long "price-execution-steps"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "Step price of execution units for script languages that use \
                  \them (from Alonzo era)."
      )
  <*> Opt.option readRational
      (  Opt.long "price-execution-memory"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "Memory price of execution units for script languages that \
                  \use them (from Alonzo era)."
      )

pMaxTxExecutionUnits :: Parser ann ExecutionUnits
pMaxTxExecutionUnits =
  uncurry ExecutionUnits <$>
  Opt.option Opt.auto
    (  Opt.long "max-tx-execution-units"
    <> Opt.metavar "(INT, INT)"
    <> Opt.help "Max total script execution resources units allowed per tx \
                \(from Alonzo era)."
    )

pMaxBlockExecutionUnits :: Parser ann ExecutionUnits
pMaxBlockExecutionUnits =
  uncurry ExecutionUnits <$>
  Opt.option Opt.auto
    (  Opt.long "max-block-execution-units"
    <> Opt.metavar "(INT, INT)"
    <> Opt.help "Max total script execution resources units allowed per block \
                \(from Alonzo era)."
    )

pMaxValueSize :: Parser ann Natural
pMaxValueSize =
  Opt.option Opt.auto
    (  Opt.long "max-value-size"
    <> Opt.metavar "INT"
    <> Opt.help "Max size of a multi-asset value in a tx output (from Alonzo \
                \era)."
    )

pCollateralPercent :: Parser ann Natural
pCollateralPercent =
  Opt.option Opt.auto
    (  Opt.long "collateral-percent"
    <> Opt.metavar "INT"
    <> Opt.help "The percentage of the script contribution to the txfee that \
                \must be provided as collateral inputs when including Plutus \
                \scripts (from Alonzo era)."
    )

pMaxCollateralInputs :: Parser ann Natural
pMaxCollateralInputs =
  Opt.option Opt.auto
    (  Opt.long "max-collateral-inputs"
    <> Opt.metavar "INT"
    <> Opt.help "The maximum number of collateral inputs allowed in a \
                \transaction (from Alonzo era)."
    )

pConsensusModeParams :: Parser ann AnyConsensusModeParams
pConsensusModeParams = asum
  [ Opt.flag' (AnyConsensusModeParams ShelleyModeParams)
      (  Opt.long "shelley-mode"
      <> Opt.help "For talking to a node running in Shelley-only mode."
      )
  , Opt.flag' ()
      (  Opt.long "byron-mode"
      <> Opt.help "For talking to a node running in Byron-only mode."
      )
       *> pByronConsensusMode
  , Opt.flag' ()
      (  Opt.long "cardano-mode"
      <> Opt.help "For talking to a node running in full Cardano mode (default)."
      )
       *> pCardanoConsensusMode
  , -- Default to the Cardano consensus mode.
    pure . AnyConsensusModeParams . CardanoModeParams $ EpochSlots defaultByronEpochSlots
  ]
 where
   pCardanoConsensusMode :: Parser ann AnyConsensusModeParams
   pCardanoConsensusMode = AnyConsensusModeParams . CardanoModeParams <$> pEpochSlots
   pByronConsensusMode :: Parser ann AnyConsensusModeParams
   pByronConsensusMode = AnyConsensusModeParams . ByronModeParams <$> pEpochSlots

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

pEpochSlots :: Parser ann EpochSlots
pEpochSlots =
  EpochSlots <$>
    Opt.option Opt.auto
      (  Opt.long "epoch-slots"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of slots per epoch for the Byron era."
      <> Opt.value defaultByronEpochSlots -- Default to the mainnet value.
      <> Opt.showDefault
      )

pProtocolVersion :: Parser ann (Natural, Natural)
pProtocolVersion =
    (,) <$> pProtocolMajorVersion <*> pProtocolMinorVersion
  where
    pProtocolMajorVersion =
      Opt.option Opt.auto
        (  Opt.long "protocol-major-version"
        <> Opt.metavar "NATURAL"
        <> Opt.help "Major protocol version. An increase indicates a hard fork."
        )
    pProtocolMinorVersion =
      Opt.option Opt.auto
        (  Opt.long "protocol-minor-version"
        <> Opt.metavar "NATURAL"
        <> Opt.help "Minor protocol version. An increase indicates a soft fork\
                    \ (old software canvalidate but not produce new blocks)."
        )

--
-- Shelley CLI flag field parsers
--

parseLovelace :: Parsec.Parser Lovelace
parseLovelace = do
  i <- decimal
  if i > toInteger (maxBound :: Word64)
  then fail $ show i <> " lovelace exceeds the Word64 upper bound"
  else return $ Lovelace i

parseAddressAny :: Parsec.Parser AddressAny
parseAddressAny = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsAddressAny str of
      Nothing   -> fail "invalid address"
      Just addr -> pure addr

parseStakeAddress :: Parsec.Parser StakeAddress
parseStakeAddress = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsStakeAddress str of
      Nothing   -> fail $ "invalid address: " <> Text.unpack str
      Just addr -> pure addr

parseTxOutAnyEra :: Parsec.Parser (Maybe (Hash ScriptData) -> TxOutAnyEra)
parseTxOutAnyEra = do
    addr <- parseAddressAny
    Parsec.spaces
    -- Accept the old style of separating the address and value in a
    -- transaction output:
    Parsec.option () (Parsec.char '+' >> Parsec.spaces)
    val <- parseValue
    return (TxOutAnyEra addr val)

lexPlausibleAddressString :: Parsec.Parser Text
lexPlausibleAddressString =
    Text.pack <$> Parsec.many1 (Parsec.satisfy isPlausibleAddressChar)
  where
    -- Covers both base58 and bech32 (with constrained prefixes)
    isPlausibleAddressChar c =
         (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c == '_'

decimal :: Parsec.Parser Integer
Parsec.TokenParser { Parsec.decimal = decimal } = Parsec.haskell

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Read a Bech32 or hex-encoded verification key.
readVerificationKey
  :: forall keyrole ann.
     SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> Opt.ReadM ann (VerificationKey keyrole)
readVerificationKey asType =
    Opt.eitherReader deserialiseFromBech32OrHex
  where
    keyFormats :: NonEmpty (InputFormat (VerificationKey keyrole))
    keyFormats = NE.fromList [InputFormatBech32, InputFormatHex]

    deserialiseFromBech32OrHex
      :: String
      -> Either String (VerificationKey keyrole)
    deserialiseFromBech32OrHex str =
      first (Text.unpack . renderInputDecodeError) $
        deserialiseInput (AsVerificationKey asType) keyFormats (BSC.pack str)

readOutputFormat :: Opt.ReadM ann OutputFormat
readOutputFormat = do
  s <- Opt.str
  case s of
    "hex" -> pure OutputFormatHex
    "bech32" -> pure OutputFormatBech32
    _ ->
      fail $ "Invalid output format: \""
        <> s
        <> "\". Accepted output formats are \"hex\" and \"bech32\"."

readURIOfMaxLength :: Int -> Opt.ReadM ann Text
readURIOfMaxLength maxLen =
  Text.pack <$> readStringOfMaxLength maxLen

readStringOfMaxLength :: Int -> Opt.ReadM ann String
readStringOfMaxLength maxLen = do
  s <- Opt.str
  let strLen = length s
  if strLen <= maxLen
    then pure s
    else fail $
      "The provided string must have at most 64 characters, but it has "
        <> show strLen
        <> " characters."

readRationalUnitInterval :: Opt.ReadM ann Rational
readRationalUnitInterval = readRational >>= checkUnitInterval
  where
   checkUnitInterval :: Rational -> Opt.ReadM ann Rational
   checkUnitInterval q
     | q >= 0 && q <= 1 = return q
     | otherwise        = fail "Please enter a value in the range [0,1]"

readRational :: Opt.ReadM ann Rational
readRational = toRational <$> readerFromAttoParser Atto.scientific

readerJSON :: Opt.ReadM ann Aeson.Value
readerJSON = readerFromAttoParser Aeson.Parser.json

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM ann a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

readerFromParsecParser :: Parsec.Parser a -> Opt.ReadM ann a
readerFromParsecParser p =
    Opt.eitherReader (first formatError . Parsec.parse (p <* Parsec.eof) "")
  where
    --TODO: the default parsec error formatting is quite good, but we could
    -- customise it somewhat:
    formatError err =
      Parsec.showErrorMessages "or" "unknown parse error"
                               "expecting" "unexpected" "end of input"
                               (Parsec.errorMessages err)

subParser :: String -> ParserInfo ann a -> Parser ann a
subParser availableCommand pInfo =
  Opt.hsubparser $ Opt.command availableCommand pInfo <> Opt.metavar availableCommand
