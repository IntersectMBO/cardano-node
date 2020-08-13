
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

import           Cardano.Api.MetaData
import           Cardano.Api.Protocol (Protocol (..))
import           Cardano.Api.Typed hiding (PoolId)
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (InputFormat (..), VerificationKeyOrFile (..),
                     VerificationKeyOrHashOrFile (..), deserialiseInput, renderInputDecodeError)
import           Cardano.CLI.Types
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import           Control.Monad.Fail (fail)
import           Data.Attoparsec.Combinator ((<?>))
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeOrError)
import           Network.Socket (PortNumber)
import           Network.URI (URI, parseURI)
import           Options.Applicative hiding (str)
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import qualified Cardano.Crypto.Hash as Crypto (Blake2b_256, Hash (..), hashFromBytesAsHex)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as Char
import qualified Data.IP as IP
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Options.Applicative as Opt
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

--
-- Shelley CLI command parsers
--

parseShelleyCommands :: Parser ShelleyCommand
parseShelleyCommands =
  Opt.subparser $
    mconcat
      [ Opt.command "address"
          (Opt.info (AddressCmd <$> pAddressCmd) $ Opt.progDesc "Shelley payment address commands")
      , Opt.command "stake-address"
          (Opt.info (StakeAddressCmd <$> pStakeAddress) $ Opt.progDesc "Shelley stake address commands")
      , Opt.command "key"
          (Opt.info (KeyCmd <$> pKeyCmd) $ Opt.progDesc "Shelley key utility commands")
      , Opt.command "transaction"
          (Opt.info (TransactionCmd <$> pTransaction) $ Opt.progDesc "Shelley transaction commands")
      , Opt.command "node"
          (Opt.info (NodeCmd <$> pNodeCmd) $ Opt.progDesc "Shelley node operaton commands")
      , Opt.command "stake-pool"
          (Opt.info (PoolCmd <$> pPoolCmd) $ Opt.progDesc "Shelley stake pool commands")
      , Opt.command "query"
          (Opt.info (QueryCmd <$> pQueryCmd) . Opt.progDesc $
             mconcat
               [ "Shelley node query commands. Will query the local node whose Unix domain socket "
               , "is obtained from the CARDANO_NODE_SOCKET_PATH enviromnent variable."
               ]
            )
      , Opt.command "genesis"
          (Opt.info (GenesisCmd <$> pGenesisCmd) $ Opt.progDesc "Shelley genesis block commands")
      , Opt.command "governance"
          (Opt.info (GovernanceCmd <$> pGovernanceCmd) $ Opt.progDesc "Shelley governance commands")
      , Opt.command "text-view"
          (Opt.info (TextViewCmd <$> pTextViewCmd) . Opt.progDesc $
             mconcat
               [ "Commands for dealing with Shelley TextView files. "
               , "Transactions, addresses etc are stored on disk as TextView files."
               ]
            )

      ]

pTextViewCmd :: Parser TextViewCmd
pTextViewCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "decode-cbor"
          (Opt.info (TextViewInfo <$> pCBORInFile <*> pMaybeOutputFile)
            $ Opt.progDesc "Print a TextView file as decoded CBOR."
            )
      ]

pCBORInFile :: Parser FilePath
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

pAddressCmd :: Parser AddressCmd
pAddressCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "key-gen"
          (Opt.info pAddressKeyGen $ Opt.progDesc "Create an address key pair.")
      , Opt.command "key-hash"
          (Opt.info pAddressKeyHash $ Opt.progDesc "Print the hash of an address key.")
      , Opt.command "build"
          (Opt.info pAddressBuild $ Opt.progDesc "Build a Shelley payment address, with optional delegation to a stake address.")
      , Opt.command "build-script"
          (Opt.info pAddressBuildScript $ Opt.progDesc "Build a Shelley script address.")
      , Opt.command "info"
          (Opt.info pAddressInfo $ Opt.progDesc "Print information about an address.")
      ]
  where
    pAddressKeyGen :: Parser AddressCmd
    pAddressKeyGen = AddressKeyGen <$> pAddressKeyType
                                   <*> pVerificationKeyFile Output
                                   <*> pSigningKeyFile Output

    pAddressKeyHash :: Parser AddressCmd
    pAddressKeyHash = AddressKeyHash <$> pPaymentVerificationKeyFile <*> pMaybeOutputFile

    pAddressBuild :: Parser AddressCmd
    pAddressBuild =
      AddressBuild
        <$> pPaymentVerificationKeyFile
        <*> Opt.optional pStakeVerificationKeyFile
        <*> pNetworkId
        <*> pMaybeOutputFile

    pAddressBuildScript :: Parser AddressCmd
    pAddressBuildScript = AddressBuildMultiSig
                            <$> pScript
                            <*> pNetworkId
                            <*> pMaybeOutputFile

    pAddressInfo :: Parser AddressCmd
    pAddressInfo = AddressInfo <$> pAddress <*> pMaybeOutputFile

pPaymentVerificationKeyFile :: Parser VerificationKeyFile
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

pScript :: Parser ScriptFile
pScript = ScriptFile <$> Opt.strOption
  (  Opt.long "script-file"
  <> Opt.metavar "FILE"
  <> Opt.help "Filepath of the script."
  <> Opt.completer (Opt.bashCompleter "file")
  )

pStakeAddress :: Parser StakeAddressCmd
pStakeAddress =
  Opt.subparser $
    mconcat
      [ Opt.command "key-gen"
          (Opt.info pStakeAddressKeyGen $ Opt.progDesc "Create a stake address key pair")
      , Opt.command "build"
          (Opt.info pStakeAddressBuild $ Opt.progDesc "Build a stake address")
      , Opt.command "key-hash"
          (Opt.info pStakeAddressKeyHash $ Opt.progDesc "Print the hash of a stake address key.")
      , Opt.command "registration-certificate"
          (Opt.info pStakeAddressRegistrationCert $ Opt.progDesc "Create a stake address registration certificate")
      , Opt.command "deregistration-certificate"
          (Opt.info pStakeAddressDeregistrationCert $ Opt.progDesc "Create a stake address deregistration certificate")
      , Opt.command "delegation-certificate"
          (Opt.info pStakeAddressDelegationCert $ Opt.progDesc "Create a stake address delegation certificate")
      ]
  where
    pStakeAddressKeyGen :: Parser StakeAddressCmd
    pStakeAddressKeyGen = StakeAddressKeyGen
                            <$> pVerificationKeyFile Output
                            <*> pSigningKeyFile Output

    pStakeAddressKeyHash :: Parser StakeAddressCmd
    pStakeAddressKeyHash = StakeAddressKeyHash <$> pStakeVerificationKeyOrFile <*> pMaybeOutputFile

    pStakeAddressBuild :: Parser StakeAddressCmd
    pStakeAddressBuild = StakeAddressBuild <$> pStakeVerificationKeyOrFile
                                           <*> pNetworkId
                                           <*> pMaybeOutputFile

    pStakeAddressRegistrationCert :: Parser StakeAddressCmd
    pStakeAddressRegistrationCert = StakeKeyRegistrationCert
                                      <$> pStakeVerificationKeyOrFile
                                      <*> pOutputFile

    pStakeAddressDeregistrationCert :: Parser StakeAddressCmd
    pStakeAddressDeregistrationCert = StakeKeyDeRegistrationCert
                                        <$> pStakeVerificationKeyOrFile
                                        <*> pOutputFile

    pStakeAddressDelegationCert :: Parser StakeAddressCmd
    pStakeAddressDelegationCert = StakeKeyDelegationCert
                                    <$> pStakeVerificationKeyOrFile
                                    <*> pStakePoolVerificationKeyOrHashOrFile
                                    <*> pOutputFile

pKeyCmd :: Parser KeyCmd
pKeyCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "verification-key" $
          Opt.info pKeyGetVerificationKey $
            Opt.progDesc $ "Get a verification key from a signing key. This "
                        ++ " supports all key types."

      , Opt.command "non-extended-key" $
          Opt.info pKeyNonExtendedKey $
            Opt.progDesc $ "Get a non-extended verification key from an "
                        ++ "extended verification key. This supports all "
                        ++ "extended key types."

      , Opt.command "convert-byron-key" $
          Opt.info pKeyConvertByronKey $
            Opt.progDesc $ "Convert a Byron payment, genesis or genesis "
                        ++ "delegate key (signing or verification) to a "
                        ++ "corresponding Shelley-format key."

      , Opt.command "convert-byron-genesis-vkey" $
          Opt.info pKeyConvertByronGenesisVKey $
            Opt.progDesc $ "Convert a Base64-encoded Byron genesis "
                        ++ "verification key to a Shelley genesis "
                        ++ "verification key"

      , Opt.command "convert-itn-key" $
          Opt.info pKeyConvertITNKey $
            Opt.progDesc $ "Convert an Incentivized Testnet (ITN) non-extended "
                        ++ "(Ed25519) signing or verification key to a "
                        ++ "corresponding Shelley stake key"
      , Opt.command "convert-itn-extended-key" $
          Opt.info pKeyConvertITNExtendedKey $
            Opt.progDesc $ "Convert an Incentivized Testnet (ITN) extended "
                        ++ "(Ed25519Extended) signing key to a corresponding "
                        ++ "Shelley stake signing key"
      , Opt.command "convert-itn-bip32-key" $
          Opt.info pKeyConvertITNBip32Key $
            Opt.progDesc $ "Convert an Incentivized Testnet (ITN) BIP32 "
                        ++ "(Ed25519Bip32) signing key to a corresponding "
                        ++ "Shelley stake signing key"

      , Opt.command "convert-cardano-address-key" $
          Opt.info pKeyConvertCardanoAddressSigningKey $
            Opt.progDesc $ "Convert a cardano-address extended signing key "
                        ++ "to a corresponding Shelley-format key."
      ]
  where
    pKeyGetVerificationKey :: Parser KeyCmd
    pKeyGetVerificationKey =
      KeyGetVerificationKey
        <$> pSigningKeyFile      Input
        <*> pVerificationKeyFile Output

    pKeyNonExtendedKey :: Parser KeyCmd
    pKeyNonExtendedKey =
      KeyNonExtendedKey
        <$> pExtendedVerificationKeyFile Input
        <*> pVerificationKeyFile Output

    pKeyConvertByronKey :: Parser KeyCmd
    pKeyConvertByronKey =
      KeyConvertByronKey
        <$> optional pPassword
        <*> pByronKeyType
        <*> pByronKeyFile
        <*> pOutputFile

    pPassword :: Parser Text
    pPassword = Opt.strOption
                  (  Opt.long "password"
                  <> Opt.metavar "TEXT"
                  <> Opt.help "Password for signing key (if applicable)."
                  )

    pByronKeyType :: Parser ByronKeyType
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

    pByronKeyFile :: Parser SomeKeyFile
    pByronKeyFile =
          (ASigningKeyFile      <$> pByronSigningKeyFile)
      <|> (AVerificationKeyFile <$> pByronVerificationKeyFile)

    pByronSigningKeyFile :: Parser SigningKeyFile
    pByronSigningKeyFile =
      SigningKeyFile <$>
        Opt.strOption
          (  Opt.long "byron-signing-key-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input filepath of the Byron-format signing key."
          <> Opt.completer (Opt.bashCompleter "file")
          )

    pByronVerificationKeyFile :: Parser VerificationKeyFile
    pByronVerificationKeyFile =
      VerificationKeyFile <$>
        Opt.strOption
          (  Opt.long "byron-verification-key-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Input filepath of the Byron-format verification key."
          <> Opt.completer (Opt.bashCompleter "file")
          )

    pKeyConvertByronGenesisVKey :: Parser KeyCmd
    pKeyConvertByronGenesisVKey =
      KeyConvertByronGenesisVKey
        <$> pByronGenesisVKeyBase64
        <*> pOutputFile

    pByronGenesisVKeyBase64 :: Parser VerificationKeyBase64
    pByronGenesisVKeyBase64 =
      VerificationKeyBase64 <$>
        Opt.strOption
          (  Opt.long "byron-genesis-verification-key"
          <> Opt.metavar "BASE64"
          <> Opt.help "Base64 string for the Byron genesis verification key."
          )

    pKeyConvertITNKey :: Parser KeyCmd
    pKeyConvertITNKey =
      KeyConvertITNStakeKey
        <$> pITNKeyFIle
        <*> pOutputFile

    pKeyConvertITNExtendedKey :: Parser KeyCmd
    pKeyConvertITNExtendedKey =
      KeyConvertITNExtendedToStakeKey
        <$> pITNSigningKeyFile
        <*> pOutputFile

    pKeyConvertITNBip32Key :: Parser KeyCmd
    pKeyConvertITNBip32Key =
      KeyConvertITNBip32ToStakeKey
        <$> pITNSigningKeyFile
        <*> pOutputFile

    pITNKeyFIle :: Parser SomeKeyFile
    pITNKeyFIle = pITNSigningKeyFile
              <|> pITNVerificationKeyFile

    pITNSigningKeyFile :: Parser SomeKeyFile
    pITNSigningKeyFile =
      ASigningKeyFile . SigningKeyFile <$>
        Opt.strOption
          (  Opt.long "itn-signing-key-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Filepath of the ITN signing key."
          <> Opt.completer (Opt.bashCompleter "file")
          )

    pITNVerificationKeyFile :: Parser SomeKeyFile
    pITNVerificationKeyFile =
      AVerificationKeyFile . VerificationKeyFile <$>
        Opt.strOption
          (  Opt.long "itn-verification-key-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Filepath of the ITN verification key."
          <> Opt.completer (Opt.bashCompleter "file")
          )

    pKeyConvertCardanoAddressSigningKey :: Parser KeyCmd
    pKeyConvertCardanoAddressSigningKey =
      KeyConvertCardanoAddressSigningKey
        <$> pCardanoAddressKeyType
        <*> pSigningKeyFile Input
        <*> pOutputFile

    pCardanoAddressKeyType :: Parser CardanoAddressKeyType
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

pTransaction :: Parser TransactionCmd
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
    , subParser "calculate-min-fee"
        (Opt.info pTransactionCalculateMinFee $ Opt.progDesc "Calculate the minimum fee for a transaction")
    , subParser "txid"
        (Opt.info pTransactionId $ Opt.progDesc "Print a transaction identifier")
    ]
 where
  subParser :: String -> ParserInfo TransactionCmd -> Parser TransactionCmd
  subParser name pInfo = Opt.subparser $ Opt.command name pInfo

  assembleInfo :: ParserInfo TransactionCmd
  assembleInfo =
    Opt.info pTransactionAssembleTxBodyWit
      $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

  pSignWitnessBackwardCompatible :: Parser TransactionCmd
  pSignWitnessBackwardCompatible =
    Opt.subparser
      $ Opt.command "sign-witness" assembleInfo <> Opt.internal

  pTransactionBuild :: Parser TransactionCmd
  pTransactionBuild = TxBuildRaw <$> some pTxIn
                                 <*> some pTxOut
                                 <*> pTxTTL
                                 <*> pTxFee
                                 <*> many pCertificateFile
                                 <*> many pWithdrawal
                                 <*> pTxMetadataJsonSchema
                                 <*> many pMetaDataFile
                                 <*> optional pUpdateProposalFile
                                 <*> pTxBodyFile Output

  pTransactionSign  :: Parser TransactionCmd
  pTransactionSign = TxSign <$> pTxBodyFile Input
                            <*> pSomeWitnessSigningData
                            <*> optional pNetworkId
                            <*> pTxFile Output

  pTransactionCreateWitness :: Parser TransactionCmd
  pTransactionCreateWitness = TxCreateWitness
                                <$> pTxBodyFile Input
                                <*> pWitnessSigningData
                                <*> optional pNetworkId
                                <*> pOutputFile

  pTransactionAssembleTxBodyWit :: Parser TransactionCmd
  pTransactionAssembleTxBodyWit = TxAssembleTxBodyWitness
                                    <$> pTxBodyFile Input
                                    <*> some pWitnessFile
                                    <*> pOutputFile

  pTransactionSubmit  :: Parser TransactionCmd
  pTransactionSubmit = TxSubmit <$> pProtocol
                                <*> pNetworkId
                                <*> pTxSubmitFile

  pTransactionCalculateMinFee :: Parser TransactionCmd
  pTransactionCalculateMinFee =
    TxCalculateMinFee
      <$> pTxBodyFile Input
      <*> optional pNetworkId
      <*> pProtocolParamsFile
      <*> pTxInCount
      <*> pTxOutCount
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount

  pTransactionId  :: Parser TransactionCmd
  pTransactionId = TxGetTxId <$> pTxBodyFile Input


pNodeCmd :: Parser NodeCmd
pNodeCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "key-gen"
          (Opt.info pKeyGenOperator $
             Opt.progDesc "Create a key pair for a node operator's offline \
                         \ key and a new certificate issue counter")
      , Opt.command "key-gen-KES"
          (Opt.info pKeyGenKES $
             Opt.progDesc "Create a key pair for a node KES operational key")
      , Opt.command "key-gen-VRF"
          (Opt.info pKeyGenVRF $
             Opt.progDesc "Create a key pair for a node VRF operational key")
      , Opt.command "key-hash-VRF"
          (Opt.info pKeyHashVRF $
             Opt.progDesc "Print hash of a node's operational VRF key.")
      , Opt.command "new-counter"
          (Opt.info pNewCounter $
             Opt.progDesc "Create a new certificate issue counter")
      , Opt.command "issue-op-cert"
          (Opt.info pIssueOpCert $
             Opt.progDesc "Issue a node operational certificate")
      ]
  where
    pKeyGenOperator :: Parser NodeCmd
    pKeyGenOperator =
      NodeKeyGenCold <$> pColdVerificationKeyFile
                     <*> pColdSigningKeyFile
                     <*> pOperatorCertIssueCounterFile

    pKeyGenKES :: Parser NodeCmd
    pKeyGenKES =
      NodeKeyGenKES <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pKeyGenVRF :: Parser NodeCmd
    pKeyGenVRF =
      NodeKeyGenVRF <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pKeyHashVRF :: Parser NodeCmd
    pKeyHashVRF =
      NodeKeyHashVRF <$> pVerificationKeyFile Input <*> pMaybeOutputFile

    pNewCounter :: Parser NodeCmd
    pNewCounter =
      NodeNewCounter <$> pColdVerificationKeyFile
                     <*> pCounterValue
                     <*> pOperatorCertIssueCounterFile

    pCounterValue :: Parser Word
    pCounterValue =
        Opt.option Opt.auto
          (  Opt.long "counter-value"
          <> Opt.metavar "INT"
          <> Opt.help "The next certificate issue counter value to use."
          )

    pIssueOpCert :: Parser NodeCmd
    pIssueOpCert =
      NodeIssueOpCert <$> pKESVerificationKeyFile
                      <*> pColdSigningKeyFile
                      <*> pOperatorCertIssueCounterFile
                      <*> pKesPeriod
                      <*> pOutputFile


pPoolCmd :: Parser PoolCmd
pPoolCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "registration-certificate"
          (Opt.info pStakePoolRegistrationCert $ Opt.progDesc "Create a stake pool registration certificate")
      , Opt.command "deregistration-certificate"
          (Opt.info pStakePoolRetirementCert $ Opt.progDesc "Create a stake pool deregistration certificate")
      , Opt.command "id"
          (Opt.info pId $
             Opt.progDesc "Build pool id from the offline key")
      , Opt.command "metadata-hash"
          (Opt.info pPoolMetaDataHashSubCmd $ Opt.progDesc "Print the hash of pool metadata.")
      ]
  where
    pId :: Parser PoolCmd
    pId = PoolGetId <$> pVerificationKeyFile Output <*> pOutputFormat

    pPoolMetaDataHashSubCmd :: Parser PoolCmd
    pPoolMetaDataHashSubCmd = PoolMetaDataHash <$> pPoolMetaDataFile <*> pMaybeOutputFile


pQueryCmd :: Parser QueryCmd
pQueryCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "protocol-parameters"
          (Opt.info pQueryProtocolParameters $ Opt.progDesc "Get the node's current protocol parameters")
      , Opt.command "tip"
          (Opt.info pQueryTip $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)")
      , Opt.command "stake-distribution"
          (Opt.info pQueryStakeDistribution $ Opt.progDesc "Get the node's current aggregated stake distribution")
      , Opt.command "stake-address-info"
          (Opt.info pQueryStakeAddressInfo $ Opt.progDesc "Get the current delegations and \
                                                          \reward accounts filtered by stake \
                                                          \address.")
      , Opt.command "utxo"
          (Opt.info pQueryUTxO $ Opt.progDesc "Get the node's current UTxO with the option of \
                                              \filtering by address(es)")
      , Opt.command "ledger-state"
          (Opt.info pQueryLedgerState $ Opt.progDesc "Dump the current state of the node")
      ]
  where
    pQueryProtocolParameters :: Parser QueryCmd
    pQueryProtocolParameters =
      QueryProtocolParameters
        <$> pProtocol
        <*> pNetworkId
        <*> pMaybeOutputFile

    pQueryTip :: Parser QueryCmd
    pQueryTip = QueryTip <$> pProtocol <*> pNetworkId <*> pMaybeOutputFile

    pQueryUTxO :: Parser QueryCmd
    pQueryUTxO =
      QueryUTxO
        <$> pProtocol
        <*> pQueryFilter
        <*> pNetworkId
        <*> pMaybeOutputFile

    pQueryStakeDistribution :: Parser QueryCmd
    pQueryStakeDistribution =
      QueryStakeDistribution
        <$> pProtocol
        <*> pNetworkId
        <*> pMaybeOutputFile

    pQueryStakeAddressInfo :: Parser QueryCmd
    pQueryStakeAddressInfo =
      QueryStakeAddressInfo
        <$> pProtocol
        <*> pFilterByStakeAddress
        <*> pNetworkId
        <*> pMaybeOutputFile

    pQueryLedgerState :: Parser QueryCmd
    pQueryLedgerState = QueryLedgerState <$> pProtocol <*> pNetworkId <*> pMaybeOutputFile

pGovernanceCmd :: Parser GovernanceCmd
pGovernanceCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "create-mir-certificate"
          (Opt.info pMIRCertificate $
            Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate")
      , Opt.command "create-genesis-key-delegation-certificate"
          (Opt.info pGovernanceGenesisKeyDelegationCertificate $
            Opt.progDesc "Create a genesis key delegation certificate")
      , Opt.command "create-update-proposal"
          (Opt.info pUpdateProposal $
            Opt.progDesc "Create an update proposal")
      ]
  where
    pMIRCertificate :: Parser GovernanceCmd
    pMIRCertificate = GovernanceMIRCertificate
                        <$> pMIRPot
                        <*> some pStakeVerificationKeyFile
                        <*> some pRewardAmt
                        <*> pOutputFile

    pGovernanceGenesisKeyDelegationCertificate :: Parser GovernanceCmd
    pGovernanceGenesisKeyDelegationCertificate =
      GovernanceGenesisKeyDelegationCertificate
        <$> pGenesisVerificationKeyOrHashOrFile
        <*> pGenesisDelegateVerificationKeyOrHashOrFile
        <*> pVrfVerificationKeyOrHashOrFile
        <*> pOutputFile

    pMIRPot :: Parser Shelley.MIRPot
    pMIRPot =
          Opt.flag' Shelley.ReservesMIR
            (  Opt.long "reserves"
            <> Opt.help "Use the reserves pot."
            )
      <|> Opt.flag' Shelley.TreasuryMIR
            (  Opt.long "treasury"
            <> Opt.help "Use the treasury pot."
            )

    pUpdateProposal :: Parser GovernanceCmd
    pUpdateProposal = GovernanceUpdateProposal
                        <$> pOutputFile
                        <*> pEpochNoUpdateProp
                        <*> some pGenesisVerificationKeyFile
                        <*> pShelleyProtocolParametersUpdate

pRewardAmt :: Parser Lovelace
pRewardAmt =
    Opt.option (readerFromAttoParser parseLovelace)
      (  Opt.long "reward"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The reward for the relevant reward account."
      )

pGenesisCmd :: Parser GenesisCmd
pGenesisCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "key-gen-genesis"
          (Opt.info pGenesisKeyGen $
             Opt.progDesc "Create a Shelley genesis key pair")
      , Opt.command "key-gen-delegate"
          (Opt.info pGenesisDelegateKeyGen $
             Opt.progDesc "Create a Shelley genesis delegate key pair")
      , Opt.command "key-gen-utxo"
          (Opt.info pGenesisUTxOKeyGen $
             Opt.progDesc "Create a Shelley genesis UTxO key pair")
      , Opt.command "key-hash"
          (Opt.info pGenesisKeyHash $
             Opt.progDesc "Print the identifier (hash) of a public key")
      , Opt.command "get-ver-key"
          (Opt.info pGenesisVerKey $
             Opt.progDesc "Derive the verification key from a signing key")
      , Opt.command "initial-addr"
          (Opt.info pGenesisAddr $
             Opt.progDesc "Get the address for an initial UTxO based on the verification key")
      , Opt.command "initial-txin"
          (Opt.info pGenesisTxIn $
             Opt.progDesc "Get the TxIn for an initial UTxO based on the verification key")
      , Opt.command "create"
          (Opt.info pGenesisCreate $
             Opt.progDesc ("Create a Shelley genesis file from a genesis "
                        ++ "template and genesis/delegation/spending keys."))

      , Opt.command "hash"
          (Opt.info pGenesisHash $
             Opt.progDesc "Compute the hash of a genesis file")
      ]
  where
    pGenesisKeyGen :: Parser GenesisCmd
    pGenesisKeyGen =
      GenesisKeyGenGenesis <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pGenesisDelegateKeyGen :: Parser GenesisCmd
    pGenesisDelegateKeyGen =
      GenesisKeyGenDelegate <$> pVerificationKeyFile Output
                            <*> pSigningKeyFile Output
                            <*> pOperatorCertIssueCounterFile

    pGenesisUTxOKeyGen :: Parser GenesisCmd
    pGenesisUTxOKeyGen =
      GenesisKeyGenUTxO <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pGenesisKeyHash :: Parser GenesisCmd
    pGenesisKeyHash =
      GenesisCmdKeyHash <$> pVerificationKeyFile Input

    pGenesisVerKey :: Parser GenesisCmd
    pGenesisVerKey =
      GenesisVerKey <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pGenesisAddr :: Parser GenesisCmd
    pGenesisAddr =
      GenesisAddr <$> pVerificationKeyFile Input <*> pNetworkId <*> pMaybeOutputFile

    pGenesisTxIn :: Parser GenesisCmd
    pGenesisTxIn =
      GenesisTxIn <$> pVerificationKeyFile Input <*> pNetworkId <*> pMaybeOutputFile

    pGenesisCreate :: Parser GenesisCmd
    pGenesisCreate =
      GenesisCreate <$> pGenesisDir
                    <*> pGenesisNumGenesisKeys
                    <*> pGenesisNumUTxOKeys
                    <*> pMaybeSystemStart
                    <*> pInitialSupply
                    <*> pNetworkId

    pGenesisHash :: Parser GenesisCmd
    pGenesisHash =
      GenesisHashFile <$> pGenesisFile

    pGenesisDir :: Parser GenesisDir
    pGenesisDir =
      GenesisDir <$>
        Opt.strOption
          (  Opt.long "genesis-dir"
          <> Opt.metavar "DIR"
          <> Opt.help "The genesis directory containing the genesis template and required genesis/delegation/spending keys."
          )

    pMaybeSystemStart :: Parser (Maybe SystemStart)
    pMaybeSystemStart =
      Opt.optional $
        SystemStart . convertTime <$>
          Opt.strOption
            (  Opt.long "start-time"
            <> Opt.metavar "UTC-TIME"
            <> Opt.help "The genesis start time in YYYY-MM-DDThh:mm:ssZ format. If unspecified, will be the current time +30 seconds."
            )

    pGenesisNumGenesisKeys :: Parser Word
    pGenesisNumGenesisKeys =
        Opt.option Opt.auto
          (  Opt.long "gen-genesis-keys"
          <> Opt.metavar "INT"
          <> Opt.help "The number of genesis keys to make [default is 0]."
          <> Opt.value 0
          )

    pGenesisNumUTxOKeys :: Parser Word
    pGenesisNumUTxOKeys =
        Opt.option Opt.auto
          (  Opt.long "gen-utxo-keys"
          <> Opt.metavar "INT"
          <> Opt.help "The number of UTxO keys to make [default is 0]."
          <> Opt.value 0
          )

    convertTime :: String -> UTCTime
    convertTime =
      parseTimeOrError False defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")

    pInitialSupply :: Parser (Maybe Lovelace)
    pInitialSupply =
      Opt.optional $
      Lovelace <$>
        Opt.option Opt.auto
          (  Opt.long "supply"
          <> Opt.metavar "LOVELACE"
          <> Opt.help "The initial coin supply in Lovelace which will be evenly distributed across initial stake holders."
          )


--
-- Shelley CLI flag parsers
--

data FileDirection
  = Input
  | Output
  deriving (Eq, Show)

pAddressKeyType :: Parser AddressKeyType
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


pProtocolParamsFile :: Parser ProtocolParamsFile
pProtocolParamsFile =
  ProtocolParamsFile <$>
    Opt.strOption
      (  Opt.long "protocol-params-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the JSON-encoded protocol parameters file"
      <> Opt.completer (Opt.bashCompleter "file")
      )

pCertificateFile :: Parser CertificateFile
pCertificateFile =
  CertificateFile <$>
    (  Opt.strOption
         (  Opt.long "certificate-file"
         <> Opt.metavar "FILE"
         <> Opt.help "Filepath of the certificate. This encompasses all \
                     \types of certificates (stake pool certificates, \
                     \stake key certificates etc)"
         <> Opt.completer (Opt.bashCompleter "file")
         )
    <|>
       Opt.strOption
         (  Opt.long "certificate"
         <> Opt.internal
         )
    )

pPoolMetaDataFile :: Parser PoolMetaDataFile
pPoolMetaDataFile =
  PoolMetaDataFile <$>
    Opt.strOption
      (  Opt.long "pool-metadata-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the pool metadata."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pTxMetadataJsonSchema :: Parser TxMetadataJsonSchema
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

pMetaDataFile :: Parser MetaDataFile
pMetaDataFile =
      MetaDataFileJSON <$>
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
      MetaDataFileCBOR <$>
        Opt.strOption
          (  Opt.long "metadata-cbor-file"
          <> Opt.metavar "FILE"
          <> Opt.help "Filepath of the metadata, in raw CBOR format."
          <> Opt.completer (Opt.bashCompleter "file")
          )

pWithdrawal :: Parser (StakeAddress, Lovelace)
pWithdrawal =
    Opt.option (readerFromAttoParser parseWithdrawal)
      (  Opt.long "withdrawal"
      <> Opt.metavar "WITHDRAWAL"
      <> Opt.help "The reward withdrawal as StakeAddress+Lovelace where \
                  \StakeAddress is the Bech32-encoded stake address \
                  \followed by the amount in Lovelace."
      )
  where
    parseWithdrawal :: Atto.Parser (StakeAddress, Lovelace)
    parseWithdrawal =
      (,) <$> parseStakeAddress <* Atto.char '+' <*> parseLovelace


pUpdateProposalFile :: Parser UpdateProposalFile
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


pColdSigningKeyFile :: Parser SigningKeyFile
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

pSomeWitnessSigningData :: Parser [WitnessSigningData]
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
    <|>
      ScriptWitnessSigningData <$> pScript

pSigningKeyFile :: FileDirection -> Parser SigningKeyFile
pSigningKeyFile fdir =
  SigningKeyFile <$>
    Opt.strOption
      (  Opt.long "signing-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help (show fdir ++ " filepath of the signing key.")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pWitnessSigningData :: Parser WitnessSigningData
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
  <|>
    ScriptWitnessSigningData <$> pScript

pKesPeriod :: Parser KESPeriod
pKesPeriod =
  KESPeriod <$>
    Opt.option Opt.auto
      (  Opt.long "kes-period"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The start of the KES key validity period."
      )

pEpochNo :: Parser EpochNo
pEpochNo =
  EpochNo <$>
    Opt.option Opt.auto
      (  Opt.long "epoch"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The epoch number."
      )


pEpochNoUpdateProp :: Parser EpochNo
pEpochNoUpdateProp =
  EpochNo <$>
    Opt.option Opt.auto
      (  Opt.long "epoch"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The epoch number in which the update proposal is valid."
      )

pGenesisFile :: Parser GenesisFile
pGenesisFile =
  GenesisFile <$>
    Opt.strOption
      (  Opt.long "genesis"
      <> Opt.metavar "FILE"
      <> Opt.help "The genesis file."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pOperatorCertIssueCounterFile :: Parser OpCertCounterFile
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


pOutputFormat :: Parser OutputFormat
pOutputFormat =
  Opt.option readOutputFormat
    (  Opt.long "output-format"
    <> Opt.metavar "STRING"
    <> Opt.help "Optional output format. Accepted output formats are \"hex\" \
                \and \"bech32\" (default is \"bech32\")."
    <> Opt.value OutputFormatBech32
    )


pMaybeOutputFile :: Parser (Maybe OutputFile)
pMaybeOutputFile =
  optional $
    OutputFile <$>
      Opt.strOption
        (  Opt.long "out-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Optional output file. Default is to write to stdout."
        <> Opt.completer (Opt.bashCompleter "file")
        )

pOutputFile :: Parser OutputFile
pOutputFile =
  OutputFile <$>
    Opt.strOption
      (  Opt.long "out-file"
      <> Opt.metavar "FILE"
      <> Opt.help "The output file."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pColdVerificationKeyFile :: Parser VerificationKeyFile
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

pVerificationKeyFile :: FileDirection -> Parser VerificationKeyFile
pVerificationKeyFile fdir =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help (show fdir ++ " filepath of the verification key.")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pExtendedVerificationKeyFile :: FileDirection -> Parser VerificationKeyFile
pExtendedVerificationKeyFile fdir =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "extended-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help (show fdir ++ " filepath of the ed25519-bip32 verification key.")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pGenesisVerificationKeyFile :: Parser VerificationKeyFile
pGenesisVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "genesis-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the genesis verification key."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pGenesisVerificationKeyHash :: Parser (Hash GenesisKey)
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

pGenesisVerificationKey :: Parser (VerificationKey GenesisKey)
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

pGenesisVerificationKeyOrFile :: Parser (VerificationKeyOrFile GenesisKey)
pGenesisVerificationKeyOrFile =
  VerificationKeyValue <$> pGenesisVerificationKey
    <|> VerificationKeyFilePath <$> pGenesisVerificationKeyFile

pGenesisVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile GenesisKey)
pGenesisVerificationKeyOrHashOrFile =
  VerificationKeyOrFile <$> pGenesisVerificationKeyOrFile
    <|> VerificationKeyHash <$> pGenesisVerificationKeyHash

pGenesisDelegateVerificationKeyFile :: Parser VerificationKeyFile
pGenesisDelegateVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "genesis-delegate-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the genesis delegate verification key."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pGenesisDelegateVerificationKeyHash :: Parser (Hash GenesisDelegateKey)
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

pGenesisDelegateVerificationKey :: Parser (VerificationKey GenesisDelegateKey)
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
  :: Parser (VerificationKeyOrFile GenesisDelegateKey)
pGenesisDelegateVerificationKeyOrFile =
  VerificationKeyValue <$> pGenesisDelegateVerificationKey
    <|> VerificationKeyFilePath <$> pGenesisDelegateVerificationKeyFile

pGenesisDelegateVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile GenesisDelegateKey)
pGenesisDelegateVerificationKeyOrHashOrFile =
  VerificationKeyOrFile <$> pGenesisDelegateVerificationKeyOrFile
    <|> VerificationKeyHash <$> pGenesisDelegateVerificationKeyHash

pKESVerificationKeyFile :: Parser VerificationKeyFile
pKESVerificationKeyFile =
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

pNetworkId :: Parser NetworkId
pNetworkId =
  pMainnet <|> fmap Testnet pTestnetMagic
 where
   pMainnet :: Parser NetworkId
   pMainnet =
    Opt.flag' Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

pTestnetMagic :: Parser NetworkMagic
pTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

pTxSubmitFile :: Parser FilePath
pTxSubmitFile =
  Opt.strOption
    (  Opt.long "tx-file"
    <> Opt.metavar "FILE"
    <> Opt.help "Filepath of the transaction you intend to submit."
    <> Opt.completer (Opt.bashCompleter "file")
    )

pTxIn :: Parser TxIn
pTxIn =
  Opt.option (readerFromAttoParser parseTxIn)
    (  Opt.long "tx-in"
    <> Opt.metavar "TX-IN"
    <> Opt.help "The input transaction as TxId#TxIx where TxId is the transaction hash and TxIx is the index."
    )

parseTxIn :: Atto.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Atto.char '#' *> parseTxIx)

renderTxIn :: TxIn -> Text
renderTxIn (TxIn txid (TxIx txix)) =
  mconcat
    [ Text.decodeUtf8 (serialiseToRawBytesHex txid)
    , "#"
    , Text.pack (show txix)
    ]

parseTxId :: Atto.Parser TxId
parseTxId = (<?> "Transaction ID (hexadecimal)") $ do
  bstr <- Atto.takeWhile1 Char.isHexDigit
  case deserialiseFromRawBytesHex AsTxId bstr of
    Just addr -> return addr
    Nothing -> fail $ "Incorrect transaction id format:: " ++ show bstr

parseTxIx :: Atto.Parser TxIx
parseTxIx = toEnum <$> Atto.decimal


pTxOut :: Parser (TxOut Shelley)
pTxOut =
  Opt.option (readerFromAttoParser parseTxOut)
    (  Opt.long "tx-out"
    <> Opt.metavar "TX-OUT"
    <> Opt.help "The ouput transaction as Address+Lovelace where Address is \
                \the Bech32-encoded address followed by the amount in \
                \Lovelace."
    )
  where
    parseTxOut :: Atto.Parser (TxOut Shelley)
    parseTxOut =
      TxOut <$> parseAddress <* Atto.char '+' <*> parseLovelace

pTxTTL :: Parser SlotNo
pTxTTL =
  SlotNo <$>
    Opt.option Opt.auto
      (  Opt.long "ttl"
      <> Opt.metavar "SLOT"
      <> Opt.help "Time to live (in slots)."
      )

pTxFee :: Parser Lovelace
pTxFee =
  Lovelace . (fromIntegral :: Natural -> Integer) <$>
    Opt.option Opt.auto
      (  Opt.long "fee"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The fee amount in Lovelace."
      )

pWitnessFile :: Parser WitnessFile
pWitnessFile =
  WitnessFile <$>
    Opt.strOption
      (  Opt.long "witness-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the witness"
      <> Opt.completer (Opt.bashCompleter "file")
      )

pTxBodyFile :: FileDirection -> Parser TxBodyFile
pTxBodyFile fdir =
    TxBodyFile <$>
      (  Opt.strOption
           (  Opt.long optName
           <> Opt.metavar "FILE"
           <> Opt.help (show fdir ++ " filepath of the TxBody.")
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


pTxFile :: FileDirection -> Parser TxFile
pTxFile fdir =
    TxFile <$>
      (  Opt.strOption
           (  Opt.long optName
           <> Opt.metavar "FILE"
           <> Opt.help (show fdir ++ " filepath of the Tx.")
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

pTxInCount :: Parser TxInCount
pTxInCount =
  TxInCount <$>
    Opt.option Opt.auto
      (  Opt.long "tx-in-count"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of transaction inputs."
      )

pTxOutCount :: Parser TxOutCount
pTxOutCount =
  TxOutCount <$>
    Opt.option Opt.auto
      (  Opt.long "tx-out-count"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of transaction outputs."
      )

pTxShelleyWitnessCount :: Parser TxShelleyWitnessCount
pTxShelleyWitnessCount =
  TxShelleyWitnessCount <$>
    Opt.option Opt.auto
      (  Opt.long "witness-count"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of Shelley key witnesses."
      )

pTxByronWitnessCount :: Parser TxByronWitnessCount
pTxByronWitnessCount =
  TxByronWitnessCount <$>
    Opt.option Opt.auto
      (  Opt.long "byron-witness-count"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of Byron key witnesses (default is 0)."
      <> Opt.value 0
      )

pQueryFilter :: Parser QueryFilter
pQueryFilter = pAddresses <|> pure NoFilter
  where
    pAddresses :: Parser QueryFilter
    pAddresses = FilterByAddress . Set.fromList <$>
                   some pFilterByAddress

pFilterByAddress :: Parser (Address Shelley)
pFilterByAddress =
    Opt.option (readerFromAttoParser parseAddress)
      (  Opt.long "address"
      <> Opt.metavar "ADDRESS"
      <> Opt.help "Filter by Cardano address(es) (Bech32-encoded)."
      )

pFilterByStakeAddress :: Parser StakeAddress
pFilterByStakeAddress =
    Opt.option (readerFromAttoParser parseStakeAddress)
      (  Opt.long "address"
      <> Opt.metavar "ADDRESS"
      <> Opt.help "Filter by Cardano stake address (Bech32-encoded)."
      )

pByronAddress :: Parser (Address Byron)
pByronAddress =
    Opt.option
      (Opt.eitherReader deserialise)
        (  Opt.long "address"
        <> Opt.metavar "STRING"
        <> Opt.help "Byron address (Base58-encoded)."
        )
  where
    deserialise :: String -> Either String (Address Byron)
    deserialise =
      maybe (Left "Invalid Byron address.") Right
        . deserialiseAddress AsByronAddress
        . Text.pack

pAddress :: Parser Text
pAddress =
  Text.pack <$>
    Opt.strOption
      (  Opt.long "address"
      <> Opt.metavar "ADDRESS"
      <> Opt.help "A Cardano address"
      )

pStakeVerificationKeyOrFile :: Parser (VerificationKeyOrFile StakeKey)
pStakeVerificationKeyOrFile =
  VerificationKeyValue <$> pStakeVerificationKey
    <|> VerificationKeyFilePath <$> pStakeVerificationKeyFile

pStakeVerificationKey :: Parser (VerificationKey StakeKey)
pStakeVerificationKey =
    Opt.option
      (Opt.eitherReader deserialiseFromBech32OrHex)
        (  Opt.long "stake-verification-key"
        <> Opt.metavar "STRING"
        <> Opt.help "Stake verification key (Bech32 or hex-encoded)."
        )
  where
    asType :: AsType (VerificationKey StakeKey)
    asType = AsVerificationKey AsStakeKey

    keyFormats :: NonEmpty (InputFormat (VerificationKey StakeKey))
    keyFormats = NE.fromList [InputFormatBech32, InputFormatHex]

    deserialiseFromBech32OrHex
      :: String
      -> Either String (VerificationKey StakeKey)
    deserialiseFromBech32OrHex str =
      first (Text.unpack . renderInputDecodeError) $
        deserialiseInput asType keyFormats (BSC.pack str)

pStakeVerificationKeyFile :: Parser VerificationKeyFile
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


pStakePoolVerificationKeyFile :: Parser VerificationKeyFile
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

pStakePoolVerificationKeyHash :: Parser (Hash StakePoolKey)
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

pStakePoolVerificationKey :: Parser (VerificationKey StakePoolKey)
pStakePoolVerificationKey =
    Opt.option
      (Opt.eitherReader deserialiseFromBech32OrHex)
        (  Opt.long "stake-pool-verification-key"
        <> Opt.metavar "STRING"
        <> Opt.help "Stake pool verification key (Bech32 or hex-encoded)."
        )
  where
    asType :: AsType (VerificationKey StakePoolKey)
    asType = AsVerificationKey AsStakePoolKey

    deserialiseFromBech32OrHex
      :: String
      -> Either String (VerificationKey StakePoolKey)
    deserialiseFromBech32OrHex str =
      case deserialiseFromBech32 asType (Text.pack str) of
        Right res -> Right res

        -- The input was valid Bech32, but some other error occurred.
        Left err@(Bech32UnexpectedPrefix _ _) -> Left (displayError err)
        Left err@(Bech32DataPartToBytesError _) -> Left (displayError err)
        Left err@(Bech32DeserialiseFromBytesError _) -> Left (displayError err)
        Left err@(Bech32WrongPrefix _ _) -> Left (displayError err)

        -- The input was not valid Bech32. Attempt to deserialize it as hex.
        Left (Bech32DecodingError _) ->
          case deserialiseFromRawBytesHex asType (BSC.pack str) of
            Just res' -> Right res'
            Nothing -> Left "Invalid stake pool verification key."

pStakePoolVerificationKeyOrFile
  :: Parser (VerificationKeyOrFile StakePoolKey)
pStakePoolVerificationKeyOrFile =
  VerificationKeyValue <$> pStakePoolVerificationKey
    <|> VerificationKeyFilePath <$> pStakePoolVerificationKeyFile

pStakePoolVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile StakePoolKey)
pStakePoolVerificationKeyOrHashOrFile =
  VerificationKeyOrFile <$> pStakePoolVerificationKeyOrFile
    <|> VerificationKeyHash <$> pStakePoolVerificationKeyHash

pVrfVerificationKeyFile :: Parser VerificationKeyFile
pVrfVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "vrf-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the VRF verification key."
      <> Opt.completer (Opt.bashCompleter "file")
      )

pVrfVerificationKeyHash :: Parser (Hash VrfKey)
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

pVrfVerificationKey :: Parser (VerificationKey VrfKey)
pVrfVerificationKey =
    Opt.option
      (Opt.eitherReader deserialiseFromBech32OrHex)
        (  Opt.long "vrf-verification-key"
        <> Opt.metavar "STRING"
        <> Opt.help "VRF verification key (Bech32 or hex-encoded)."
        )
  where
    asType :: AsType (VerificationKey VrfKey)
    asType = AsVerificationKey AsVrfKey

    keyFormats :: NonEmpty (InputFormat (VerificationKey VrfKey))
    keyFormats = NE.fromList [InputFormatBech32, InputFormatHex]

    deserialiseFromBech32OrHex
      :: String
      -> Either String (VerificationKey VrfKey)
    deserialiseFromBech32OrHex str =
      first (Text.unpack . renderInputDecodeError) $
        deserialiseInput asType keyFormats (BSC.pack str)

pVrfVerificationKeyOrFile :: Parser (VerificationKeyOrFile VrfKey)
pVrfVerificationKeyOrFile =
  VerificationKeyValue <$> pVrfVerificationKey
    <|> VerificationKeyFilePath <$> pVrfVerificationKeyFile

pVrfVerificationKeyOrHashOrFile :: Parser (VerificationKeyOrHashOrFile VrfKey)
pVrfVerificationKeyOrHashOrFile =
  VerificationKeyOrFile <$> pVrfVerificationKeyOrFile
    <|> VerificationKeyHash <$> pVrfVerificationKeyHash

pRewardAcctVerificationKeyFile :: Parser VerificationKeyFile
pRewardAcctVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "pool-reward-account-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the reward account staking verification key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
        (  Opt.long "reward-account-verification-key-file"
        <> Opt.internal
        )
    )


pPoolOwner :: Parser VerificationKeyFile
pPoolOwner =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "pool-owner-stake-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "Filepath of the pool owner staking verification key."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
          (  Opt.long "pool-owner-staking-verification-key"
          <> Opt.internal
          )
    )


pPoolPledge :: Parser Lovelace
pPoolPledge =
    Opt.option (readerFromAttoParser parseLovelace)
      (  Opt.long "pool-pledge"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The stake pool's pledge."
      )


pPoolCost :: Parser Lovelace
pPoolCost =
    Opt.option (readerFromAttoParser parseLovelace)
      (  Opt.long "pool-cost"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The stake pool's cost."
      )

pPoolMargin :: Parser Rational
pPoolMargin =
    Opt.option readRationalUnitInterval
      (  Opt.long "pool-margin"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "The stake pool's margin."
      )

pPoolRelay :: Parser StakePoolRelay
pPoolRelay = pSingleHostAddress <|> pSingleHostName <|> pMultiHostName

pMultiHostName :: Parser StakePoolRelay
pMultiHostName =
  StakePoolRelayDnsSrvRecord <$> pDNSName
 where
  pDNSName :: Parser ByteString
  pDNSName = Opt.option (Opt.eitherReader eDNSName)
               (  Opt.long "multi-host-pool-relay"
               <> Opt.metavar "STRING"
               <> Opt.help "The stake pool relay's DNS name that corresponds to \
                            \an SRV DNS record"
               )

pSingleHostName :: Parser StakePoolRelay
pSingleHostName =
  StakePoolRelayDnsARecord <$> pDNSName <*> optional pPort
 where
  pDNSName :: Parser ByteString
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

pSingleHostAddress :: Parser StakePoolRelay
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



pIpV4 :: Parser IP.IPv4
pIpV4 = Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv4)
          (  Opt.long "pool-relay-ipv4"
          <> Opt.metavar "STRING"
          <> Opt.help "The stake pool relay's IPv4 address"
          )

pIpV6 :: Parser IP.IPv6
pIpV6 = Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv6)
           (  Opt.long "pool-relay-ipv6"
           <> Opt.metavar "STRING"
           <> Opt.help "The stake pool relay's IPv6 address"
           )

pPort :: Parser PortNumber
pPort = Opt.option (fromInteger <$> Opt.eitherReader readEither)
           (  Opt.long "pool-relay-port"
           <> Opt.metavar "INT"
           <> Opt.help "The stake pool relay's port"
           )

pStakePoolMetadataReference :: Parser (Maybe StakePoolMetadataReference)
pStakePoolMetadataReference =
  optional $
    StakePoolMetadataReference
      <$> pStakePoolMetadataUrl
      <*> pStakePoolMetadataHash

pStakePoolMetadataUrl :: Parser URI
pStakePoolMetadataUrl =
  Opt.option (readURIOfMaxLength 64)
    (  Opt.long "metadata-url"
    <> Opt.metavar "URL"
    <> Opt.help "Pool metadata URL (maximum length of 64 characters)."
    )

pStakePoolMetadataHash :: Parser (Hash StakePoolMetadata)
pStakePoolMetadataHash =
    Opt.option
      (Opt.maybeReader metadataHash)
        (  Opt.long "metadata-hash"
        <> Opt.metavar "HASH"
        <> Opt.help "Pool metadata hash."
        )
  where
    getHashFromHexString :: String -> Maybe (Crypto.Hash Crypto.Blake2b_256 ByteString)
    getHashFromHexString = Crypto.hashFromBytesAsHex . BSC.pack

    metadataHash :: String -> Maybe (Hash StakePoolMetadata)
    metadataHash str = StakePoolMetadataHash <$> getHashFromHexString str

pStakePoolRegistrationCert :: Parser PoolCmd
pStakePoolRegistrationCert =
 PoolRegistrationCert
  <$> pStakePoolVerificationKeyFile
  <*> pVrfVerificationKeyFile
  <*> pPoolPledge
  <*> pPoolCost
  <*> pPoolMargin
  <*> pRewardAcctVerificationKeyFile
  <*> some pPoolOwner
  <*> many pPoolRelay
  <*> pStakePoolMetadataReference
  <*> pNetworkId
  <*> pOutputFile

pStakePoolRetirementCert :: Parser PoolCmd
pStakePoolRetirementCert =
  PoolRetirementCert
    <$> pStakePoolVerificationKeyFile
    <*> pEpochNo
    <*> pOutputFile


pShelleyProtocolParametersUpdate :: Parser ProtocolParametersUpdate
pShelleyProtocolParametersUpdate =
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

pMinFeeLinearFactor :: Parser Natural
pMinFeeLinearFactor =
    Opt.option Opt.auto
      (  Opt.long "min-fee-linear"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The linear factor for the minimum fee calculation."
      )

pMinFeeConstantFactor :: Parser Natural
pMinFeeConstantFactor =
    Opt.option Opt.auto
      (  Opt.long "min-fee-constant"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The constant factor for the minimum fee calculation."
      )

pMinUTxOValue :: Parser Lovelace
pMinUTxOValue =
    Opt.option (readerFromAttoParser parseLovelace)
      (  Opt.long "min-utxo-value"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The minimum allowed UTxO value."
      )

pMinPoolCost :: Parser Lovelace
pMinPoolCost =
    Opt.option (readerFromAttoParser parseLovelace)
      (  Opt.long "min-pool-cost"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The minimum allowed cost parameter for stake pools."
      )

pMaxBodySize :: Parser Natural
pMaxBodySize =
    Opt.option Opt.auto
      (  Opt.long "max-block-body-size"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Maximal block body size."
      )

pMaxTransactionSize :: Parser Natural
pMaxTransactionSize =
    Opt.option Opt.auto
      (  Opt.long "max-tx-size"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Maximum transaction size."
      )

pMaxBlockHeaderSize :: Parser Natural
pMaxBlockHeaderSize =
    Opt.option Opt.auto
      (  Opt.long "max-block-header-size"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Maximum block header size."
      )

pKeyRegistDeposit :: Parser Lovelace
pKeyRegistDeposit =
    Opt.option (readerFromAttoParser parseLovelace)
      (  Opt.long "key-reg-deposit-amt"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Key registration deposit amount."
      )

pPoolDeposit :: Parser Lovelace
pPoolDeposit =
    Opt.option (readerFromAttoParser parseLovelace)
      (  Opt.long "pool-reg-deposit"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The amount of a pool registration deposit."
      )

pEpochBoundRetirement :: Parser EpochNo
pEpochBoundRetirement =
    EpochNo <$>
    Opt.option Opt.auto
      (  Opt.long "pool-retirement-epoch-boundary"
      <> Opt.metavar "INT"
      <> Opt.help "Epoch bound on pool retirement."
      )

pNumberOfPools :: Parser Natural
pNumberOfPools =
    Opt.option Opt.auto
      (  Opt.long "number-of-pools"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Desired number of pools."
      )

pPoolInfluence :: Parser Rational
pPoolInfluence =
    Opt.option readRational
      (  Opt.long "pool-influence"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "Pool influence."
      )

pTreasuryExpansion :: Parser Rational
pTreasuryExpansion =
    Opt.option readRationalUnitInterval
      (  Opt.long "treasury-expansion"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "Treasury expansion."
      )

pMonetaryExpansion :: Parser Rational
pMonetaryExpansion =
    Opt.option readRationalUnitInterval
      (  Opt.long "monetary-expansion"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "Monetary expansion."
      )

pDecentralParam :: Parser Rational
pDecentralParam =
    Opt.option readRationalUnitInterval
      (  Opt.long "decentralization-parameter"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "Decentralization parameter."
      )

pExtraEntropy :: Parser (Maybe ByteString)
pExtraEntropy =
      Opt.option (Just <$> readerFromAttoParser parseEntropyBytes)
        (  Opt.long "extra-entropy"
        <> Opt.metavar "HEX"
        <> Opt.help "Praos extra entropy, as a hex byte string."
        )
  <|> Opt.flag' Nothing
        (  Opt.long "reset-extra-entropy"
        <> Opt.help "Reset the Praos extra entropy to none."
        )
  where
    parseEntropyBytes :: Atto.Parser ByteString
    parseEntropyBytes =
      fst . Base16.decode <$> Atto.takeWhile1 Char.isHexDigit


pProtocol :: Parser Protocol
pProtocol =
    (  Opt.flag' ()
        (  Opt.long "shelley-mode"
        <> Opt.help "For talking to a node running in Shelley-only mode."
        )
    *> pShelley
    )
  <|>
    (  Opt.flag' ()
        (  Opt.long "byron-mode"
        <> Opt.help "For talking to a node running in Byron-only mode."
        )
    *> pByron
    )
  <|>
    (  Opt.flag' ()
        (  Opt.long "cardano-mode"
        <> Opt.help "For talking to a node running in full Cardano mode (default)."
        )
    *> pCardano
    )
  <|>
    -- Default to the Cardano protocol.
    pure
      (CardanoProtocol
        (EpochSlots defaultByronEpochSlots))
  where
    pByron :: Parser Protocol
    pByron = ByronProtocol <$> pEpochSlots

    pShelley :: Parser Protocol
    pShelley = pure ShelleyProtocol

    pCardano :: Parser Protocol
    pCardano = CardanoProtocol <$> pEpochSlots

    pEpochSlots :: Parser EpochSlots
    pEpochSlots =
      EpochSlots <$>
        Opt.option Opt.auto
          (  Opt.long "epoch-slots"
          <> Opt.metavar "NATURAL"
          <> Opt.help "The number of slots per epoch for the Byron era."
          <> Opt.value defaultByronEpochSlots -- Default to the mainnet value.
          <> Opt.showDefault
          )

    defaultByronEpochSlots :: Word64
    defaultByronEpochSlots = 21600

pProtocolVersion :: Parser (Natural, Natural)
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

parseLovelace :: Atto.Parser Lovelace
parseLovelace = Lovelace <$> Atto.decimal

parseAddress :: Atto.Parser (Address Shelley)
parseAddress = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsShelleyAddress str of
      Nothing   -> fail "invalid address"
      Just addr -> pure addr

parseStakeAddress :: Atto.Parser StakeAddress
parseStakeAddress = do
    str <- lexPlausibleAddressString
    case deserialiseAddress AsStakeAddress str of
      Nothing   -> fail "invalid address"
      Just addr -> pure addr

lexPlausibleAddressString :: Atto.Parser Text
lexPlausibleAddressString =
    Text.decodeLatin1 <$> Atto.takeWhile1 isPlausibleAddressChar
  where
    -- Covers both base58 and bech32 (with constrained prefixes)
    isPlausibleAddressChar c =
         (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c == '_'


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

readOutputFormat :: Opt.ReadM OutputFormat
readOutputFormat = do
  s <- Opt.str
  case s of
    "hex" -> pure OutputFormatHex
    "bech32" -> pure OutputFormatBech32
    _ ->
      fail $ "Invalid output format: \""
        <> s
        <> "\". Accepted output formats are \"hex\" and \"bech32\"."

readURIOfMaxLength :: Int -> Opt.ReadM URI
readURIOfMaxLength maxLen = do
  s <- readStringOfMaxLength maxLen
  maybe (fail "The provided string must be a valid URI.") pure (parseURI s)

readStringOfMaxLength :: Int -> Opt.ReadM String
readStringOfMaxLength maxLen = do
  s <- Opt.str
  let strLen = length s
  if strLen <= maxLen
    then pure s
    else fail $
      "The provided string must have at most 64 characters, but it has "
        <> show strLen
        <> " characters."

readRationalUnitInterval :: Opt.ReadM Rational
readRationalUnitInterval = readRational >>= checkUnitInterval
  where
   checkUnitInterval :: Rational -> Opt.ReadM Rational
   checkUnitInterval q
     | q >= 0 && q <= 1 = return q
     | otherwise        = fail "Please enter a value in the range [0,1]"

readRational :: Opt.ReadM Rational
readRational = toRational <$> readerFromAttoParser Atto.scientific

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)
