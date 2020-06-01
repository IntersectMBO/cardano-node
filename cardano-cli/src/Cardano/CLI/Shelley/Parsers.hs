module Cardano.CLI.Shelley.Parsers
  ( -- * CLI command parser
    parseShelleyCommands

    -- * CLI command and flag types
  , module Cardano.CLI.Shelley.Commands
  ) where

import           Prelude (String)
import           Cardano.Prelude hiding (option)

import           Control.Monad.Fail (fail)
import qualified Data.ByteString.Char8 as C8
import qualified Data.IP as IP
import           Data.Ratio (approxRational)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeOrError)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley

import           Cardano.Api
import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Config.Types (SigningKeyFile(..), CertificateFile (..),
                   UpdateProposalFile (..))
import           Cardano.Config.Parsers (parseNodeAddress)
import           Cardano.Config.Shelley.OCert (KESPeriod(..))

import           Cardano.CLI.Shelley.Commands

import           Cardano.Crypto.Hash (hashFromBytes)

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
      , Opt.command "block"
          (Opt.info (BlockCmd <$> pBlockCmd) $ Opt.progDesc "Shelley block commands")
      , Opt.command "system"
          (Opt.info (SystemCmd <$> pSystemCmd) $ Opt.progDesc "Shelley system commands")
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
          (Opt.info pAddressKeyGen $ Opt.progDesc "Create a single address key pair.")
      , Opt.command "key-hash"
          (Opt.info pAddressKeyHash $ Opt.progDesc "Print the hash of an address key to stdout.")
      , Opt.command "build"
          (Opt.info pAddressBuild $ Opt.progDesc "Build a Shelley payment address, with optional delegation to a stake address.")
      , Opt.command "build-multisig"
          (Opt.info pAddressBuildMultiSig $ Opt.progDesc "Build a Shelley payment multi-sig address.")
      , Opt.command "info"
          (Opt.info pAddressInfo $ Opt.progDesc "Print information about an address.")
      ]
  where
    pAddressKeyGen :: Parser AddressCmd
    pAddressKeyGen = AddressKeyGen <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pAddressKeyHash :: Parser AddressCmd
    pAddressKeyHash = AddressKeyHash <$> pPaymentVerificationKeyFile <*> pMaybeOutputFile

    pAddressBuild :: Parser AddressCmd
    pAddressBuild =
      AddressBuild
        <$> pPaymentVerificationKeyFile
        <*> Opt.optional pStakeVerificationKeyFile
        <*> pNetwork
        <*> pMaybeOutputFile


    pAddressBuildMultiSig :: Parser AddressCmd
    pAddressBuildMultiSig = pure AddressBuildMultiSig

    pAddressInfo :: Parser AddressCmd
    pAddressInfo = AddressInfo <$> pAddress


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


pStakeAddress :: Parser StakeAddressCmd
pStakeAddress =
  Opt.subparser $
    mconcat
      [ Opt.command "key-gen"
          (Opt.info pStakeAddressKeyGen $ Opt.progDesc "Create a stake address key pair")
      , Opt.command "build"
          (Opt.info pStakeAddressBuild $ Opt.progDesc "Build a stake address")
      , Opt.command "register"
          (Opt.info pStakeAddressRegister $ Opt.progDesc "Register a stake address")
      , Opt.command "delegate"
          (Opt.info pStakeAddressDelegate $ Opt.progDesc "Delegate from a stake address to a stake pool")
      , Opt.command "de-register"
          (Opt.info pStakeAddressDeRegister $ Opt.progDesc "De-register a stake address")
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

    pStakeAddressBuild :: Parser StakeAddressCmd
    pStakeAddressBuild = StakeAddressBuild <$> pStakeVerificationKeyFile
                                           <*> pNetwork
                                           <*> pMaybeOutputFile

    pStakeAddressRegister :: Parser StakeAddressCmd
    pStakeAddressRegister = StakeKeyRegister <$> pPrivKeyFile <*> parseNodeAddress

    pStakeAddressDelegate :: Parser StakeAddressCmd
    pStakeAddressDelegate =
      StakeKeyDelegate <$> pPrivKeyFile <*> pPoolId <*> pDelegationFee <*> parseNodeAddress

    pStakeAddressDeRegister :: Parser StakeAddressCmd
    pStakeAddressDeRegister = StakeKeyDeRegister <$> pPrivKeyFile <*> parseNodeAddress

    pStakeAddressRegistrationCert :: Parser StakeAddressCmd
    pStakeAddressRegistrationCert = StakeKeyRegistrationCert
                                      <$> pStakeVerificationKeyFile
                                      <*> pOutputFile

    pStakeAddressDeregistrationCert :: Parser StakeAddressCmd
    pStakeAddressDeregistrationCert = StakeKeyDeRegistrationCert
                                        <$> pStakeVerificationKeyFile
                                        <*> pOutputFile

    pStakeAddressDelegationCert :: Parser StakeAddressCmd
    pStakeAddressDelegationCert = StakeKeyDelegationCert
                                    <$> pStakeVerificationKeyFile
                                    <*> pPoolStakingVerificationKeyFile
                                    <*> pOutputFile



    pDelegationFee :: Parser Lovelace
    pDelegationFee =
      Lovelace <$>
        Opt.option Opt.auto
          (  Opt.long "delegation-fee"
          <> Opt.metavar "LOVELACE"
          <> Opt.help "The delegation fee in Lovelace."
          )

pTransaction :: Parser TransactionCmd
pTransaction =
  Opt.subparser $
    mconcat
      [ Opt.command "build-raw"
          (Opt.info pTransactionBuild $ Opt.progDesc "Build a transaction (low-level, inconvenient)")
      , Opt.command "sign"
          (Opt.info pTransactionSign $ Opt.progDesc "Sign a transaction")
      , Opt.command "witness"
          (Opt.info pTransactionWitness $ Opt.progDesc "Witness a transaction")
      , Opt.command "sign-witness"
          (Opt.info pTransactionSignWit $ Opt.progDesc "Sign and witness a transaction")
      , Opt.command "check"
          (Opt.info pTransactionCheck $ Opt.progDesc "Check a transaction")
      , Opt.command "submit"
          (Opt.info pTransactionSubmit . Opt.progDesc $
             mconcat
               [ "Submit a transaction to the local node whose Unix domain socket "
               , "is obtained from the CARDANO_NODE_SOCKET_PATH enviromnent variable."
               ]
            )
      , Opt.command "calculate-min-fee"
          (Opt.info pTransactionCalculateMinFee $ Opt.progDesc "Calulate the min fee for a transaction")
      , Opt.command "info"
          (Opt.info pTransactionInfo $ Opt.progDesc "Print information about a transaction")
      ]
  where
    pTransactionBuild :: Parser TransactionCmd
    pTransactionBuild = TxBuildRaw <$> some pTxIn
                                   <*> some pTxOut
                                   <*> pTxTTL
                                   <*> pTxFee
                                   <*> many pCertificateFile
                                   <*> optional pUpdateProposalFile
                                   <*> pTxBodyFile Output

    pTransactionSign  :: Parser TransactionCmd
    pTransactionSign = TxSign <$> pTxBodyFile Input
                              <*> pSomeSigningKeyFiles
                              <*> pNetwork
                              <*> pTxFile Output

    pTransactionWitness :: Parser TransactionCmd
    pTransactionWitness = pure TxWitness

    pTransactionSignWit :: Parser TransactionCmd
    pTransactionSignWit = pure TxSignWitness

    pTransactionCheck  :: Parser TransactionCmd
    pTransactionCheck = pure TxCheck

    pTransactionSubmit  :: Parser TransactionCmd
    pTransactionSubmit = TxSubmit <$> pTxSubmitFile
                                  <*> pNetwork

    pTransactionCalculateMinFee :: Parser TransactionCmd
    pTransactionCalculateMinFee =
      TxCalculateMinFee
        <$> pTxInCount
        <*> pTxOutCount
        <*> pTxTTL
        <*> pNetwork
        <*> pSomeSigningKeyFiles
        <*> many pCertificateFile
        <*> pProtocolParamsFile

    pTransactionInfo  :: Parser TransactionCmd
    pTransactionInfo = pure TxInfo


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
      [ Opt.command "register"
          (Opt.info pPoolRegster $ Opt.progDesc "Register a stake pool")
      , Opt.command "re-register"
          (Opt.info pPoolReRegster $ Opt.progDesc "Re-register a stake pool")
      , Opt.command "retire"
          (Opt.info pPoolRetire $ Opt.progDesc "Retire a stake pool")
      , Opt.command "registration-certificate"
          (Opt.info pStakePoolRegistrationCert $ Opt.progDesc "Create a stake pool registration certificate")
      , Opt.command "deregistration-certificate"
          (Opt.info pStakePoolRetirementCert $ Opt.progDesc "Create a stake pool deregistration certificate")
      , Opt.command "id"
          (Opt.info pId $
             Opt.progDesc "Build pool id from the offline key")
      ]
  where
    pPoolRegster :: Parser PoolCmd
    pPoolRegster = PoolRegister <$> pPoolId

    pPoolReRegster :: Parser PoolCmd
    pPoolReRegster = PoolReRegister <$> pPoolId

    pPoolRetire :: Parser PoolCmd
    pPoolRetire = PoolRetire <$> pPoolId <*> pEpochNo <*> parseNodeAddress

    pId :: Parser PoolCmd
    pId = PoolGetId <$> pVerificationKeyFile Output


pQueryCmd :: Parser QueryCmd
pQueryCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "pool-id"
          (Opt.info pQueryPoolId $ Opt.progDesc "Get the node's pool id")
      , Opt.command "protocol-parameters"
          (Opt.info pQueryProtocolParameters $ Opt.progDesc "Get the node's current protocol parameters")
      , Opt.command "tip"
          (Opt.info pQueryTip $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)")
      , Opt.command "stake-distribution"
          (Opt.info pQueryStakeDistribution $ Opt.progDesc "Get the node's current aggregated stake distribution")
      , Opt.command "utxo"
          (Opt.info pQueryUTxO $ Opt.progDesc "Get the node's current UTxO with the option of \
                                              \filtering by address(es)")
      , Opt.command "version"
          (Opt.info pQueryVersion $ Opt.progDesc "Get the node version")
      , Opt.command "ledger-state"
          (Opt.info pQueryLedgerState $ Opt.progDesc "Dump the current state of the node")
      , Opt.command "status"
          (Opt.info pQueryStatus $ Opt.progDesc "Get the status of the node")
      ]
  where
    pQueryPoolId :: Parser QueryCmd
    pQueryPoolId = QueryPoolId <$> parseNodeAddress

    pQueryProtocolParameters :: Parser QueryCmd
    pQueryProtocolParameters =
      QueryProtocolParameters
        <$> pNetwork
        <*> pMaybeOutputFile

    pQueryTip :: Parser QueryCmd
    pQueryTip = QueryTip <$> pNetwork <*> pMaybeOutputFile

    pQueryUTxO :: Parser QueryCmd
    pQueryUTxO =
      QueryUTxO
        <$> pQueryFilter
        <*> pNetwork
        <*> pMaybeOutputFile

    pQueryStakeDistribution :: Parser QueryCmd
    pQueryStakeDistribution =
      QueryStakeDistribution
        <$> pNetwork
        <*> pMaybeOutputFile

    pQueryVersion :: Parser QueryCmd
    pQueryVersion = QueryVersion <$> parseNodeAddress

    pQueryLedgerState :: Parser QueryCmd
    pQueryLedgerState = QueryLedgerState <$> pNetwork <*> pMaybeOutputFile

    pQueryStatus :: Parser QueryCmd
    pQueryStatus = QueryStatus <$> parseNodeAddress


pBlockCmd :: Parser BlockCmd
pBlockCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "info"
          (Opt.info pBlockInfo $ Opt.progDesc "Get the node's pool id")
      ]
  where
    pBlockInfo :: Parser BlockCmd
    pBlockInfo = BlockInfo <$> pBlockId <*> parseNodeAddress

pGovernanceCmd :: Parser GovernanceCmd
pGovernanceCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "create-mir-certificate"
          (Opt.info pMIRCertificate $
            Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate")
      , Opt.command "create-update-proposal"
          (Opt.info pUpdateProposal $
            Opt.progDesc "Create an update proposal")
      , Opt.command "protocol-update"
          (Opt.info pProtocolUpdate $ Opt.progDesc "Protocol update")
      , Opt.command "cold-keys"
          (Opt.info pColdKeys $ Opt.progDesc "Cold keys")
      ]
  where
    pMIRCertificate :: Parser GovernanceCmd
    pMIRCertificate = GovernanceMIRCertificate
                        <$> some pStakeVerificationKeyFile
                        <*> some pRewardAmt
                        <*> pOutputFile

    pUpdateProposal :: Parser GovernanceCmd
    pUpdateProposal = GovernanceUpdateProposal
                        <$> pOutputFile
                        <*> pEpochNoUpdateProp
                        <*> some pGenesisVerificationKeyFile
                        <*> pShelleyPParamsUpdate

    pProtocolUpdate :: Parser GovernanceCmd
    pProtocolUpdate = GovernanceProtocolUpdate <$> pColdSigningKeyFile

    pColdKeys :: Parser GovernanceCmd
    pColdKeys = GovernanceColdKeys <$> pGenesisKeyFile

    pGenesisKeyFile :: Parser SigningKeyFile
    pGenesisKeyFile =
      pColdSigningKeyFile
        <|> Opt.strOption
              (  Opt.long "genesis-key"
              <> Opt.internal
              )

pRewardAmt :: Parser ShelleyCoin
pRewardAmt =
  Shelley.Coin <$>
    Opt.option Opt.auto
      (  Opt.long "reward"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The reward for the relevant reward account."
      )

pSystemCmd :: Parser SystemCmd
pSystemCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "start"
          (Opt.info pSystemStart $ Opt.progDesc "Start system")
      , Opt.command "stop"
          (Opt.info pSystemStop $ Opt.progDesc "Stop system")
      ]
  where
    pSystemStart :: Parser SystemCmd
    pSystemStart = SysStart <$> pGenesisFile <*> parseNodeAddress

    pSystemStop :: Parser SystemCmd
    pSystemStop = SysStop <$> parseNodeAddress


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
      GenesisKeyHash <$> pVerificationKeyFile Input

    pGenesisVerKey :: Parser GenesisCmd
    pGenesisVerKey =
      GenesisVerKey <$> pVerificationKeyFile Output <*> pSigningKeyFile Output

    pGenesisAddr :: Parser GenesisCmd
    pGenesisAddr =
      GenesisAddr <$> pVerificationKeyFile Input <*> pNetwork <*> pMaybeOutputFile

    pGenesisTxIn :: Parser GenesisCmd
    pGenesisTxIn =
      GenesisTxIn <$> pVerificationKeyFile Input <*> pNetwork <*> pMaybeOutputFile

    pGenesisCreate :: Parser GenesisCmd
    pGenesisCreate =
      GenesisCreate <$> pGenesisDir
                    <*> pGenesisNumGenesisKeys
                    <*> pGenesisNumUTxOKeys
                    <*> pMaybeSystemStart
                    <*> pInitialSupply
                    <*> pNetwork

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

pSomeSigningKeyFiles :: Parser [SigningKeyFile]
pSomeSigningKeyFiles =
  some $
    SigningKeyFile <$>
      Opt.strOption
      (  Opt.long "signing-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help ("Input filepath of the signing key (one or more).")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pSigningKeyFile :: FileDirection -> Parser SigningKeyFile
pSigningKeyFile fdir =
  SigningKeyFile <$>
    Opt.strOption
      (  Opt.long "signing-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help (show fdir ++ " filepath of the signing key.")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pBlockId :: Parser BlockId
pBlockId =
  BlockId <$>
    Opt.strOption
      (  Opt.long "block-id"
      <> Opt.metavar "STRING"
      <> Opt.help "The block identifier."
      )

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

pPoolId :: Parser PoolId
pPoolId =
  PoolId <$>
    Opt.strOption
      (  Opt.long "pool-id"
      <> Opt.metavar "STRING"
      <> Opt.help "The pool identifier."
      )

pPrivKeyFile :: Parser PrivKeyFile
pPrivKeyFile =
  PrivKeyFile <$>
    ( Opt.strOption
        (  Opt.long "signing-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help "The private key file."
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
        (  Opt.long "private-key"
        <> Opt.internal
        )

    )

pColdVerificationKeyFile :: Parser VerificationKeyFile
pColdVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "cold-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help ("Filepath of the cold verification key.")
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

pGenesisVerificationKeyFile :: Parser VerificationKeyFile
pGenesisVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "genesis-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help "Filepath of the genesis verification key."
      <> Opt.completer (Opt.bashCompleter "file")
      )


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


pNetwork :: Parser Network
pNetwork =
  pMainnet <|> fmap Testnet pTestnetMagic

pMainnet :: Parser Network
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
  Opt.option (Opt.eitherReader (parseTxIn . Text.pack))
    (  Opt.long "tx-in"
    <> Opt.metavar "TX-IN"
    <> Opt.help "The input transaction as TxId#TxIx where TxId is the transaction hash and TxIx is the index."
    )

pTxOut :: Parser TxOut
pTxOut =
  Opt.option (Opt.maybeReader (parseTxOut . Text.pack))
    (  Opt.long "tx-out"
    <> Opt.metavar "TX-OUT"
    <> Opt.help "The ouput transaction as TxOut+Lovelace where TxOut is the hex encoded address followed by the amount in Lovelace."
    )

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
  Lovelace <$>
    Opt.option Opt.auto
      (  Opt.long "fee"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The fee amount in Lovelace."
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

pQueryFilter :: Parser QueryFilter
pQueryFilter = pAddresses <|> pure NoFilter
  where
    pAddresses :: Parser QueryFilter
    pAddresses = FilterByAddress . Set.fromList <$> some pFilterByHexEncodedAddress

pFilterByHexEncodedAddress :: Parser Address
pFilterByHexEncodedAddress =
  Opt.option maybeHexEncodedAddressReader
    (  Opt.long "address"
    <> Opt.metavar "ADDRESS"
    <> Opt.help "Filter by Cardano address(es) (hex-encoded)."
    )

maybeHexEncodedAddressReader :: Opt.ReadM Address
maybeHexEncodedAddressReader = Opt.maybeReader (addressFromHex . Text.pack)

pAddress :: Parser Text
pAddress =
  Text.pack <$>
    Opt.strOption
      (  Opt.long "address"
      <> Opt.metavar "ADDRESS"
      <> Opt.help "A Cardano address"
      )


pStakeVerificationKeyFile :: Parser VerificationKeyFile
pStakeVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "stake-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help ("Filepath of the staking verification key.")
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
        (  Opt.long "staking-verification-key-file"
        <> Opt.internal
        )
    )


pPoolStakingVerificationKeyFile :: Parser VerificationKeyFile
pPoolStakingVerificationKeyFile =
  VerificationKeyFile <$>
    (  Opt.strOption
         (  Opt.long "cold-verification-key-file"
         <> Opt.metavar "FILE"
         <> Opt.help ("Filepath of the stake pool verification key.")
         <> Opt.completer (Opt.bashCompleter "file")
         )
    <|>
       Opt.strOption
         (  Opt.long "stake-pool-verification-key-file"
         <> Opt.internal
         )
    )

pVRFVerificationKeyFile :: Parser VerificationKeyFile
pVRFVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "vrf-verification-key-file"
      <> Opt.metavar "FILE"
      <> Opt.help ("Filepath of the VRF verification key.")
      <> Opt.completer (Opt.bashCompleter "file")
      )

pRewardAcctVerificationKeyFile :: Parser VerificationKeyFile
pRewardAcctVerificationKeyFile =
  VerificationKeyFile <$>
    ( Opt.strOption
        (  Opt.long "pool-reward-account-verification-key-file"
        <> Opt.metavar "FILE"
        <> Opt.help ("Filepath of the reward account staking verification key.")
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
        <> Opt.help ("Filepath of the pool owner staking verification key.")
        <> Opt.completer (Opt.bashCompleter "file")
        )
    <|>
      Opt.strOption
          (  Opt.long "pool-owner-staking-verification-key"
          <> Opt.internal
          )
    )


pPoolPledge :: Parser ShelleyCoin
pPoolPledge =
  Shelley.Coin <$>
    Opt.option Opt.auto
      (  Opt.long "pool-pledge"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The stake pool's pledge."
      )


pPoolCost :: Parser ShelleyCoin
pPoolCost =
  Shelley.Coin <$>
    Opt.option Opt.auto
      (  Opt.long "pool-cost"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The stake pool's cost."
      )

pPoolMargin :: Parser ShelleyStakePoolMargin
pPoolMargin =
  (\dbl -> maybeOrFail . Shelley.mkUnitInterval $ approxRational (dbl :: Double) 0) <$>
    Opt.option Opt.auto
      (  Opt.long "pool-margin"
      <> Opt.metavar "DOUBLE"
      <> Opt.help "The stake pool's margin."
      )
  where
    maybeOrFail (Just mgn) = mgn
    maybeOrFail Nothing = panic "Pool margin outside of [0,1] range."

_pPoolRelay :: Parser ShelleyStakePoolRelay
_pPoolRelay = Shelley.SingleHostAddr Shelley.SNothing
               <$> (Shelley.maybeToStrictMaybe <$> optional _pIpV4)
               <*> (Shelley.maybeToStrictMaybe <$> optional _pIpV6)

_pIpV4 :: Parser IP.IPv4
_pIpV4 = Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv4)
          (  Opt.long "pool-relay-ipv4"
          <> Opt.metavar "STRING"
          <> Opt.help "The stake pool relay's IpV4 address"
          )

_pIpV6 :: Parser IP.IPv6
_pIpV6 = Opt.option (Opt.maybeReader readMaybe :: Opt.ReadM IP.IPv6)
          (  Opt.long "pool-relay-ipv6"
          <> Opt.metavar "STRING"
          <> Opt.help "The stake pool relay's IpV6 address"
          )

pStakePoolRegistrationCert :: Parser PoolCmd
pStakePoolRegistrationCert =
 PoolRegistrationCert
  <$> pPoolStakingVerificationKeyFile
  <*> pVRFVerificationKeyFile
  <*> pPoolPledge
  <*> pPoolCost
  <*> pPoolMargin
  <*> pRewardAcctVerificationKeyFile
  <*> some pPoolOwner
  <*> pure []  --TODO: the relays
  <*> pNetwork
  <*> pOutputFile

pStakePoolRetirementCert :: Parser PoolCmd
pStakePoolRetirementCert =
  PoolRetirementCert
    <$> pPoolStakingVerificationKeyFile
    <*> pEpochNo
    <*> pOutputFile


pShelleyPParamsUpdate :: Parser ShelleyPParamsUpdate
pShelleyPParamsUpdate =
  PParams
    <$> (maybeToStrictMaybe <$> pMinFeeLinearFactor)
    <*> (maybeToStrictMaybe <$> pMinFeeConstantFactor)
    <*> (maybeToStrictMaybe <$> pMaxBodySize)
    <*> (maybeToStrictMaybe <$> pMaxTransactionSize)
    <*> (maybeToStrictMaybe <$> pMaxBlockHeaderSize)
    <*> (maybeToStrictMaybe <$> pKeyRegistDeposit)
    <*> (maybeToStrictMaybe <$> pMinRefund)
    <*> (maybeToStrictMaybe <$> pDepositDecay)
    <*> (maybeToStrictMaybe <$> pPoolDeposit)
    <*> (maybeToStrictMaybe <$> pPoolMinRefund)
    <*> (maybeToStrictMaybe <$> pPoolDecayRate)
    <*> (maybeToStrictMaybe <$> pEpochBoundRetirement)
    <*> (maybeToStrictMaybe <$> pNumberOfPools)
    <*> (maybeToStrictMaybe <$> pPoolInfluence)
    <*> (maybeToStrictMaybe <$> pMonetaryExpansion)
    <*> (maybeToStrictMaybe <$> pTreasuryExpansion)
    <*> (maybeToStrictMaybe <$> pDecentralParam)
    <*> (maybeToStrictMaybe <$> pExtraEntropy)
    <*> (maybeToStrictMaybe <$> pProtocolVersion)
    <*> (maybeToStrictMaybe <$> pMinUTxOValue)

pMinFeeLinearFactor :: Parser (Maybe Natural)
pMinFeeLinearFactor =
  optional
    $ Opt.option Opt.auto
        (  Opt.long "min-fee-linear"
        <> Opt.metavar "NATURAL"
        <> Opt.help "The linear factor for the minimum fee calculation."
        )

pMinFeeConstantFactor :: Parser (Maybe Natural)
pMinFeeConstantFactor =
  optional
  $ Opt.option Opt.auto
      (  Opt.long "min-fee-constant"
      <> Opt.metavar "LOVELACE"
      <> Opt.help "The constant factor for the minimum fee calculation."
      )

pMinUTxOValue :: Parser (Maybe Natural)
pMinUTxOValue =
  Opt.optional $
    Opt.option Opt.auto
      (  Opt.long "min-utxo-value"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The minimum allowed UTxO value."
      )

pMaxBodySize :: Parser (Maybe Natural)
pMaxBodySize =
  optional
    $ Opt.option Opt.auto
        (  Opt.long "max-block-body-size"
        <> Opt.metavar "NATURAL"
        <> Opt.help "Maximal block body size."
        )

pMaxTransactionSize :: Parser (Maybe Natural)
pMaxTransactionSize =
  optional
    $ Opt.option Opt.auto
        (  Opt.long "max-tx-size"
        <> Opt.metavar "NATURAL"
        <> Opt.help "Maximum transaction size."
        )

pMaxBlockHeaderSize :: Parser (Maybe Natural)
pMaxBlockHeaderSize =
  optional
    $ Opt.option Opt.auto
        (  Opt.long "max-block-header-size"
        <> Opt.metavar "NATURAL"
        <> Opt.help "Maximum block header size."
        )

pKeyRegistDeposit :: Parser (Maybe ShelleyCoin)
pKeyRegistDeposit =
  optional
    $ Shelley.Coin
        <$> Opt.option Opt.auto
              (  Opt.long "key-reg-deposit-amt"
              <> Opt.metavar "NATURAL"
              <> Opt.help "Key registration deposit amount."
              )



pMinRefund :: Parser (Maybe UnitInterval)
pMinRefund =
   optional
     $ Opt.option pFieldUnitInterval
       (  Opt.long "min-percent-refund"
       <> Opt.metavar "DOUBLE"
       <> Opt.help "The refund guarantee minimum percent."
       )


pDepositDecay :: Parser (Maybe Rational)
pDepositDecay =
  optional
    $ Opt.option Opt.auto
        (  Opt.long "deposit-decay-rate"
        <> Opt.metavar "DOUBLE"
        <> Opt.help "The deposit decay rate."
        )

pPoolDeposit :: Parser (Maybe ShelleyCoin)
pPoolDeposit =
  optional $
    Shelley.Coin
      <$> Opt.option Opt.auto
            (  Opt.long "pool-reg-deposit"
            <> Opt.metavar "NATURAL"
            <> Opt.help "The amount of a pool registration deposit."
            )


pPoolMinRefund :: Parser (Maybe UnitInterval)
pPoolMinRefund =
  optional
    $ Opt.option pFieldUnitInterval
        (  Opt.long "min-pool-percent-refund"
        <> Opt.metavar "DOUBLE"
        <> Opt.help "The pool refund minimum percent."
        )


pPoolDecayRate :: Parser (Maybe Rational)
pPoolDecayRate =
  optional
    $ Opt.option Opt.auto
        (  Opt.long "pool-deposit-decay-rate"
        <> Opt.metavar "DOUBLE"
        <> Opt.help "Decay rate for pool deposits."
        )


pEpochBoundRetirement :: Parser (Maybe EpochNo)
pEpochBoundRetirement =
  optional $
    EpochNo
       <$> Opt.option Opt.auto
             (  Opt.long "pool-retirement-epoch-boundary"
             <> Opt.metavar "INT"
             <> Opt.help "Epoch bound on pool retirement."
             )



pNumberOfPools :: Parser (Maybe Natural)
pNumberOfPools =
  optional
    $ Opt.option Opt.auto
        (  Opt.long "number-of-pools"
        <> Opt.metavar "NATURAL"
        <> Opt.help "Desired number of pools."
        )

pPoolInfluence :: Parser (Maybe Rational)
pPoolInfluence =
  optional
    $ Opt.option Opt.auto
        (  Opt.long "pool-influence"
        <> Opt.metavar "DOUBLE"
        <> Opt.help "Pool influence."
        )

pTreasuryExpansion :: Parser (Maybe UnitInterval)
pTreasuryExpansion =
  optional
    $ Opt.option pFieldUnitInterval
        (  Opt.long "treasury-expansion"
        <> Opt.metavar "DOUBLE"
        <> Opt.help "Treasury expansion."
        )


pMonetaryExpansion :: Parser (Maybe UnitInterval)
pMonetaryExpansion =
  optional
    $ Opt.option pFieldUnitInterval
       (  Opt.long "monetary-expansion"
       <> Opt.metavar "DOUBLE"
       <> Opt.help "Monetary expansion."
       )


pDecentralParam :: Parser (Maybe UnitInterval)
pDecentralParam =
  optional
    $ Opt.option pFieldUnitInterval
        (  Opt.long "decentralization-parameter"
        <> Opt.metavar "DOUBLE"
        <> Opt.help "Decentralization parameter."
        )



pExtraEntropy :: Parser (Maybe Nonce)
pExtraEntropy =
   optional
     $ Opt.option
         (Opt.maybeReader nonceHash)
           (  Opt.long "extra-entropy"
           <> Opt.metavar "HASH"
           <> Opt.help "Extra entropy."
           <> Opt.value Shelley.NeutralNonce
           )
  where
    nonceHash :: String -> Maybe Nonce
    nonceHash str = Nonce <$> (hashFromBytes $ C8.pack str)

pProtocolVersion :: Parser (Maybe ProtVer)
pProtocolVersion =
  optional $
    ProtVer
      <$> Opt.option Opt.auto
            (  Opt.long "protocol-major-version"
            <> Opt.metavar "NATURAL"
            <> Opt.help "Major protocol version. An increase indicates a hard fork."
            )
      <*> Opt.option Opt.auto
            (  Opt.long "protocol-minor-version"
            <> Opt.metavar "NATURAL"
            <> Opt.help "Minor protocol version. An increase indicates a soft fork\
                        \ (old software canvalidate but not produce new blocks)."
            )

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

pFieldUnitInterval :: Opt.ReadM UnitInterval
pFieldUnitInterval = Opt.auto >>= checkUnitInterval
  where
   checkUnitInterval :: Double -> Opt.ReadM UnitInterval
   checkUnitInterval dbl =
     case mkUnitInterval $ toRational dbl of
       Just interval -> return interval
       Nothing -> fail "Please enter a value in the range [0,1]"
