module Cardano.CLI.Shelley.Parsers
  ( -- * CLI command parser
    parseShelleyCommands

    -- * CLI command types
  , ShelleyCommand (..)
  , AddressCmd (..)
  , StakeAddressCmd (..)
  , TransactionCmd (..)
  , NodeCmd (..)
  , PoolCmd (..)
  , QueryCmd (..)
  , BlockCmd (..)
  , SystemCmd (..)
  , DevOpsCmd (..)
  , GenesisCmd (..)

    -- * CLI flag types
  , GenesisDir (..)
  , OutputFile (..)
  , SigningKeyFile (..)
  , VerificationKeyFile (..)
  , OpCertCounterFile (..)
  ) where

import           Cardano.Prelude hiding (option)

import           Cardano.Chain.Common (Lovelace, mkLovelace)
import           Cardano.Common.Parsers (parseNodeAddress)
import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Config.Types (NodeAddress, SigningKeyFile(..))
import           Cardano.Config.Shelley.OCert (KESPeriod(..))
import           Cardano.CLI.Key (VerificationKeyFile(..))

import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeOrError)

import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import           Prelude (String)
import qualified Prelude as Prelude


--
-- Shelley CLI command data types
--

-- | All the CLI subcommands under \"shelley\".
--
data ShelleyCommand
  = AddressCmd      AddressCmd
  | StakeAddressCmd StakeAddressCmd
  | TransactionCmd  TransactionCmd
  | NodeCmd         NodeCmd
  | PoolCmd         PoolCmd
  | QueryCmd        QueryCmd
  | BlockCmd        BlockCmd
  | SystemCmd       SystemCmd
  | DevOpsCmd       DevOpsCmd
  | GenesisCmd      GenesisCmd
  deriving (Eq, Show)


data AddressCmd
  = AddressKeyGen OutputFile OutputFile
  | AddressKeyHash VerificationKeyFile
  | AddressBuild          --TODO
  | AddressBuildMultiSig  --TODO
  deriving (Eq, Show)


data StakeAddressCmd
  = StakeKeyRegister PrivKeyFile NodeAddress
  | StakeKeyDelegate PrivKeyFile PoolId Lovelace NodeAddress
  | StakeKeyDeRegister PrivKeyFile NodeAddress
  deriving (Eq, Show)


data TransactionCmd
  = TxBuild         -- { input :: [Utxo], output :: [(Address,Lovelace)], nodeAddr :: NodeAddress }
  | TxSign          -- { transaction :: Transaction, keys :: [PrivKeyFile], utxo :: [Utxo], nodeAddr :: NodeAddress }
  | TxWitness       -- { transaction :: Transaction, key :: PrivKeyFile, nodeAddr :: NodeAddress }
  | TxSignWitness   -- { transaction :: Transaction, witnesses :: [Witness], nodeAddr :: NodeAddress }
  | TxCheck         -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  | TxSubmit        -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  | TxInfo          -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  deriving (Eq, Show)


data NodeCmd
  = NodeKeyGenCold VerificationKeyFile SigningKeyFile OpCertCounterFile
  | NodeKeyGenKES  VerificationKeyFile SigningKeyFile Natural
  | NodeKeyGenVRF  VerificationKeyFile SigningKeyFile
  | NodeIssueOpCert VerificationKeyFile SigningKeyFile OpCertCounterFile
                    KESPeriod OutputFile
  deriving (Eq, Show)


data PoolCmd
  = PoolRegister PoolId   -- { operator :: PubKey, owner :: [PubKey], kes :: PubKey, vrf :: PubKey, rewards :: PubKey, cost :: Lovelace, margin :: Margin, nodeAddr :: NodeAddress }
  | PoolReRegister PoolId -- { operator :: PubKey, owner :: [PubKey], kes :: PubKey, vrf :: PubKey, rewards :: PubKey, cost :: Lovelace, margin :: Margin, nodeAddr :: NodeAddress }
  | PoolRetire PoolId EpochNo NodeAddress
  deriving (Eq, Show)


data QueryCmd
  = QueryPoolId NodeAddress
  | QueryTip NodeAddress
  | QueryVersion NodeAddress
  | QueryStatus NodeAddress
  deriving (Eq, Show)


data BlockCmd
  = BlockInfo BlockId NodeAddress
  deriving (Eq, Show)


data DevOpsCmd
  = DevOpsProtocolUpdate PrivKeyFile -- { parameters :: ProtocolParams, nodeAddr :: NodeAddress }
  | DevOpsColdKeys GenesisKeyFile     -- { genesis :: GenesisKeyFile, keys :: [PubKey], nodeAddr :: NodeAddress }
  deriving (Eq, Show)


data SystemCmd
  = SysStart GenesisFile NodeAddress
  | SysStop NodeAddress
  deriving (Eq, Show)


data GenesisCmd
  = GenesisCreate GenesisDir (Maybe SystemStart) Lovelace
  | GenesisKeyGenGenesis  VerificationKeyFile SigningKeyFile
  | GenesisKeyGenDelegate VerificationKeyFile SigningKeyFile OpCertCounterFile
  | GenesisKeyGenUTxO     VerificationKeyFile SigningKeyFile
  | GenesisKeyHash        VerificationKeyFile
  | GenesisVerKey         VerificationKeyFile SigningKeyFile
  deriving (Eq, Show)

--
-- Shelley CLI flag/option data types
--

newtype BlockId
  = BlockId String -- Probably not a String
  deriving (Eq, Show)

newtype GenesisFile
  = GenesisFile FilePath
  deriving (Eq, Show)

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving (Eq, Show)

newtype OutputFile
  = OutputFile FilePath
  deriving (Eq, Show)

newtype PoolId
  = PoolId String -- Probably not a String
  deriving (Eq, Show)

newtype GenesisDir
  = GenesisDir FilePath
  deriving (Eq, Show)

newtype OpCertCounterFile
  = OpCertCounterFile FilePath
  deriving (Eq, Show)

newtype PrivKeyFile
  = PrivKeyFile FilePath
  deriving (Eq, Show)


--
-- Shelley CLI command parsers
--

parseShelleyCommands :: Parser ShelleyCommand
parseShelleyCommands =
  Opt.subparser $
    mconcat
      [ Opt.command "address"
          (Opt.info (AddressCmd <$> pAddress) $ Opt.progDesc "Shelley address commands")
      , Opt.command "stake-address"
          (Opt.info (StakeAddressCmd <$> pStakeAddress) $ Opt.progDesc "Shelley stake address commands")
      , Opt.command "transaction"
          (Opt.info (TransactionCmd <$> pTransaction) $ Opt.progDesc "Shelley transaction commands")
      , Opt.command "node"
          (Opt.info (NodeCmd <$> pNodeCmd) $ Opt.progDesc "Shelley node operaton commands")
      , Opt.command "stake-pool"
          (Opt.info (PoolCmd <$> pPoolCmd) $ Opt.progDesc "Shelley stake pool commands")
      , Opt.command "query"
          (Opt.info (QueryCmd <$> pQueryCmd) $ Opt.progDesc "Shelley node query commands")
      , Opt.command "block"
          (Opt.info (BlockCmd <$> pBlockCmd) $ Opt.progDesc "Shelley block commands")
      , Opt.command "system"
          (Opt.info (SystemCmd <$> pSystemCmd) $ Opt.progDesc "Shelley system commands")
      , Opt.command "devops"
          (Opt.info (DevOpsCmd <$> pDevOpsCmd) $ Opt.progDesc "Shelley devops commands")
      , Opt.command "genesis"
          (Opt.info (GenesisCmd <$> pGenesisCmd) $ Opt.progDesc "Shelley genesis block commands")
      ]

pAddress :: Parser AddressCmd
pAddress =
  Opt.subparser $
    mconcat
      [ Opt.command "key-gen"
          (Opt.info pAddressKeyGen $ Opt.progDesc "Create a single address key pair")
      , Opt.command "key-hash"
          (Opt.info pAddressKeyHash $ Opt.progDesc "Show the hash of an address key")
      , Opt.command "build"
          (Opt.info pAddressBuild $ Opt.progDesc "Build an address")
      , Opt.command "build-multisig"
          (Opt.info pAddressBuildMultiSig $ Opt.progDesc "Build a multi-sig address")
      ]
  where
    pAddressKeyGen :: Parser AddressCmd
    pAddressKeyGen = AddressKeyGen <$> pOutputFile <*> pOutputFile

    pAddressKeyHash :: Parser AddressCmd
    pAddressKeyHash = AddressKeyHash <$> pVerificationKeyFile

    pAddressBuild :: Parser AddressCmd
    pAddressBuild = pure AddressBuild

    pAddressBuildMultiSig :: Parser AddressCmd
    pAddressBuildMultiSig = pure AddressBuildMultiSig


pStakeAddress :: Parser StakeAddressCmd
pStakeAddress =
  Opt.subparser $
    mconcat
      [ Opt.command "register"
          (Opt.info pStakeAddressRegister $ Opt.progDesc "Register a stake address")
      , Opt.command "delegate"
          (Opt.info pStakeAddressDelegate $ Opt.progDesc "Delegate from a stake address to a stake pool")
      , Opt.command "de-register"
          (Opt.info pStakeAddressDeRegister $ Opt.progDesc "De-register a stake address")
      ]
  where
    pStakeAddressRegister :: Parser StakeAddressCmd
    pStakeAddressRegister = StakeKeyRegister <$> pPrivKeyFile <*> parseNodeAddress

    pStakeAddressDelegate :: Parser StakeAddressCmd
    pStakeAddressDelegate =
      StakeKeyDelegate <$> pPrivKeyFile <*> pPoolId <*> pDelegationFee <*> parseNodeAddress

    pStakeAddressDeRegister :: Parser StakeAddressCmd
    pStakeAddressDeRegister = StakeKeyDeRegister <$> pPrivKeyFile <*> parseNodeAddress

    pDelegationFee :: Parser Lovelace
    pDelegationFee =
      either (Prelude.error . Prelude.show) identity . mkLovelace <$>
        Opt.option Opt.auto
          (  Opt.long "delegation-fee"
          <> Opt.metavar "LOVELACE"
          <> Opt.help "The delegation fee in Lovelace."
          )



pTransaction :: Parser TransactionCmd
pTransaction =
  Opt.subparser $
    mconcat
      [ Opt.command "build"
          (Opt.info pTransactionBuild $ Opt.progDesc "Build a transaction")
      , Opt.command "sign"
          (Opt.info pTransactionSign $ Opt.progDesc "Sign a transaction")
      , Opt.command "witness"
          (Opt.info pTransactionWitness $ Opt.progDesc "Witness a transaction")
      , Opt.command "sign-witness"
          (Opt.info pTransactionSignWit $ Opt.progDesc "Sign and witness a transaction")
      , Opt.command "check"
          (Opt.info pTransactionCheck $ Opt.progDesc "Check a transaction")
      , Opt.command "submit"
          (Opt.info pTransactionSubmit $ Opt.progDesc "Submit a transaction")
      , Opt.command "info"
          (Opt.info pTransactionInfo $ Opt.progDesc "Print information about a transaction")
      ]
  where
    pTransactionBuild :: Parser TransactionCmd
    pTransactionBuild = pure TxBuild

    pTransactionSign  :: Parser TransactionCmd
    pTransactionSign = pure TxSign

    pTransactionWitness :: Parser TransactionCmd
    pTransactionWitness = pure TxWitness

    pTransactionSignWit :: Parser TransactionCmd
    pTransactionSignWit = pure TxSignWitness

    pTransactionCheck  :: Parser TransactionCmd
    pTransactionCheck = pure TxCheck

    pTransactionSubmit  :: Parser TransactionCmd
    pTransactionSubmit = pure TxSubmit

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
      NodeKeyGenCold <$> pVerificationKeyFile
                     <*> pSigningKeyFile
                     <*> pOperatorCertIssueCounterFile

    pKeyGenKES :: Parser NodeCmd
    pKeyGenKES =
      NodeKeyGenKES <$> pVerificationKeyFile <*> pSigningKeyFile <*> pDuration

    pKeyGenVRF :: Parser NodeCmd
    pKeyGenVRF =
      NodeKeyGenVRF <$> pVerificationKeyFile <*> pSigningKeyFile

    pIssueOpCert :: Parser NodeCmd
    pIssueOpCert =
      NodeIssueOpCert <$> pVerificationKeyFile
                      <*> pSigningKeyFile
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
      ]
  where
    pPoolRegster :: Parser PoolCmd
    pPoolRegster = PoolRegister <$> pPoolId

    pPoolReRegster :: Parser PoolCmd
    pPoolReRegster = PoolReRegister <$> pPoolId

    pPoolRetire :: Parser PoolCmd
    pPoolRetire = PoolRetire <$> pPoolId <*> pEpochNo <*> parseNodeAddress


pQueryCmd :: Parser QueryCmd
pQueryCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "pool-id"
          (Opt.info pQueryPoolId $ Opt.progDesc "Get the node's pool id")
      , Opt.command "tip"
          (Opt.info pQueryTip $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)")
      , Opt.command "version"
          (Opt.info pQueryVersion $ Opt.progDesc "Get the node version")
      , Opt.command "status"
          (Opt.info pQueryStatus $ Opt.progDesc "Get the status of the node")
      ]
  where
    pQueryPoolId :: Parser QueryCmd
    pQueryPoolId = QueryPoolId <$> parseNodeAddress

    pQueryTip :: Parser QueryCmd
    pQueryTip = QueryTip <$> parseNodeAddress

    pQueryVersion :: Parser QueryCmd
    pQueryVersion = QueryVersion <$> parseNodeAddress

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

pDevOpsCmd :: Parser DevOpsCmd
pDevOpsCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "protocol-update"
          (Opt.info pProtocolUpdate $ Opt.progDesc "Protocol update")
      , Opt.command "cold-keys"
          (Opt.info pColdKeys $ Opt.progDesc "Cold keys")
      ]
  where
    pProtocolUpdate :: Parser DevOpsCmd
    pProtocolUpdate = DevOpsProtocolUpdate <$> pPrivKeyFile

    pColdKeys :: Parser DevOpsCmd
    pColdKeys = DevOpsColdKeys <$> pGenesisKeyFile

    pGenesisKeyFile :: Parser GenesisKeyFile
    pGenesisKeyFile =
      GenesisKeyFile <$>
        Opt.strOption
          (  Opt.long "genesis-key"
          <> Opt.metavar "FILE"
          <> Opt.help "The genesis key file."
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
      , Opt.command "create-genesis"
          (Opt.info pGenesisCreate $
             Opt.progDesc ("Create a Shelley genesis file from a genesis "
                        ++ "template and genesis/delegation/spending keys."))
      ]
  where
    pGenesisKeyGen :: Parser GenesisCmd
    pGenesisKeyGen =
      GenesisKeyGenGenesis <$> pVerificationKeyFile <*> pSigningKeyFile

    pGenesisDelegateKeyGen :: Parser GenesisCmd
    pGenesisDelegateKeyGen =
      GenesisKeyGenDelegate <$> pVerificationKeyFile
                            <*> pSigningKeyFile
                            <*> pOperatorCertIssueCounterFile

    pGenesisUTxOKeyGen :: Parser GenesisCmd
    pGenesisUTxOKeyGen =
      GenesisKeyGenUTxO <$> pVerificationKeyFile <*> pSigningKeyFile

    pGenesisKeyHash :: Parser GenesisCmd
    pGenesisKeyHash =
      GenesisKeyHash <$> pVerificationKeyFile

    pGenesisVerKey :: Parser GenesisCmd
    pGenesisVerKey =
      GenesisVerKey <$> pVerificationKeyFile <*> pSigningKeyFile

    pGenesisCreate :: Parser GenesisCmd
    pGenesisCreate =
      GenesisCreate <$> pGenesisDir <*> pMaybeSystemStart <*> pInitialSupply

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
            <> Opt.metavar "UTC_TIME"
            <> Opt.help "The genesis start time in YYYY-MM-DDThh:mm:ssZ format. If unspecified, will be the current time +30 seconds."
            )

    convertTime :: String -> UTCTime
    convertTime =
      parseTimeOrError False defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")

    pInitialSupply :: Parser Lovelace
    pInitialSupply =
      either (Prelude.error . Prelude.show) identity . mkLovelace <$>
        Opt.option Opt.auto
          (  Opt.long "supply"
          <> Opt.metavar "LOVELACE"
          <> Opt.help "The initial coin supply in Lovelace which will be evenly distributed across initial stake holders."
          )


--
-- Shelley CLI flag parsers
--

pSigningKeyFile :: Parser SigningKeyFile
pSigningKeyFile =
  SigningKeyFile <$>
   Opt.strOption
     (  Opt.long "signing-key-file"
     <> Opt.metavar "FILEPATH"
     <> Opt.help "Output filepath of the signing key."
     )


pBlockId :: Parser BlockId
pBlockId =
  BlockId <$>
    Opt.strOption
      (  Opt.long "block-id"
      <> Opt.metavar "STRING"
      <> Opt.help "The block identifier."
      )

pDuration :: Parser Natural
pDuration =
  Opt.option Opt.auto (  Opt.long "kes-duration"
                      <> Opt.metavar "NATURAL"
                      <> Opt.help "The duration of the KESPeriod."
                      )

pKesPeriod :: Parser KESPeriod
pKesPeriod =
  KESPeriod <$>
  Opt.option Opt.auto (  Opt.long "kes-period"
                      <> Opt.metavar "NATURAL"
                      <> Opt.help "The start of the KES key validity period."
                      )

pEpochNo :: Parser EpochNo
pEpochNo =
  EpochNo <$>
    Opt.option Opt.auto
      (  Opt.long "epoch"
      <> Opt.metavar "INT"
      <> Opt.help "The epoch number."
      )

pGenesisFile :: Parser GenesisFile
pGenesisFile =
  GenesisFile <$>
    Opt.strOption
      (  Opt.long "genesis"
      <> Opt.metavar "FILE"
      <> Opt.help "The genesis file."
      )

pOperatorCertIssueCounterFile :: Parser OpCertCounterFile
pOperatorCertIssueCounterFile =
  OpCertCounterFile <$>
    Opt.strOption
      (  Opt.long "operational-certificate-issue-counter"
      <> Opt.metavar "FILE"
      <> Opt.help "The file with the issue counter for the operational certificate."
      )

pOutputFile :: Parser OutputFile
pOutputFile =
  OutputFile <$>
    Opt.strOption
      (  Opt.long "out-file"
      <> Opt.metavar "FILE"
      <> Opt.help "The output file."
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
    Opt.strOption
      (  Opt.long "private-key"
      <> Opt.metavar "FILE"
      <> Opt.help "The private key file."
      )

pVerificationKeyFile :: Parser VerificationKeyFile
pVerificationKeyFile =
  VerificationKeyFile <$>
    Opt.strOption
      (  Opt.long "verification-key-file"
      <> Opt.metavar "FILEPATH"
      <> Opt.help "Output filepath of the verification key."
      )
