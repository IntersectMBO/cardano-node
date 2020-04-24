module Cardano.CLI.Shelley.Parsers
  ( -- * CLI command parser
    parseShelleyCommands

    -- * CLI command types
  , ShelleyCommand (..)
  , ShelleyAddressCmd (..)
  , ShelleyStakeAddressCmd (..)
  , ShelleyTransactionCmd (..)
  , ShelleyNodeCmd (..)
  , ShelleyPoolCmd (..)
  , ShelleyQueryCmd (..)
  , ShelleyBlockCmd (..)
  , ShelleySystemCmd (..)
  , ShelleyDevOpsCmd (..)
  , ShelleyGenesisCmd (..)

    -- * CLI flag types
  , GenesisDir (..)
  , OutputFile (..)
  ) where

import           Cardano.Prelude hiding (option)

import           Cardano.Chain.Common (Lovelace, mkLovelace)
import           Cardano.CLI.Key (VerificationKeyFile(..))
import           Cardano.Common.Parsers (parseNodeAddress)
import           Cardano.Config.Types (NodeAddress, SigningKeyFile(..))
import           Cardano.Slotting.Slot (EpochNo (..))

import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

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
  = ShelleyAddress      ShelleyAddressCmd
  | ShelleyStakeAddress ShelleyStakeAddressCmd
  | ShelleyTransaction  ShelleyTransactionCmd
  | ShelleyNode         ShelleyNodeCmd
  | ShelleyPool         ShelleyPoolCmd
  | ShelleyQuery        ShelleyQueryCmd
  | ShelleyBlock        ShelleyBlockCmd
  | ShelleySystem       ShelleySystemCmd
  | ShelleyDevOps       ShelleyDevOpsCmd
  | ShelleyGenesis      ShelleyGenesisCmd
  deriving (Eq, Show)


data ShelleyAddressCmd
  = AddressKeyGen OutputFile OutputFile
  | AddressKeyHash VerificationKeyFile
  | AddressBuild          --TODO
  | AddressBuildMultiSig  --TODO
  deriving (Eq, Show)


data ShelleyStakeAddressCmd
  = StakeKeyRegister PrivKeyFile NodeAddress
  | StakeKeyDelegate PrivKeyFile PoolId Lovelace NodeAddress
  | StakeKeyDeRegister PrivKeyFile NodeAddress
  deriving (Eq, Show)


data ShelleyTransactionCmd
  = TxBuild         -- { input :: [Utxo], output :: [(Address,Lovelace)], nodeAddr :: NodeAddress }
  | TxSign          -- { transaction :: Transaction, keys :: [PrivKeyFile], utxo :: [Utxo], nodeAddr :: NodeAddress }
  | TxWitness       -- { transaction :: Transaction, key :: PrivKeyFile, nodeAddr :: NodeAddress }
  | TxSignWitness   -- { transaction :: Transaction, witnesses :: [Witness], nodeAddr :: NodeAddress }
  | TxCheck         -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  | TxSubmit        -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  | TxInfo          -- { transaction :: Transaction, nodeAddr :: NodeAddress }
  deriving (Eq, Show)


data ShelleyNodeCmd
  = NodeKeyGenCold VerificationKeyFile SigningKeyFile
  | NodeKeyGenKES  VerificationKeyFile SigningKeyFile Natural
  | NodeKeyGenVRF  VerificationKeyFile SigningKeyFile
  | NodeIssueOpCert --TODO
  deriving (Eq, Show)


data ShelleyPoolCmd
  = PoolRegister PoolId   -- { operator :: PubKey, owner :: [PubKey], kes :: PubKey, vrf :: PubKey, rewards :: PubKey, cost :: Lovelace, margin :: Margin, nodeAddr :: NodeAddress }
  | PoolReRegister PoolId -- { operator :: PubKey, owner :: [PubKey], kes :: PubKey, vrf :: PubKey, rewards :: PubKey, cost :: Lovelace, margin :: Margin, nodeAddr :: NodeAddress }
  | PoolRetire PoolId EpochNo NodeAddress
  deriving (Eq, Show)


data ShelleyQueryCmd
  = QueryPoolId NodeAddress
  | QueryTip NodeAddress
  | QueryVersion NodeAddress
  | QueryStatus NodeAddress
  deriving (Eq, Show)


data ShelleyBlockCmd
  = BlockInfo BlockId NodeAddress
  deriving (Eq, Show)


data ShelleyDevOpsCmd
  = DevOpsProtocolUpdate PrivKeyFile -- { parameters :: ProtocolParams, nodeAddr :: NodeAddress }
  | DevOpsColdKeys GenesisKeyFile     -- { genesis :: GenesisKeyFile, keys :: [PubKey], nodeAddr :: NodeAddress }
  deriving (Eq, Show)


data ShelleySystemCmd
  = SysStart GenesisFile NodeAddress
  | SysStop NodeAddress
  deriving (Eq, Show)


data ShelleyGenesisCmd
  = GenesisCreate GenesisDir (Maybe SystemStart) Lovelace
  | GenesisKeyGen OutputFile OutputFile
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
          (Opt.info (ShelleyAddress <$> pAddress) $ Opt.progDesc "Shelley address commands")
      , Opt.command "stake-address"
          (Opt.info (ShelleyStakeAddress <$> pStakeAddress) $ Opt.progDesc "Shelley stake address commands")
      , Opt.command "transaction"
          (Opt.info (ShelleyTransaction <$> pTransaction) $ Opt.progDesc "Shelley transaction commands")
      , Opt.command "node"
          (Opt.info (ShelleyNode <$> pShelleyNodeCmd) $ Opt.progDesc "Shelley node operaton commands")
      , Opt.command "stake-pool"
          (Opt.info (ShelleyPool <$> pShelleyPoolCmd) $ Opt.progDesc "Shelley stake pool commands")
      , Opt.command "query"
          (Opt.info (ShelleyQuery <$> pShelleyQueryCmd) $ Opt.progDesc "Shelley node query commands")
      , Opt.command "block"
          (Opt.info (ShelleyBlock <$> pShelleyBlockCmd) $ Opt.progDesc "Shelley block commands")
      , Opt.command "system"
          (Opt.info (ShelleySystem <$> pShelleySystemCmd) $ Opt.progDesc "Shelley system commands")
      , Opt.command "devops"
          (Opt.info (ShelleyDevOps <$> pShelleyDevOpsCmd) $ Opt.progDesc "Shelley devops commands")
      , Opt.command "genesis"
          (Opt.info (ShelleyGenesis <$> pShelleyGenesisCmd) $ Opt.progDesc "Shelley genesis block commands")
      ]


pAddress :: Parser ShelleyAddressCmd
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
    pAddressKeyGen :: Parser ShelleyAddressCmd
    pAddressKeyGen = AddressKeyGen <$> pOutputFile <*> pOutputFile

    pAddressKeyHash :: Parser ShelleyAddressCmd
    pAddressKeyHash = AddressKeyHash <$> pVerificationKeyFile

    pAddressBuild :: Parser ShelleyAddressCmd
    pAddressBuild = pure AddressBuild

    pAddressBuildMultiSig :: Parser ShelleyAddressCmd
    pAddressBuildMultiSig = pure AddressBuildMultiSig


pStakeAddress :: Parser ShelleyStakeAddressCmd
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
    pStakeAddressRegister :: Parser ShelleyStakeAddressCmd
    pStakeAddressRegister = StakeKeyRegister <$> pPrivKeyFile <*> parseNodeAddress

    pStakeAddressDelegate :: Parser ShelleyStakeAddressCmd
    pStakeAddressDelegate =
      StakeKeyDelegate <$> pPrivKeyFile <*> pPoolId <*> pDelegationFee <*> parseNodeAddress

    pStakeAddressDeRegister :: Parser ShelleyStakeAddressCmd
    pStakeAddressDeRegister = StakeKeyDeRegister <$> pPrivKeyFile <*> parseNodeAddress

    pDelegationFee :: Parser Lovelace
    pDelegationFee =
      either (Prelude.error . Prelude.show) identity . mkLovelace <$>
        Opt.option Opt.auto
          (  Opt.long "delegation-fee"
          <> Opt.metavar "LOVELACE"
          <> Opt.help "The delegation fee in Lovelace."
          )



pTransaction :: Parser ShelleyTransactionCmd
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
    pTransactionBuild :: Parser ShelleyTransactionCmd
    pTransactionBuild = pure TxBuild

    pTransactionSign  :: Parser ShelleyTransactionCmd
    pTransactionSign = pure TxSign

    pTransactionWitness :: Parser ShelleyTransactionCmd
    pTransactionWitness = pure TxWitness

    pTransactionSignWit :: Parser ShelleyTransactionCmd
    pTransactionSignWit = pure TxSignWitness

    pTransactionCheck  :: Parser ShelleyTransactionCmd
    pTransactionCheck = pure TxCheck

    pTransactionSubmit  :: Parser ShelleyTransactionCmd
    pTransactionSubmit = pure TxSubmit

    pTransactionInfo  :: Parser ShelleyTransactionCmd
    pTransactionInfo = pure TxInfo


pShelleyNodeCmd :: Parser ShelleyNodeCmd
pShelleyNodeCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "key-gen"
          (Opt.info pKeyGenOperator $
             Opt.progDesc "Create a key pair for node operator's offline key")
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
    pKeyGenOperator :: Parser ShelleyNodeCmd
    pKeyGenOperator =
      NodeKeyGenCold <$> pVerificationKeyFile <*> pSigningKeyFile

    pKeyGenKES :: Parser ShelleyNodeCmd
    pKeyGenKES =
      NodeKeyGenKES <$> pVerificationKeyFile <*> pSigningKeyFile <*> pDuration

    pKeyGenVRF :: Parser ShelleyNodeCmd
    pKeyGenVRF =
      NodeKeyGenVRF <$> pVerificationKeyFile <*> pSigningKeyFile

    pIssueOpCert :: Parser ShelleyNodeCmd
    pIssueOpCert =
      pure NodeIssueOpCert


pShelleyPoolCmd :: Parser ShelleyPoolCmd
pShelleyPoolCmd =
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
    pPoolRegster :: Parser ShelleyPoolCmd
    pPoolRegster = PoolRegister <$> pPoolId

    pPoolReRegster :: Parser ShelleyPoolCmd
    pPoolReRegster = PoolReRegister <$> pPoolId

    pPoolRetire :: Parser ShelleyPoolCmd
    pPoolRetire = PoolRetire <$> pPoolId <*> pEpochNo <*> parseNodeAddress


pShelleyQueryCmd :: Parser ShelleyQueryCmd
pShelleyQueryCmd =
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
    pQueryPoolId :: Parser ShelleyQueryCmd
    pQueryPoolId = QueryPoolId <$> parseNodeAddress

    pQueryTip :: Parser ShelleyQueryCmd
    pQueryTip = QueryTip <$> parseNodeAddress

    pQueryVersion :: Parser ShelleyQueryCmd
    pQueryVersion = QueryVersion <$> parseNodeAddress

    pQueryStatus :: Parser ShelleyQueryCmd
    pQueryStatus = QueryStatus <$> parseNodeAddress


pShelleyBlockCmd :: Parser ShelleyBlockCmd
pShelleyBlockCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "info"
          (Opt.info pBlockInfo $ Opt.progDesc "Get the node's pool id")
      ]
  where
    pBlockInfo :: Parser ShelleyBlockCmd
    pBlockInfo = BlockInfo <$> pBlockId <*> parseNodeAddress

pShelleyDevOpsCmd :: Parser ShelleyDevOpsCmd
pShelleyDevOpsCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "protocol-update"
          (Opt.info pProtocolUpdate $ Opt.progDesc "Protocol update")
      , Opt.command "cold-keys"
          (Opt.info pColdKeys $ Opt.progDesc "Cold keys")
      ]
  where
    pProtocolUpdate :: Parser ShelleyDevOpsCmd
    pProtocolUpdate = DevOpsProtocolUpdate <$> pPrivKeyFile

    pColdKeys :: Parser ShelleyDevOpsCmd
    pColdKeys = DevOpsColdKeys <$> pGenesisKeyFile

    pGenesisKeyFile :: Parser GenesisKeyFile
    pGenesisKeyFile =
      GenesisKeyFile <$>
        Opt.strOption
          (  Opt.long "genesis-key"
          <> Opt.metavar "FILE"
          <> Opt.help "The genesis key file."
          )


pShelleySystemCmd :: Parser ShelleySystemCmd
pShelleySystemCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "start"
          (Opt.info pSystemStart $ Opt.progDesc "Start system")
      , Opt.command "stop"
          (Opt.info pSystemStop $ Opt.progDesc "Stop system")
      ]
  where
    pSystemStart :: Parser ShelleySystemCmd
    pSystemStart = SysStart <$> pGenesisFile <*> parseNodeAddress

    pSystemStop :: Parser ShelleySystemCmd
    pSystemStop = SysStop <$> parseNodeAddress


pShelleyGenesisCmd :: Parser ShelleyGenesisCmd
pShelleyGenesisCmd =
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
      , Opt.command "create-genesis"
          (Opt.info pGenesisCommand $
             Opt.progDesc ("Create a Shelley genesis file from a genesis "
                        ++ "template and genesis/delegation/spending keys."))
      ]
  where
    pGenesisKeyGen :: Parser ShelleyGenesisCmd
    pGenesisKeyGen =
      GenesisKeyGen <$> pOutputFile <*> pOutputFile

    pGenesisDelegateKeyGen :: Parser ShelleyGenesisCmd
    pGenesisDelegateKeyGen =
      GenesisKeyGen <$> pOutputFile <*> pOutputFile

    pGenesisUTxOKeyGen :: Parser ShelleyGenesisCmd
    pGenesisUTxOKeyGen =
      GenesisKeyGen <$> pOutputFile <*> pOutputFile

    pGenesisCommand :: Parser ShelleyGenesisCmd
    pGenesisCommand =
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
          Opt.option Opt.auto
            (  Opt.long "start-time"
            <> Opt.metavar "EPOCH_SECS"
            <> Opt.help "The genesis start time in POSIX seconds. If unspecified, will be the current time +30 seconds."
            )

    convertTime :: Integer -> UTCTime
    convertTime = posixSecondsToUTCTime . realToFrac

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
