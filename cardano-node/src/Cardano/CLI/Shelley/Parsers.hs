module Cardano.CLI.Shelley.Parsers
  ( ShelleyCommand (..)
  , GenesisCommand (..)
  , GenesisDir (..)
  , OutputFile (..)
  , parseShelleyCommands
  ) where

import           Cardano.Prelude hiding (option)

import           Cardano.Chain.Common (Lovelace, mkLovelace)
import           Cardano.Common.Parsers (parseNodeAddress)
import           Cardano.Config.Types (NodeAddress)
import           Cardano.Slotting.Slot (EpochNo (..))

import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))

import           Prelude (String)
import qualified Prelude as Prelude

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

data ShelleyBlockCmd
  = BlockInfo BlockId NodeAddress
  deriving (Eq, Show)

data ShelleyDevOpsCmd
  = DevOpsProtocolUpdate PrivKeyFile -- { parameters :: ProtocolParams, nodeAddr :: NodeAddress }
  | DevOpsColdKeys GenesisKeyFile     -- { genesis :: GenesisKeyFile, keys :: [PubKey], nodeAddr :: NodeAddress }
  deriving (Eq, Show)


data ShelleyCommand
  = ShelleyCreateGenesis GenesisDir (Maybe SystemStart) Lovelace
  | ShelleyKeyGenerate OutputFile ByteString
  | ShelleyPool ShelleyPoolCmd
  | ShelleyStakeKey ShelleyStakeKeyCmd
  | ShelleyTransaction ShelleyTransactionCmd
  | ShelleyNode ShelleyNodeCmd
  | ShelleyBlock ShelleyBlockCmd
  | ShelleySystem ShelleySystemCmd
  | ShelleyDevOps ShelleyDevOpsCmd
  deriving (Eq, Show)

data GenesisCommand
  = GenesisCreateCmd GenesisFile
  deriving (Eq, Show)

data ShelleyNodeCmd
  = NodePoolId NodeAddress
  | NodeTip NodeAddress
  | NodeSlot NodeAddress
  | NodeVersion NodeAddress
  | NodeStatus NodeAddress
  deriving (Eq, Show)

data ShelleyPoolCmd
  = PoolRegister PoolId   -- { operator :: PubKey, owner :: [PubKey], kes :: PubKey, vrf :: PubKey, rewards :: PubKey, cost :: Lovelace, margin :: Margin, nodeAddr :: NodeAddress }
  | PoolReRegister PoolId -- { operator :: PubKey, owner :: [PubKey], kes :: PubKey, vrf :: PubKey, rewards :: PubKey, cost :: Lovelace, margin :: Margin, nodeAddr :: NodeAddress }
  | PoolRetire PoolId EpochNo NodeAddress
  deriving (Eq, Show)

data ShelleyStakeKeyCmd
  = StakeKeyRegister PrivKeyFile NodeAddress
  | StakeKeyDelegate PrivKeyFile PoolId Lovelace NodeAddress
  | StakeKeyDeRegister PrivKeyFile NodeAddress
  deriving (Eq, Show)

data ShelleySystemCmd
  = SysStart GenesisFile NodeAddress
  | SysStop NodeAddress
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

parseShelleyCommands :: Parser ShelleyCommand
parseShelleyCommands =
  Opt.subparser $
    mconcat
      [ Opt.command "create-genesis"
          (Opt.info pGenesisCommand
          $ Opt.progDesc "Create a Shelley genesis file from a genesis template and genesis/delegation/spending keys."
          )
      , Opt.command "key-gen"
          (Opt.info pKeyGen
          $ Opt.progDesc "Generate Shelley era crypto keys."
          )
      , Opt.command "pool"
          (Opt.info (ShelleyPool <$> pShelleyPoolCmd) $ Opt.progDesc "Shelley pool commands")
      , Opt.command "stake-key"
          (Opt.info (ShelleyStakeKey <$> pStakeKey) $ Opt.progDesc "Shelley stake key commands")
      , Opt.command "transaction"
          (Opt.info (ShelleyTransaction <$> pTransaction) $ Opt.progDesc "Shelley transaction commands")
      , Opt.command "node"
          (Opt.info (ShelleyNode <$> pShelleyNodeCmd) $ Opt.progDesc "Shelley node commands")
      , Opt.command "block"
          (Opt.info (ShelleyBlock <$> pShelleyBlockCmd) $ Opt.progDesc "Shelley block commands")
      , Opt.command "system"
          (Opt.info (ShelleySystem <$> pShelleySystemCmd) $ Opt.progDesc "Shelley system commands")
      , Opt.command "devops"
          (Opt.info (ShelleyDevOps <$> pShelleyDevOpsCmd) $ Opt.progDesc "Shelley devops commands")
      ]
  where
    pGenesisCommand :: Parser ShelleyCommand
    pGenesisCommand =
      ShelleyCreateGenesis <$> pGenesisDir <*> pMaybeSystemStart <*> pInitialSupply

    pKeyGen :: Parser ShelleyCommand
    pKeyGen =
      ShelleyKeyGenerate <$> pOutputFile <*> pComment

    -- The comment field is just passed around as a 'ByteString' to its better to leave it
    -- as such instead of converting it to 'Text'.
    pComment :: Parser ByteString
    pComment =
      BS.pack <$>
        Opt.strOption
          (  Opt.long "comment"
          <> Opt.metavar "TEXT"
          <> Opt.help "A single line of text comment."
          )

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

pStakeKey :: Parser ShelleyStakeKeyCmd
pStakeKey =
  Opt.subparser $
    mconcat
      [ Opt.command "register"
          (Opt.info pStakeKeyRegister $ Opt.progDesc "Register a stake pool")
      , Opt.command "delegate"
          (Opt.info pStakeKeyDelegate $ Opt.progDesc "Re-register a stake pool")
      , Opt.command "de-register"
          (Opt.info pStakeKeyDeRegister $ Opt.progDesc "Retire a stake pool")
      ]
  where
    pStakeKeyRegister :: Parser ShelleyStakeKeyCmd
    pStakeKeyRegister = StakeKeyRegister <$> pPrivKeyFile <*> parseNodeAddress

    pStakeKeyDelegate :: Parser ShelleyStakeKeyCmd
    pStakeKeyDelegate =
      StakeKeyDelegate <$> pPrivKeyFile <*> pPoolId <*> pDelegationFee <*> parseNodeAddress

    pStakeKeyDeRegister :: Parser ShelleyStakeKeyCmd
    pStakeKeyDeRegister = StakeKeyDeRegister <$> pPrivKeyFile <*> parseNodeAddress

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
      [ Opt.command "pool-id"
          (Opt.info pNodePoolId $ Opt.progDesc "Get the node's pool id")
      , Opt.command "tip"
          (Opt.info pNodeTip $ Opt.progDesc "Get the node tip")
      , Opt.command "slot"
          (Opt.info pNodeSlot $ Opt.progDesc "Get the node slot")
      , Opt.command "version"
          (Opt.info pNodeVersion $ Opt.progDesc "Get the node version")
      , Opt.command "status"
          (Opt.info pNodeStatus $ Opt.progDesc "Get the status of the node")
      ]
  where
    pNodePoolId :: Parser ShelleyNodeCmd
    pNodePoolId = NodePoolId <$> parseNodeAddress

    pNodeTip :: Parser ShelleyNodeCmd
    pNodeTip = NodeTip <$> parseNodeAddress

    pNodeSlot :: Parser ShelleyNodeCmd
    pNodeSlot = NodeSlot <$> parseNodeAddress

    pNodeVersion :: Parser ShelleyNodeCmd
    pNodeVersion = NodeVersion <$> parseNodeAddress

    pNodeStatus :: Parser ShelleyNodeCmd
    pNodeStatus = NodeStatus <$> parseNodeAddress


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

pBlockId :: Parser BlockId
pBlockId =
  BlockId <$>
    Opt.strOption
      (  Opt.long "block-id"
      <> Opt.metavar "STRING"
      <> Opt.help "The block identifier."
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
