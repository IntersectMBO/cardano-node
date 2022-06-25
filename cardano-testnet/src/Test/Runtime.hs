{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Runtime
  ( PaymentKeyPair(..)
  , StakingKeyPair(..)
  , TestnetRuntime(..)
  , TestnetNode(..)
  , PoolNode(..)
  , PoolNodeKeys(..)
  , Delegator(..)
  , bftSprockets
  , poolSprockets
  , poolNodeToTestnetNode
  ) where

import           Data.Eq (Eq)
import           Data.Function ((.))
import           Data.Functor (fmap)
import           Data.Int (Int)
import           Data.String (String)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           System.IO (FilePath)
import           Text.Show (Show (..))

import qualified System.IO as IO
import qualified System.Process as IO

data TestnetRuntime = TestnetRuntime
  { configurationFile :: FilePath
  , testnetMagic :: Int
  , bftNodes :: [TestnetNode]
  , poolNodes :: [PoolNode]
  , wallets :: [PaymentKeyPair]
  , delegators :: [Delegator]
  }

data TestnetNode = TestnetNode
  { nodeName :: String
  , nodeSprocket :: Sprocket
  , nodeStdinHandle :: IO.Handle
  , nodeStdout :: FilePath
  , nodeStderr :: FilePath
  , nodeProcessHandle :: IO.ProcessHandle
  }

data PoolNode = PoolNode
  { poolNodeName :: String
  , poolNodeSprocket :: Sprocket
  , poolNodeStdinHandle :: IO.Handle
  , poolNodeStdout :: FilePath
  , poolNodeStderr :: FilePath
  , poolNodeProcessHandle :: IO.ProcessHandle
  , poolNodeKeys :: PoolNodeKeys
  }

data PoolNodeKeys = PoolNodeKeys
  { poolNodeKeysColdVkey :: FilePath
  , poolNodeKeysColdSkey :: FilePath
  , poolNodeKeysVrfVkey :: FilePath
  , poolNodeKeysVrfSkey :: FilePath
  , poolNodeKeysStakingVkey :: FilePath
  , poolNodeKeysStakingSkey :: FilePath
  } deriving (Eq, Show)

data PaymentKeyPair = PaymentKeyPair
  { paymentVKey :: FilePath
  , paymentSKey :: FilePath
  } deriving (Eq, Show)

data StakingKeyPair = StakingKeyPair
  { stakingVKey :: FilePath
  , stakingSKey :: FilePath
  } deriving (Eq, Show)

data Delegator = Delegator
  { paymentKeyPair :: PaymentKeyPair
  , stakingKeyPair :: StakingKeyPair
  } deriving (Eq, Show)

poolNodeToTestnetNode :: PoolNode -> TestnetNode
poolNodeToTestnetNode PoolNode
  { poolNodeName
  , poolNodeSprocket
  , poolNodeStdinHandle
  , poolNodeStdout
  , poolNodeStderr
  , poolNodeProcessHandle
  } = TestnetNode
  { nodeName = poolNodeName
  , nodeSprocket = poolNodeSprocket
  , nodeStdinHandle = poolNodeStdinHandle
  , nodeStdout = poolNodeStdout
  , nodeStderr = poolNodeStderr
  , nodeProcessHandle = poolNodeProcessHandle
  }

bftSprockets :: TestnetRuntime -> [Sprocket]
bftSprockets = fmap nodeSprocket . bftNodes

poolSprockets :: TestnetRuntime -> [Sprocket]
poolSprockets = fmap poolNodeSprocket . poolNodes
