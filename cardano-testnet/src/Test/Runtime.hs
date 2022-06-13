module Test.Runtime
  ( TestnetRuntime(..)
  , TestnetNode(..)
  , Wallet(..)
  ) where

import           Data.Eq (Eq)
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
  , poolNodes :: [TestnetNode]
  , wallets :: [Wallet]
  }

data TestnetNode = TestnetNode
  { nodeName :: String
  , nodeSprocket :: Sprocket
  , nodeStdinHandle :: IO.Handle
  , nodeStdout :: FilePath
  , nodeStderr :: FilePath
  , nodeProcessHandle :: IO.ProcessHandle
  }

data Wallet = Wallet
  { paymentVKey :: FilePath
  , paymentSKey :: FilePath
  } deriving (Eq, Show)
