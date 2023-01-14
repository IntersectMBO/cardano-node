{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Testnet.Util.Runtime
  ( LeadershipSlot(..)
  , NodeLoggingFormat(..)
  , PaymentKeyPair(..)
  , StakingKeyPair(..)
  , TestnetRuntime(..)
  , NodeRuntime(..)
  , PoolNode(..)
  , PoolNodeKeys(..)
  , Delegator(..)
  , allNodes
  , bftSprockets
  , poolSprockets
  , poolNodeStdout
  , readNodeLoggingFormat
  , startNode
  ) where

import           Prelude

import           Control.Monad
import           Data.Aeson (FromJSON)
import qualified Data.List as L

import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

import           System.FilePath.Posix ((</>))
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO

import qualified Testnet.Util.Process as H

data NodeLoggingFormat = NodeLoggingFormatAsJson | NodeLoggingFormatAsText deriving (Eq, Show)

data TestnetRuntime = TestnetRuntime
  { configurationFile :: FilePath
  , shelleyGenesisFile :: FilePath
  , testnetMagic :: Int
  , bftNodes :: [NodeRuntime]
  , poolNodes :: [PoolNode]
  , wallets :: [PaymentKeyPair]
  , delegators :: [Delegator]
  }

data NodeRuntime = NodeRuntime
  { nodeName :: String
  , nodeSprocket :: Sprocket
  , nodeStdinHandle :: IO.Handle
  , nodeStdout :: FilePath
  , nodeStderr :: FilePath
  , nodeProcessHandle :: IO.ProcessHandle
  }

data PoolNode = PoolNode
  { poolRuntime :: NodeRuntime
  , poolKeys :: PoolNodeKeys
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

data LeadershipSlot = LeadershipSlot
  { slotNumber  :: Int
  , slotTime    :: Text
  } deriving (Eq, Show, Generic, FromJSON)

poolNodeStdout :: PoolNode -> FilePath
poolNodeStdout = nodeStdout . poolRuntime

bftSprockets :: TestnetRuntime -> [Sprocket]
bftSprockets = fmap nodeSprocket . bftNodes

poolSprockets :: TestnetRuntime -> [Sprocket]
poolSprockets = fmap (nodeSprocket . poolRuntime) . poolNodes

readNodeLoggingFormat :: String -> Either String NodeLoggingFormat
readNodeLoggingFormat = \case
  "json" -> Right NodeLoggingFormatAsJson
  "text" -> Right NodeLoggingFormatAsText
  s -> Left $ "Unrecognised node logging format: " <> show s <> ".  Valid options: \"json\", \"text\""

allNodes :: TestnetRuntime -> [NodeRuntime]
allNodes tr = bftNodes tr <> fmap poolRuntime (poolNodes tr)

-- | Start a node, creating file handles, sockets and temp-dirs.
startNode
  :: String
  -- ^ The tempBaseAbsPath
  -> FilePath
  -- ^ The tempAbsPath
  -> FilePath
  -- ^ The log directory
  -> FilePath
  -- ^ The directory where the sockets are created
  -> String
  -- ^ The name of the node
  -> [String]
  -- ^ The command --socket-path and --port will be added automatically.
  -> H.Integration NodeRuntime
startNode tempBaseAbsPath tempAbsPath logDir socketDir node nodeCmd = do
  dbDir <- H.noteShow $ tempAbsPath </> "db/" <> node
  nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
  nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
  sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

  H.createDirectoryIfMissing dbDir
  H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

  hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

  H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

  portString <- fmap S.strip . H.readFile $ tempAbsPath </> node </> "port"

  (Just stdIn, _, _, hProcess, _) <- H.createProcess
    . (\ cp
         -> cp
              {IO.std_in = IO.CreatePipe, IO.std_out = IO.UseHandle hNodeStdout,
               IO.std_err = IO.UseHandle hNodeStderr,
               IO.cwd = Just tempBaseAbsPath})
    =<<
      H.procNode
        (nodeCmd
           <>
             [ "--socket-path", IO.sprocketArgumentName sprocket
             , "--port", portString
             ])

  H.noteShowM_ $ H.getPid hProcess

  when (OS.os `L.elem` ["darwin", "linux"]) $ do
    H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

  return $ NodeRuntime node sprocket stdIn nodeStdoutFile nodeStderrFile hProcess
