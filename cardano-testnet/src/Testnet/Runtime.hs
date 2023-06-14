{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Testnet.Runtime
  ( LeadershipSlot(..)
  , NodeLoggingFormat(..)
  , PaymentKeyPair(..)
  , StakingKeyPair(..)
  , TmpAbsolutePath(..)
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
  , ShelleyGenesis(..)
  , shelleyGenesis
  , getStartTime
  , fromNominalDiffTimeMicro
  ) where

import           Prelude

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.Aeson as A
import qualified Data.List as L
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)
import           GHC.Stack
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H
import           System.FilePath
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO

import           Cardano.Api
import qualified Cardano.Chain.Genesis as G
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.Genesis
import           Cardano.Node.Configuration.POM
import qualified Cardano.Node.Protocol.Byron as Byron
import           Cardano.Node.Types
import           Testnet.Filepath
import qualified Testnet.Process.Run as H

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

shelleyGenesis :: (H.MonadTest m, MonadIO m, HasCallStack) => TestnetRuntime -> m (ShelleyGenesis StandardCrypto)
shelleyGenesis TestnetRuntime{shelleyGenesisFile} = withFrozenCallStack $
  H.evalEither =<< H.evalIO (A.eitherDecodeFileStrict' shelleyGenesisFile)

getStartTime :: (H.MonadTest m, MonadIO m, HasCallStack) => FilePath -> TestnetRuntime -> m UTCTime
getStartTime tempRootPath TestnetRuntime{configurationFile} = withFrozenCallStack $ H.evalEither <=< H.evalIO . runExceptT $ do
  byronGenesisFile <-
    decodeNodeConfiguration configurationFile >>=
      \case
        NodeProtocolConfigurationCardano NodeByronProtocolConfiguration{npcByronGenesisFile} _ _ _ _ ->
          pure $ unGenesisFile npcByronGenesisFile
        NodeProtocolConfigurationByron NodeByronProtocolConfiguration{npcByronGenesisFile} ->
          pure $ unGenesisFile npcByronGenesisFile
        unsupported ->
          throwError $ unwords
            [ "cannot find byron configuration path in"
            , configurationFile
            , "- found instead:"
            , show unsupported
            ]
  let byronGenesisFilePath = tempRootPath </> byronGenesisFile
  G.gdStartTime . G.configGenesisData <$> decodeGenesisFile byronGenesisFilePath
  where
    decodeNodeConfiguration :: FilePath -> ExceptT String IO NodeProtocolConfiguration
    decodeNodeConfiguration file = do
      partialNodeCfg <- ExceptT $ A.eitherDecodeFileStrict' file
      fmap ncProtocolConfig . liftEither . makeNodeConfiguration $ defaultPartialNodeConfiguration <> partialNodeCfg
    decodeGenesisFile :: FilePath -> ExceptT String IO G.Config
    decodeGenesisFile fp = withExceptT displayError $
      Byron.readGenesis (GenesisFile fp) Nothing RequiresNoMagic

readNodeLoggingFormat :: String -> Either String NodeLoggingFormat
readNodeLoggingFormat = \case
  "json" -> Right NodeLoggingFormatAsJson
  "text" -> Right NodeLoggingFormatAsText
  s -> Left $ "Unrecognised node logging format: " <> show s <> ".  Valid options: \"json\", \"text\""

allNodes :: TestnetRuntime -> [NodeRuntime]
allNodes tr = bftNodes tr <> fmap poolRuntime (poolNodes tr)

-- | Start a node, creating file handles, sockets and temp-dirs.
startNode
  :: TmpAbsolutePath
  -- ^ The temporary absolute path
  -> String
  -- ^ The name of the node
  -> [String]
  -- ^ The command --socket-path and --port will be added automatically.
  -> H.Integration NodeRuntime
startNode tp@(TmpAbsolutePath tempAbsPath) node nodeCmd = do
  let tempBaseAbsPath = makeTmpBaseAbsPath tp
      socketDir = makeSocketDir tp
      logDir = makeLogDir tp

  H.createDirectoryIfMissing_ logDir
  H.createSubdirectoryIfMissing_ tempBaseAbsPath socketDir

  nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
  nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
  sprocket <- H.noteShow $ Sprocket tempBaseAbsPath (socketDir </> node)

  hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

  H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

  portString <- fmap S.strip . H.readFile $ tempAbsPath </> node </> "port"


  createProcessNode
    <- H.procNode $ mconcat
                    [ nodeCmd
                    , [ "--socket-path", IO.sprocketArgumentName sprocket
                      , "--port", portString
                      ]
                    ]


  (Just stdIn, _, _, hProcess, _)
    <- H.createProcess
         $ createProcessNode
            { IO.std_in = IO.CreatePipe, IO.std_out = IO.UseHandle hNodeStdout
            , IO.std_err = IO.UseHandle hNodeStderr
            , IO.cwd = Just tempBaseAbsPath
            }

  H.noteShowM_ $ H.getPid hProcess

  -- The node process can fail on startup, e.g while
  -- parsing the configuration yaml file. If we don't fail
  -- when stderr is populated, we end up having to wait for the
  -- timeout to expire before we can see the error.

  when (OS.os `L.elem` ["darwin", "linux"]) $ do
    H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

  return $ NodeRuntime node sprocket stdIn nodeStdoutFile nodeStderrFile hProcess
