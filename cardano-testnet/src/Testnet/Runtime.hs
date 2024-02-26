{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Runtime
  ( LeadershipSlot(..)
  , NodeLoggingFormat(..)
  , PaymentKeyInfo(..)
  , PaymentKeyPair(..)
  , StakingKeyPair(..)
  , TestnetRuntime(..)
  , NodeRuntime(..)
  , PoolNode(..)
  , PoolNodeKeys(..)
  , Delegator(..)
  , allNodes
  , poolSprockets
  , poolNodeStdout
  , readNodeLoggingFormat
  , startNode
  , ShelleyGenesis(..)
  , shelleyGenesis
  , getStartTime
  , fromNominalDiffTimeMicro
  , startLedgerNewEpochStateLogging
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api

import qualified Cardano.Chain.Genesis as G
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.Genesis
import           Cardano.Node.Configuration.POM
import qualified Cardano.Node.Protocol.Byron as Byron
import           Cardano.Node.Types

import           Prelude

import           Control.Exception.Safe
import           Control.Monad
import qualified Control.Monad.Class.MonadTimer.SI as MT
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as A
import qualified Data.List as List
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)
import qualified GHC.IO.Handle as IO
import           GHC.Stack
import qualified GHC.Stack as GHC
import qualified System.Directory as IO
import           System.Directory (doesDirectoryExist)
import           System.FilePath
import qualified System.IO as IO
import qualified System.Process as IO

import           Testnet.Filepath
import qualified Testnet.Ping as Ping
import           Testnet.Process.Run
import           Testnet.Property.Utils (runInBackground)
import           Testnet.Start.Types

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as H
import qualified Hedgehog.Extras.Test.Base as H

data TestnetRuntime = TestnetRuntime
  { configurationFile :: FilePath
  , shelleyGenesisFile :: FilePath
  , testnetMagic :: Int
  , poolNodes :: [PoolNode]
  , wallets :: [PaymentKeyInfo]
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

data PaymentKeyInfo = PaymentKeyInfo
  { paymentKeyInfoPair :: PaymentKeyPair
  , paymentKeyInfoAddr :: Text
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

poolSprockets :: TestnetRuntime -> [Sprocket]
poolSprockets = fmap (nodeSprocket . poolRuntime) . poolNodes

shelleyGenesis :: (H.MonadTest m, MonadIO m, HasCallStack) => TestnetRuntime -> m (ShelleyGenesis StandardCrypto)
shelleyGenesis TestnetRuntime{shelleyGenesisFile} = withFrozenCallStack $
  H.evalEither =<< H.evalIO (A.eitherDecodeFileStrict' shelleyGenesisFile)

getStartTime
  :: (H.MonadTest m, MonadIO m, HasCallStack)
  => FilePath -> TestnetRuntime -> m UTCTime
getStartTime tempRootPath TestnetRuntime{configurationFile} = withFrozenCallStack $ H.evalEither <=< H.evalIO . runExceptT $ do
  byronGenesisFile <-
    decodeNodeConfiguration configurationFile >>= \case
      NodeProtocolConfigurationCardano NodeByronProtocolConfiguration{npcByronGenesisFile} _ _ _ _ ->
        pure $ unGenesisFile npcByronGenesisFile
  let byronGenesisFilePath = tempRootPath </> byronGenesisFile
  G.gdStartTime . G.configGenesisData <$> decodeGenesisFile byronGenesisFilePath
  where
    decodeNodeConfiguration :: FilePath -> ExceptT String IO NodeProtocolConfiguration
    decodeNodeConfiguration file = do
      partialNodeCfg <- ExceptT $ A.eitherDecodeFileStrict' file
      fmap ncProtocolConfig . liftEither . makeNodeConfiguration $ defaultPartialNodeConfiguration <> partialNodeCfg
    decodeGenesisFile :: FilePath -> ExceptT String IO G.Config
    decodeGenesisFile fp = withExceptT (docToString . prettyError) $
      Byron.readGenesis (GenesisFile fp) Nothing RequiresNoMagic

readNodeLoggingFormat :: String -> Either String NodeLoggingFormat
readNodeLoggingFormat = \case
  "json" -> Right NodeLoggingFormatAsJson
  "text" -> Right NodeLoggingFormatAsText
  s -> Left $ "Unrecognised node logging format: " <> show s <> ".  Valid options: \"json\", \"text\""

allNodes :: TestnetRuntime -> [NodeRuntime]
allNodes tr = fmap poolRuntime (poolNodes tr)

data NodeStartFailure
  = ProcessRelatedFailure ProcessError
  | ExecutableRelatedFailure ExecutableError
  | FileRelatedFailure IOException
  | NodeExecutableError String
 -- | NodePortNotOpenError IOException
  | MaxSprocketLengthExceededError
  deriving Show

-- TODO: We probably want a check that this node has the necessary config files to run and
-- if it doesn't we fail hard.
-- | Start a node, creating file handles, sockets and temp-dirs.
startNode
  :: TmpAbsolutePath
  -- ^ The temporary absolute path
  -> String
  -- ^ The name of the node
  -> Int
  -- ^ Node port
  -> Int
  -- ^ Testnet magic
  -> [String]
  -- ^ The command --socket-path will be added automatically.
  -> ExceptT NodeStartFailure (ResourceT IO) NodeRuntime
startNode tp node port testnetMagic nodeCmd = GHC.withFrozenCallStack $ do
  let tempBaseAbsPath = makeTmpBaseAbsPath tp
      socketDir = makeSocketDir tp
      logDir = makeLogDir tp

  liftIO $ createDirectoryIfMissingNew_ $ logDir </> node
  void . liftIO $ createSubdirectoryIfMissingNew tempBaseAbsPath (socketDir </> node)

  let nodeStdoutFile = logDir </> node </> "stdout.log"
      nodeStderrFile = logDir </> node </> "stderr.log"
      socketRelPath = socketDir </> node </> "sock"
      sprocket = Sprocket tempBaseAbsPath socketRelPath

  hNodeStdout <- handleIOExceptT FileRelatedFailure $ IO.openFile nodeStdoutFile IO.WriteMode
  hNodeStderr <- handleIOExceptT FileRelatedFailure $ IO.openFile nodeStderrFile IO.ReadWriteMode

  unless (List.length (H.sprocketArgumentName sprocket) <= H.maxSprocketArgumentNameLength) $
     left MaxSprocketLengthExceededError

  let portString = show port
      socketAbsPath = H.sprocketSystemName sprocket

  nodeProcess
    <- firstExceptT ExecutableRelatedFailure
         $ hoistExceptT lift $ procNode $ mconcat
                       [ nodeCmd
                       , [ "--socket-path", H.sprocketArgumentName sprocket
                         , "--port", portString
                         ]
                       ]

  (Just stdIn, _, _, hProcess, _)
    <- firstExceptT ProcessRelatedFailure $ initiateProcess
          $ nodeProcess
             { IO.std_in = IO.CreatePipe, IO.std_out = IO.UseHandle hNodeStdout
             , IO.std_err = IO.UseHandle hNodeStderr
             , IO.cwd = Just tempBaseAbsPath
             }

  -- We force the evaluation of initiateProcess so we can be sure that
  -- the process has started. This allows us to read stderr in order
  -- to fail early on errors generated from the cardano-node binary.
  _ <- liftIO (IO.getPid hProcess)
    >>= hoistMaybe (NodeExecutableError $ mconcat ["startNode: ", node, "'s process did not start."])

  -- allow network to thermalize before proceeding
  -- FIXME: use ledger events to listen here instead of this workaround
  liftIO $ MT.threadDelay 10

  -- Wait for socket to be created
  eSprocketError <-
    Ping.waitForSprocket
      20  -- timeout
      0.2 -- check interval
      sprocket

  -- If we do have anything on stderr, fail.
  stdErrContents <- liftIO $ IO.readFile nodeStderrFile
  unless (null stdErrContents)
    $ left $ NodeExecutableError stdErrContents

  -- No stderr and no socket? Fail.
  firstExceptT
    (\ioex ->
      NodeExecutableError . mconcat $
        ["Socket ", socketAbsPath, " was not created after 20 seconds. There was no output on stderr. Exception: ", displayException ioex])
    $ hoistEither eSprocketError

  -- Ping node and fail on error
  Ping.pingNode (fromIntegral testnetMagic) sprocket
    >>= (firstExceptT (NodeExecutableError . docToString . ("Ping error:" <+>) . prettyError) . hoistEither)

  pure $ NodeRuntime node sprocket stdIn nodeStdoutFile nodeStderrFile hProcess


createDirectoryIfMissingNew :: HasCallStack => FilePath -> IO FilePath
createDirectoryIfMissingNew directory = GHC.withFrozenCallStack $ do
  IO.createDirectoryIfMissing True directory
  pure directory

createDirectoryIfMissingNew_ :: HasCallStack => FilePath -> IO ()
createDirectoryIfMissingNew_ directory = GHC.withFrozenCallStack $
  void $ createDirectoryIfMissingNew directory

createSubdirectoryIfMissingNew :: ()
  => HasCallStack
  => FilePath
  -> FilePath
  -> IO FilePath
createSubdirectoryIfMissingNew parent subdirectory = GHC.withFrozenCallStack $ do
  IO.createDirectoryIfMissing True $ parent </> subdirectory
  pure subdirectory

-- | Start ledger's new epoch state logging for the first node in the background.
-- Logs will be placed in <tmp workspace directory>/logs/ledger-new-epoch-state.log
-- The logging thread will be cancelled when `MonadResource` releases all resources.
startLedgerNewEpochStateLogging
  :: forall m. HasCallStack
  => MonadCatch m
  => MonadResource m
  => MonadTest m
  => TestnetRuntime
  -> FilePath -- ^ tmp workspace directory
  -> m ()
startLedgerNewEpochStateLogging testnetRuntime tmpWorkspace = withFrozenCallStack $ do
  let logDir = makeLogDir (TmpAbsolutePath tmpWorkspace)
      logFile = logDir </> "ledger-epoch-state.log"

  H.evalIO (doesDirectoryExist logDir) >>= \case
    True -> pure ()
    False -> do
      H.note_ $ "Log directory does not exist: " <> logDir <> " - cannot start logging epoch states"
      H.failure

  socketPath <- H.noteM $ H.sprocketSystemName <$> H.headM (poolSprockets testnetRuntime)
  _ <- runInBackground . runExceptT $
    foldEpochState
      (File $ configurationFile testnetRuntime)
      (Api.File socketPath)
      Api.QuickValidation
      (EpochNo maxBound)
      ()
      (handler logFile)
  H.note_ $ "Started logging epoch states to to: " <> logFile
  where
    handler :: FilePath -> AnyNewEpochState -> StateT () IO LedgerStateCondition
    handler outputFp anyNewEpochState = handleException . liftIO $ do
      appendFile outputFp $ "#### BLOCK ####" <> "\n"
      appendFile outputFp $ show anyNewEpochState <> "\n"
      pure ConditionNotMet
      where
        -- | Handle all sync exceptions and log them into the log file. We don't want to fail the test just
        -- because logging has failed.
        handleException = handle $ \(e :: SomeException) -> do
          liftIO $ appendFile outputFp $ "Ledger new epoch logging failed - caught exception:\n"
            <> displayException e <> "\n"
          pure ConditionMet


