{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

module Testnet.Start.Byron
  ( createByronGenesis
  , createByronUpdateProposal
  , createByronUpdateProposalVote
  , testnet
  , TestnetOptions(..)
  , byronDefaultTestnetOptions
  ) where

import           Control.Monad (forM_, void, when)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor ((<&>))
import           Data.Time.Clock (UTCTime)
import           GHC.Stack
import           Hedgehog.Extras.Stock.Aeson (rewriteObject)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import           Hedgehog.Extras.Stock.Time (showUTCTimeSeconds)
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import           System.FilePath.Posix ((</>))

import           Cardano.Api hiding (Value)

import qualified Cardano.Node.Configuration.Topology as NonP2P
import qualified Cardano.Node.Configuration.TopologyP2P as P2P
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.File as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Stock.String as S
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Network as H
import qualified Hedgehog.Extras.Test.Process as H
import           Hedgehog.Internal.Property (MonadTest)
import qualified System.Info as OS
import qualified System.IO as IO
import qualified System.Process as IO
import qualified Testnet.Conf as H
import           Testnet.Defaults
import           Testnet.Filepath
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import           Testnet.Property.Assert
import           Testnet.Property.Utils


{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use head" -}

data TestnetOptions = TestnetOptions
  { numBftNodes :: Int
  , slotDuration :: Int
  , securityParam :: Int
  , nPoorAddresses :: Int
  , testnetMagic :: Int
  , totalBalance :: Int
  , enableP2P :: Bool
  } deriving (Eq, Show)

byronDefaultTestnetOptions :: TestnetOptions
byronDefaultTestnetOptions = TestnetOptions
  { numBftNodes = 3
  , slotDuration = 2000
  , securityParam = 10
  , testnetMagic = 42
  , nPoorAddresses = 128
  -- TODO: createByronGenesis should have a check that errors
  -- if totalBalance can be evenly split between numBftNodes
  -- with no remainder. Having a remainder results in rounding errors.
  , totalBalance = 8000000000000001
  , enableP2P = False
  }

-- TODO: We should not abuse the byron testnet options for genesis creation.
-- This will lead to confusion. We should create a separate type for genesis creation.
-- | Creates a default Byron genesis. This is required for any testnet, predominantly because
-- we inject our ADA supply into our testnet via the Byron genesis.
createByronGenesis
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int
  -> UTCTime
  -> TestnetOptions
  -> String
  -> String
  -> m ()
createByronGenesis testnetMagic' startTime testnetOptions pParamFp genOutputDir =
  withFrozenCallStack $ execCli_
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show testnetMagic'
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show (securityParam testnetOptions)
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show @Int (numBftNodes testnetOptions)
    , "--total-balance", show @Int (totalBalance testnetOptions)
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--avvm-balance-factor", "1"
    , "--protocol-parameters-file", pParamFp
    , "--genesis-output-dir", genOutputDir
    ]

createByronUpdateProposal
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int -> String -> String -> Int -> m ()
createByronUpdateProposal testnetMagic' signingKeyFp updateProposalFp ptclMajorVersion =
  withFrozenCallStack $ execCli_
    [ "byron", "governance", "create-update-proposal"
    , "--filepath", updateProposalFp
    , "--testnet-magic", show testnetMagic'
    , "--signing-key", signingKeyFp
    , "--protocol-version-major", show ptclMajorVersion
    , "--protocol-version-minor", "0"
    , "--protocol-version-alt", "0"
    , "--application-name", "cardano-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    ]

createByronUpdateProposalVote
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => Int -> String -> String -> String -> m ()
createByronUpdateProposalVote testnetMagic' updateProposalFp signingKey outputFp =
    withFrozenCallStack $ execCli_
      [ "byron", "governance", "create-proposal-vote"
      , "--proposal-filepath", updateProposalFp
      , "--testnet-magic", show testnetMagic'
      , "--signing-key", signingKey
      , "--vote-yes"
      , "--output-filepath", outputFp
      ]


rewriteParams :: TestnetOptions -> Value -> Value
rewriteParams testnetOptions = rewriteObject
  $ HM.insert "slotDuration" (J.toJSON @String (show @Int (slotDuration testnetOptions)))

mkTopologyConfig :: Int -> Int -> [Int]
                 -> Bool -- ^ if true use p2p topology configuration
                 -> ByteString
mkTopologyConfig i numBftNodes' allPorts False = J.encode topologyNonP2P
  where
    topologyNonP2P :: NonP2P.NetworkTopology
    topologyNonP2P =
      NonP2P.RealNodeTopology
        $ flip fmap ([0 .. numBftNodes' - 1] L.\\ [i])
        $ \j -> NonP2P.RemoteAddress "127.0.0.1"
                                    (fromIntegral $ allPorts L.!! j)
                                    1
mkTopologyConfig i numBftNodes' allPorts True = J.encode topologyP2P
  where
    rootConfig :: P2P.RootConfig
    rootConfig =
      P2P.RootConfig
        (flip fmap ([0 .. numBftNodes' - 1] L.\\ [i])
        $ \j -> RelayAccessAddress "127.0.0.1"
                            (fromIntegral $ allPorts L.!! j)
        )
        P2P.DoNotAdvertisePeer

    localRootPeerGroups :: P2P.LocalRootPeersGroups
    localRootPeerGroups =
      P2P.LocalRootPeersGroups
        [ P2P.LocalRootPeersGroup rootConfig
                                  (numBftNodes' - 1)
        ]

    topologyP2P :: P2P.NetworkTopology
    topologyP2P =
      P2P.RealNodeTopology
        localRootPeerGroups
        []
        (P2P.UseLedger DontUseLedger)


testnet :: TestnetOptions -> H.Conf -> H.Integration [String]
testnet testnetOptions conf = do
  void $ H.note OS.os
  let tNetMagic = testnetMagic testnetOptions
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime 15 currentTime -- 15 seconds into the future
  allPorts <- H.noteShowIO $ IO.allocateRandomPorts (numBftNodes testnetOptions)
  let tempAbsPath' = unTmpAbsPath $ H.tempAbsPath conf
      sockDir = makeSocketDir $ H.tempAbsPath conf
      tempBaseAbsPath' = makeTmpBaseAbsPath $ H.tempAbsPath conf
      logDir = makeLogDir $ H.tempAbsPath conf


  H.lbsWriteFile (tempAbsPath' </> "byron.genesis.spec.json")
    . J.encode $ defaultByronProtocolParamsJsonValue

  H.copyRewriteJsonFile
    (tempAbsPath' </> "byron.genesis.spec.json")
    (tempAbsPath' </> "protocol-params.json")
    (rewriteParams testnetOptions)

  -- Generate keys
  void $ H.execCli
    [ "byron"
    , "genesis"
    , "genesis"
    , "--genesis-output-dir", tempAbsPath' </> "genesis"
    , "--start-time", showUTCTimeSeconds startTime
    , "--protocol-parameters-file", tempAbsPath' </> "protocol-params.json"
    , "--k", show @Int (securityParam testnetOptions)
    , "--protocol-magic", show @Int tNetMagic
    , "--n-poor-addresses", show @Int (nPoorAddresses testnetOptions)
    , "--n-delegate-addresses", show @Int (numBftNodes testnetOptions)
    , "--total-balance", show @Int (totalBalance testnetOptions)
    , "--avvm-entry-count", "128"
    , "--avvm-entry-balance", "10000000000000"
    , "--delegate-share", "0.9"
    , "--secret-seed", "2718281828"
    ]

  H.writeFile (tempAbsPath' </> "genesis/GENHASH") . S.lastLine =<< H.execCli
    [ "print-genesis-hash"
    , "--genesis-json"
    , tempAbsPath' </> "genesis/genesis.json"
    ]

  let nodeIndexes = [0..numBftNodes testnetOptions - 1]
  let allNodes = fmap (\i -> "node-" <> show @Int i) nodeIndexes

  H.createDirectoryIfMissing_ logDir


  -- Launch cluster of three nodes in P2P Mode
  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    let dbDir = makeDbDir i (TmpAbsolutePath tempAbsPath')
    H.createDirectoryIfMissing_ dbDir
    nodeStdoutFile <- H.noteTempFile tempAbsPath' $ "cardano-node-" <> si <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile tempAbsPath' $ "cardano-node-" <> si <> ".stderr.log"
    sprocket <- H.noteShow $ makeSprocket (TmpAbsolutePath tempAbsPath') $ "node-" <> si
    portString <- H.note $ show @Int (allPorts L.!! i)
    topologyFile <- H.noteShow $ tempAbsPath' </> "topology-node-" <> si <> ".json"
    configFile <- H.noteShow $ tempAbsPath' </> "config-" <> si <> ".yaml"
    signingKeyFile <- H.noteShow $ tempAbsPath' </> "genesis/delegate-keys.00" <> si <> ".key"
    delegationCertificateFile <- H.noteShow $ tempAbsPath' </> "genesis/delegation-cert.00" <> si <> ".json"


    H.lbsWriteFile (tempAbsPath' </> "topology-node-" <> si <> ".json") $
      mkTopologyConfig i (numBftNodes testnetOptions) allPorts (enableP2P testnetOptions)

    byronGenesisHash <- getByronGenesisHash $ tempAbsPath' </> "genesis/genesis.json"

    let finalYamlConfig :: LBS.ByteString
        finalYamlConfig = J.encode . J.Object
                            $ mconcat [ byronGenesisHash
                                      , defaultYamlHardforkViaConfig $ AnyCardanoEra ByronEra
                                      ]

    H.evalIO $ LBS.writeFile (tempAbsPath' </> "config-" <> si <> ".yaml")  finalYamlConfig

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    void $ H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--database-path", dbDir
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--port", portString
        , "--topology", topologyFile
        , "--config", configFile
        , "--signing-key", signingKeyFile
        , "--delegation-certificate", delegationCertificateFile
        , "--shutdown-ipc", "0"
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath'
          }
        )
      )

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 90 now

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    sprocket <- H.noteShow $ Sprocket tempBaseAbsPath' (sockDir </> "node-" <> si)
    _spocketSystemNameFile <- H.noteShow $ IO.sprocketSystemName sprocket
    -- TODO: Better error message need to indicate a sprocket was not created
    H.byDeadlineM 10 deadline "Failed to connect to node socket" $ H.assertM $ H.doesSprocketExist sprocket

  forM_ nodeIndexes $ \i -> do
    si <- H.noteShow $ show @Int i
    nodeStdoutFile <- H.noteTempFile tempAbsPath' $ "cardano-node-" <> si <> ".stdout.log"
    assertByDeadlineIOCustom "stdout does not contain \"until genesis start time\"" deadline $ IO.fileContains "until genesis start time at" nodeStdoutFile

  H.copyFile (tempAbsPath' </> "config-1.yaml") (tempAbsPath' </> "configuration.yaml")

  return allNodes

