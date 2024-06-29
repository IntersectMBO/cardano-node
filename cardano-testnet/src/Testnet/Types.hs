{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Testnet.Types
  ( LeadershipSlot(..)
  , NodeLoggingFormat(..)
  , PaymentKeyInfo(..)
  , TestnetRuntime(..)
  , NodeRuntime(..)
  , nodeSocketPath
  , PoolNode(..)
  , PoolNodeKeys(..)
  , Delegator(..)
  , KeyPair(..)
  , verificationKeyFp
  , signingKeyFp
  , SomeKeyPair(..)
  , VKey
  , SKey
  , ColdPoolKey
  , VrfKey
  , StakingKey
  , PaymentKey
  , DRepKey
  , SpoColdKey
  , allNodes
  , poolSprockets
  , poolNodeStdout
  , readNodeLoggingFormat
  , ShelleyGenesis(..)
  , shelleyGenesis
  , getStartTime
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley (VrfKey)

import qualified Cardano.Chain.Genesis as G
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.Genesis
import           Cardano.Node.Configuration.POM
import qualified Cardano.Node.Protocol.Byron as Byron
import           Cardano.Node.Types

import           Prelude

import           Control.Monad
import qualified Data.Aeson as A
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)
import qualified GHC.IO.Handle as IO
import           GHC.Stack
import           Network.Socket (HostAddress, PortNumber)
import           System.FilePath
import qualified System.Process as IO

import           Testnet.Start.Types

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))

data KeyPair k = KeyPair
  { verificationKey :: forall dir. (File (VKey k) dir)
  , signingKey :: forall dir. (File (SKey k) dir)
  }

deriving instance Show (KeyPair k)
deriving instance Eq (KeyPair k)

verificationKeyFp :: KeyPair k -> FilePath
verificationKeyFp = unFile . verificationKey

signingKeyFp :: KeyPair k -> FilePath
signingKeyFp = unFile . signingKey

data SomeKeyPair = forall a. SomeKeyPair (KeyPair a)
deriving instance Show SomeKeyPair

-- | Verification key tag
data VKey k

-- | Signing key tag
data SKey k

data TestnetRuntime = TestnetRuntime
  { configurationFile :: !(NodeConfigFile In)
  , shelleyGenesisFile :: !FilePath
  , testnetMagic :: !Int
  , poolNodes :: ![PoolNode]
  , wallets :: ![PaymentKeyInfo]
  , delegators :: ![Delegator]
  }

poolSprockets :: TestnetRuntime -> [Sprocket]
poolSprockets = fmap (nodeSprocket . poolRuntime) . poolNodes

data PoolNode = PoolNode
  { poolRuntime :: NodeRuntime
  , poolKeys :: PoolNodeKeys
  }

poolNodeStdout :: PoolNode -> FilePath
poolNodeStdout = nodeStdout . poolRuntime

data NodeRuntime = NodeRuntime
  { nodeName :: !String
  , nodeIpv4 :: !HostAddress
  , nodePort :: !PortNumber
  , nodeSprocket :: !Sprocket
  , nodeStdinHandle :: !IO.Handle
  , nodeStdout :: !FilePath
  , nodeStderr :: !FilePath
  , nodeProcessHandle :: !IO.ProcessHandle
  }

nodeSocketPath :: NodeRuntime -> SocketPath
nodeSocketPath = File . H.sprocketSystemName . nodeSprocket

data ColdPoolKey
data StakingKey
data SpoColdKey

data PoolNodeKeys = PoolNodeKeys
  { poolNodeKeysCold :: KeyPair SpoColdKey
  , poolNodeKeysVrf :: KeyPair VrfKey
  , poolNodeKeysStaking :: KeyPair StakingKey
  } deriving (Eq, Show)

data PaymentKeyInfo = PaymentKeyInfo
  { paymentKeyInfoPair :: KeyPair PaymentKey
  , paymentKeyInfoAddr :: Text
  } deriving (Eq, Show)

data Delegator = Delegator
  { paymentKeyPair :: KeyPair PaymentKey
  , stakingKeyPair :: KeyPair StakingKey
  } deriving (Eq, Show)

data LeadershipSlot = LeadershipSlot
  { slotNumber  :: Int
  , slotTime    :: Text
  } deriving (Eq, Show, Generic, FromJSON)

shelleyGenesis
  :: H.MonadTest m
  => MonadIO m
  => HasCallStack
  => TestnetRuntime -> m (ShelleyGenesis StandardCrypto)
shelleyGenesis TestnetRuntime{shelleyGenesisFile} = withFrozenCallStack $
  H.evalEither =<< H.evalIO (A.eitherDecodeFileStrict' shelleyGenesisFile)

getStartTime
  :: H.MonadTest m
  => MonadIO m
  => HasCallStack
  => FilePath
  -> TestnetRuntime
  -> m UTCTime
getStartTime tempRootPath TestnetRuntime{configurationFile} = withFrozenCallStack $ H.evalEither <=< H.evalIO . runExceptT $ do
  byronGenesisFile <-
    decodeNodeConfiguration configurationFile >>= \case
      NodeProtocolConfigurationCardano NodeByronProtocolConfiguration{npcByronGenesisFile} _ _ _ _ ->
        pure $ unGenesisFile npcByronGenesisFile
  let byronGenesisFilePath = tempRootPath </> byronGenesisFile
  G.gdStartTime . G.configGenesisData <$> decodeGenesisFile byronGenesisFilePath
  where
    decodeNodeConfiguration :: File NodeConfig In -> ExceptT String IO NodeProtocolConfiguration
    decodeNodeConfiguration (File file) = do
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
