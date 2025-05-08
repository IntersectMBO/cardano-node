{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Testnet.Types
  ( LeadershipSlot(..)
  , NodeLoggingFormat(..)
  , PaymentKeyInfo(..)
  , TestnetRuntime(..)
  , spoNodes
  , relayNodes
  , testnetSprockets
  , TestnetNode(..)
  , nodeSocketPath
  , node0ConnectionInfo
  , isTestnetNodeSpo
  , SpoNodeKeys(..)
  , Delegator(..)
  , KeyPair(..)
  , verificationKeyFp
  , signingKeyFp
  , VKey
  , SKey
  , VrfKey
  , StakePoolKey
  , StakeKey
  , PaymentKey
  , KesKey
  , DRepKey
  , readNodeLoggingFormat
  , ShelleyGenesis(..)
  , shelleyGenesis
  , getStartTime
  , testnetDefaultIpv4Address
  , showIpv4Address
  ) where

import           Cardano.Api
import           Cardano.Api.Experimental (Some (..))
import           Cardano.Api.Shelley (KesKey, StakePoolKey, VrfKey)

import qualified Cardano.Chain.Genesis as G
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Cardano.Ledger.Shelley.Genesis
import           Cardano.Node.Configuration.POM
import qualified Cardano.Node.Protocol.Byron as Byron
import           Cardano.Node.Types

import           Prelude

import           Control.Monad
import qualified Data.Aeson as A
import           Data.List (intercalate)
import           Data.Maybe
import           Data.MonoTraversable (Element, MonoFunctor (..))
import           Data.Text (Text)
import           GHC.Exts (IsString (..))
import           GHC.Generics (Generic)
import qualified GHC.IO.Handle as IO
import           GHC.Stack
import           Network.Socket (HostAddress, PortNumber, hostAddressToTuple, tupleToHostAddress)
import           System.FilePath
import qualified System.Process as IO

import           Testnet.Start.Types

import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock as H
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))

data KeyPair k = KeyPair
  { verificationKey :: forall dir. (File (VKey k) dir)
  , signingKey :: forall dir. (File (SKey k) dir)
  }

type instance Element (KeyPair k) = FilePath
instance MonoFunctor (KeyPair k) where
  omap f (KeyPair vk sk) = KeyPair (f' vk) (f' sk)
    where
      f' :: File k' d -> File k' d
      f' = File . f . unFile

deriving instance Show (KeyPair k)
deriving instance Eq (KeyPair k)

instance Show (Some KeyPair) where
  show (Some kp) = show kp

instance Eq (Some KeyPair) where
  (Some KeyPair{verificationKey=File vk1, signingKey=File sk1})
    == (Some KeyPair{verificationKey=File vk2, signingKey=File sk2}) =
      vk1 == vk2 && sk1 == sk2

verificationKeyFp :: KeyPair k -> FilePath
verificationKeyFp = unFile . verificationKey

signingKeyFp :: KeyPair k -> FilePath
signingKeyFp = unFile . signingKey

-- | Verification key tag
data VKey k

-- | Signing key tag
data SKey k

data TestnetRuntime = TestnetRuntime
  { configurationFile :: !(NodeConfigFile In)
  , shelleyGenesisFile :: !FilePath
  , testnetMagic :: !Int -- TODO change to Word32
  , testnetNodes :: ![TestnetNode]
  , wallets :: ![PaymentKeyInfo]
  , delegators :: ![Delegator]
  }

testnetSprockets :: TestnetRuntime -> [Sprocket]
testnetSprockets = fmap nodeSprocket . testnetNodes

spoNodes :: TestnetRuntime -> [TestnetNode]
spoNodes = filter isTestnetNodeSpo . testnetNodes

relayNodes :: TestnetRuntime -> [TestnetNode]
relayNodes = filter (not . isTestnetNodeSpo) . testnetNodes

data TestnetNode = TestnetNode
  { nodeName :: !String
  , poolKeys :: Maybe SpoNodeKeys -- ^ Keys are only present for SPO nodes
  , nodeIpv4 :: !HostAddress
  , nodePort :: !PortNumber
  , nodeSprocket :: !Sprocket
  , nodeStdinHandle :: !IO.Handle
  , nodeStdout :: !FilePath
  , nodeStderr :: !FilePath
  , nodeProcessHandle :: !IO.ProcessHandle
  }

isTestnetNodeSpo :: TestnetNode -> Bool
isTestnetNodeSpo = isJust . poolKeys

nodeSocketPath :: TestnetNode -> SocketPath
nodeSocketPath = File . H.sprocketSystemName . nodeSprocket

-- | Connection data for the first node in the testnet
node0ConnectionInfo :: MonadTest m => TestnetRuntime -> m LocalNodeConnectInfo
node0ConnectionInfo TestnetRuntime{testnetMagic, testnetNodes} = do
  case testnetNodes of
    [] -> H.note_ "There are no nodes in the testnet" >> H.failure
    node0:_ -> do
        pure LocalNodeConnectInfo
              { localNodeSocketPath= nodeSocketPath node0
              , localNodeNetworkId=Testnet (NetworkMagic $ fromIntegral testnetMagic)
              , localConsensusModeParams=CardanoModeParams $ EpochSlots 21600}


data SpoNodeKeys = SpoNodeKeys
  { poolNodeKeysCold :: KeyPair StakePoolKey
  , poolNodeKeysVrf :: KeyPair VrfKey
  , poolNodeKeysStaking :: KeyPair StakeKey
  } deriving (Eq, Show)

type instance Element SpoNodeKeys = FilePath
instance MonoFunctor SpoNodeKeys where
  omap f (SpoNodeKeys cold vrf staking) = SpoNodeKeys (omap f cold) (omap f vrf) (omap f staking)

data PaymentKeyInfo = PaymentKeyInfo
  { paymentKeyInfoPair :: KeyPair PaymentKey
  , paymentKeyInfoAddr :: Text
  } deriving (Eq, Show)

data Delegator = Delegator
  { paymentKeyPair :: KeyPair PaymentKey
  , stakingKeyPair :: KeyPair StakeKey
  } deriving (Eq, Show)

data LeadershipSlot = LeadershipSlot
  { slotNumber  :: Int
  , slotTime    :: Text
  } deriving (Eq, Show, Generic, FromJSON)

shelleyGenesis
  :: H.MonadTest m
  => MonadIO m
  => HasCallStack
  => TestnetRuntime -> m ShelleyGenesis
shelleyGenesis TestnetRuntime{shelleyGenesisFile} = withFrozenCallStack $
  H.evalEither =<< H.evalIO (A.eitherDecodeFileStrict' shelleyGenesisFile)

getStartTime
  :: H.MonadTest m
  => MonadIO m
  => HasCallStack
  => FilePath
  -> TestnetRuntime
  -> m SystemStart
getStartTime tempRootPath TestnetRuntime{configurationFile} = withFrozenCallStack $ H.evalEither <=< H.evalIO . runExceptT $ do
  byronGenesisFile <-
    decodeNodeConfiguration configurationFile >>= \case
      NodeProtocolConfigurationCardano NodeByronProtocolConfiguration{npcByronGenesisFile} _ _ _ _ _ ->
        pure $ unGenesisFile npcByronGenesisFile
  let byronGenesisFilePath = tempRootPath </> byronGenesisFile
  SystemStart . G.gdStartTime . G.configGenesisData <$> decodeGenesisFile byronGenesisFilePath
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


-- | Hardcoded testnet IPv4 address pointing to the local host
testnetDefaultIpv4Address :: HostAddress
testnetDefaultIpv4Address = tupleToHostAddress (127, 0, 0, 1)

-- | Format IPv4 address as a string-like e.g. @127.0.0.1@
showIpv4Address :: IsString s => HostAddress -> s
showIpv4Address address = fromString . intercalate "." $ show <$> [a,b,c,d]
  where (a,b,c,d) = hostAddressToTuple address

