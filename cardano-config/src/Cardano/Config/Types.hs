{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Config.Types
    ( ConfigError(..)
    , CardanoConfiguration (..)
    , CardanoEnvironment (..)
    , Core (..)
    -- * specific for @Core@
    , RequireNetworkMagic (..)
    , NodeProtocol (..)
    , Spec (..)
    , Initializer (..)
    -- * rest
    , TestBalance (..)
    , FakeAvvmBalance (..)
    , BlockVersionData (..)
    , LastKnownBlockVersion (..)
    , SoftForkRule (..)
    , TxFeePolicy (..)
    , TxSizeLinear (..)
    , ProtocolConstants (..)
    , NTP (..)
    , Update (..)
    , TXP (..)
    , DLG (..)
    , Block (..)
    , Node (..)
    , TLS (..)
    , Wallet (..)
    , Certificate (..)
    ) where

import           Prelude (String, show)
import           Cardano.Prelude

import qualified Ouroboros.Consensus.BlockchainTime as Consensus

import           Cardano.Config.Orphanage ()
import           Cardano.Config.Topology

--------------------------------------------------------------------------------
-- Cardano Environment
--------------------------------------------------------------------------------

-- | Just a placeholder for now.
data CardanoEnvironment = NoEnvironment
    deriving (Eq, Show)

-- | Exception type for configuration-related errors.
data ConfigError
  = PartialConfigValue !String

instance Show ConfigError where
  show (PartialConfigValue name)
    = "Undefined CardanoConfiguration value: " <> name

instance Exception ConfigError

--------------------------------------------------------------------------------
-- Cardano Configuration Data Structures
--------------------------------------------------------------------------------
-- | The basic configuration structure. It should contain all the required
-- configuration parameters for the modules to work.

data CardanoConfiguration = CardanoConfiguration
    { ccLogPath             :: !FilePath
    -- ^ The location of the log files on the filesystem.
    , ccLogConfig           :: !FilePath
    -- ^ The location of the log configuration on the filesystem.
    , ccDBPath              :: !FilePath
    -- ^ The location of the DB on the filesystem.
    , ccSocketDir           :: !FilePath
    -- ^ Directory with local sockets:  ${dir}/node-{core,relay}-${node-id}.socket.
    , ccApplicationLockFile :: !FilePath
    -- ^ The location of the application lock file that is used
    -- as a semaphore se we can run just one application
    -- instance at a time.

    , ccTopologyInfo        :: !TopologyInfo
    -- ^ The network topology.
    , ccCore                :: !Core
    , ccNTP                 :: !NTP
    , ccUpdate              :: !Update
    , ccTXP                 :: !TXP
    , ccDLG                 :: !DLG
    , ccBlock               :: !Block
    , ccNode                :: !Node
    , ccTLS                 :: !TLS
    , ccWallet              :: !Wallet
    } deriving (Eq, Show)

-- | Do we require network magic or not?
-- Network magic allows the differentiation from mainnet and testnet.
data RequireNetworkMagic
    = RequireNetworkMagic
    | NoRequireNetworkMagic
    deriving (Eq, Show, Generic)

-- | The type of the protocol being run on the node.
data NodeProtocol
    = BFTProtocol
    | PraosProtocol
    | MockPBFTProtocol
    | RealPBFTProtocol
    deriving (Eq, Show, Generic)

-- | Core configuration.
-- For now, we only store the path to the genesis file(s) and their hash.
-- The rest is in the hands of the modules/features that need to use it.
-- The info flow is:
-- __genesis config ---> genesis file__
-- And separately:
-- __genesis file ---> runtime config ---> running node__
-- __static config ---> ...__
data Core = Core
    { coGenesisFile                 :: !FilePath
    -- ^ Genesis source file JSON.
    , coGenesisHash                 :: !Text
    -- ^ Genesis previous block hash.
    , coNodeId                      :: !(Maybe Int)
    -- ^ Core node ID, the number of the node.
    , coNumCoreNodes                :: !(Maybe Int)
    -- ^ The number of the core nodes.
    , coNodeProtocol                :: !NodeProtocol
    -- ^ The type of protocol run on the node.
    , coStaticKeySigningKeyFile     :: !(Maybe FilePath)
    -- ^ Static key signing file.
    , coStaticKeyDlgCertFile        :: !(Maybe FilePath)
    -- ^ Static key delegation certificate.
    , coRequiresNetworkMagic        :: !RequireNetworkMagic
    -- ^ Do we require the network byte indicator for mainnet, testnet or staging?
    , coPBftSigThd                  :: !(Maybe Double)
    -- ^ PBFT signature threshold system parameters

    } deriving (Eq, Show, Generic)

data Spec = Spec
    { spInitializer       :: !Initializer
      -- ^ Other data which depend on genesis type.
    , spBlockVersionData  :: !BlockVersionData
      -- ^ Genesis 'BlockVersionData'.
    , spProtocolConstants :: !ProtocolConstants
      -- ^ Other constants which affect consensus.
    , spFTSSeed           :: !Text
      -- ^ Seed for FTS for 0-th epoch.
    , spHeavyDelegation   :: !Text
      -- ^ Genesis state of heavyweight delegation.
    , spAVVMDistr         :: !Text
      -- ^ Genesis data describes avvm utxo.
    } deriving (Eq, Show)

-- | This data type contains various options presense of which depends
-- on whether we want genesis for mainnet or testnet.
data Initializer = Initializer
    { inTestBalance       :: !TestBalance
    , inFakeAvvmBalance   :: !FakeAvvmBalance
    , inAVVMBalanceFactor :: !Word64
    , inUseHeavyDlg       :: !Bool
    , inSeed              :: !Integer
      -- ^ Seed to use to generate secret data. It's used only in
      -- testnet, shouldn't be used for anything important.
    } deriving (Eq, Show)

-- | These options determine balances of nodes specific for testnet.
data TestBalance = TestBalance
    { tePoors          :: !Word
      -- ^ Number of poor nodes (with small balance).
    , teRichmen        :: !Word
      -- ^ Number of rich nodes (with huge balance).
    , teRichmenShare   :: !Double
      -- ^ Portion of stake owned by all richmen together.
    , teUseHDAddresses :: !Bool
      -- ^ Whether generate plain addresses or with hd payload.
    , teTotalBalance   :: !Word64
      -- ^ Total balance owned by these nodes.
    } deriving (Eq, Show)

-- | These options determines balances of fake AVVM nodes which didn't
-- really go through vending, but pretend they did.
data FakeAvvmBalance = FakeAvvmBalance
    { faCount      :: !Word
    , faOneBalance :: !Word64
    } deriving (Eq, Show)

-- | If we require options to automatically restart a module.
data ModuleAutoRestart
    = ModuleRestart
    | ModuleNoRestart
    deriving (Eq, Show)

data BlockVersionData = BlockVersionData
    { bvdScriptVersion     :: !Word16
    , bvdSlotDuration      :: !Int
    , bvdMaxBlockSize      :: !Natural
    , bvdMaxHeaderSize     :: !Natural
    , bvdMaxTxSize         :: !Natural
    , bvdMaxProposalSize   :: !Natural
    , bvdMpcThd            :: !Word64
    , bvdHeavyDelThd       :: !Word64
    , bvdUpdateVoteThd     :: !Word64
    , bvdUpdateProposalThd :: !Word64
    , bvdUpdateImplicit    :: !Word64
    , bvdSoftforkRule      :: !SoftForkRule
    , bvdTXFeePolicy       :: !TxFeePolicy
    , bvdUnlockStakeEpoch  :: !Word64
    } deriving (Eq, Show)

data SoftForkRule = SoftForkRule
    { sfrInitThd      :: !Word64
    , sfrMinThd       :: !Word64
    , sfrThdDecrement :: !Word64
    } deriving (Eq, Show)

data TxFeePolicy = TxFeePolicy
    { txfTXSizeLinear :: !TxSizeLinear
    } deriving (Eq, Show)

data TxSizeLinear = TxSizeLinear
    { txsA :: !Word64
    , txsB :: !Word64
    } deriving (Eq, Show)

data ProtocolConstants = ProtocolConstants
    { prK             :: !Word64
    -- ^ Security parameter from the paper.
    , prProtocolMagic :: !Word32
    -- ^ Magic constant for separating real/testnet.
    } deriving (Eq, Show)

data NTP = NTP
    { ntpResponseTimeout :: !Int
    , ntpPollDelay       :: !Int
    , ntpServers         :: ![Text]
    } deriving (Eq, Show)

data Update = Update
    { upApplicationName       :: !Text
    -- ^ Update application name.
    , upApplicationVersion    :: !Int
    -- ^ Update application version.
    , upLastKnownBlockVersion :: !LastKnownBlockVersion
    -- ^ Update last known block version.
    } deriving (Eq, Show)

data LastKnownBlockVersion = LastKnownBlockVersion
    { lkbvMajor :: !Int
    -- ^ Last known block version major.
    , lkbvMinor :: !Int
    -- ^ Last known block version minor.
    , lkbvAlt   :: !Int
    -- ^ Last known block version alternative.
    } deriving (Eq, Show)

data TXP = TXP
    { txpMemPoolLimitTx        :: !Int
    -- ^ Limit on the number of transactions that can be stored in the mem pool.
    , txpAssetLockedSrcAddress :: ![Text]
    -- ^ Set of source address which are asset-locked. Transactions which
    -- use these addresses as transaction inputs will be silently dropped.
    } deriving (Eq, Show)

data DLG = DLG
    { dlgCacheParam          :: !Int
      -- ^ This value parameterizes size of cache used in Delegation.
      -- Not bytes, but number of elements.
    , dlgMessageCacheTimeout :: !Int
      -- ^ Interval we ignore cached messages for if it's sent again.
    } deriving (Eq, Show)

data Block = Block
    { blNetworkDiameter        :: !Int
      -- ^Estimated time needed to broadcast message from one node to all other nodes.
    , blRecoveryHeadersMessage :: !Int
      -- ^Maximum amount of headers node can put into headers message while in "after offline" or "recovery" mode.
    , blStreamWindow           :: !Int
      -- ^ Number of blocks to have inflight
    , blNonCriticalCQBootstrap :: !Double
      -- ^ If chain quality in bootstrap era is less than this value, non critical misbehavior will be reported.
    , blNonCriticalCQ          :: !Double
      -- ^ If chain quality after bootstrap era is less than this value, non critical misbehavior will be reported.
    , blCriticalCQ             :: !Double
      -- ^ If chain quality after bootstrap era is less than this value, critical misbehavior will be reported.
    , blCriticalCQBootstrap    :: !Double
      -- ^ If chain quality in bootstrap era is less than this value, critical misbehavior will be reported.
    , blCriticalForkThreshold  :: !Int
      -- ^ Number of blocks such that if so many blocks are rolled back, it requires immediate reaction.
    , blFixedTimeCQ            :: !Int
      -- ^ Chain quality will be also calculated for this amount of seconds.
    } deriving (Eq, Show)

--- | Top-level Cardano SL node configuration
data Node = Node
    { noSlotLength                      :: !Consensus.SlotLength
    -- ^ Slot length time.
    , noNetworkConnectionTimeout        :: !Int
    -- ^ Network connection timeout in milliseconds.
    , noHandshakeTimeout                :: !Int
    -- ^ Protocol acknowledgement timeout in milliseconds.
    } deriving (Eq, Show)

data TLS = TLS
    { tlsCA      :: !Certificate
    , tlsServer  :: !Certificate
    , tlsClients :: !Certificate
    } deriving (Eq, Show)

data Certificate = Certificate
    { certOrganization :: !Text
    , certCommonName   :: !Text
    , certExpiryDays   :: !Int
    , certAltDNS       :: ![Text]
    } deriving (Eq, Show)

-- | Wallet rate-limiting/throttling parameters
data Wallet = Wallet
    { thEnabled :: !Bool
    , thRate    :: !Int
    , thPeriod  :: !Text
    , thBurst   :: !Int
    } deriving (Eq, Show)
