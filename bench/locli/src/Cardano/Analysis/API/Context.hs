{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
module Cardano.Analysis.API.Context (module Cardano.Analysis.API.Context) where

import Cardano.Prelude

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, object, (.:), (.:?), (.=))
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, NominalDiffTime)


-- This is difficult: we have two different genesis-related structures:
--  1. the real ShelleyGenesis
--  2. the profile-supplied genesis specification used by the workbench & bench-on-AWS.
--
data GenesisSpec
  = GenesisSpec
  { delegators          :: Word64
  , utxo                :: Word64
  }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, NFData)

-- | Partial 'Cardano.Ledger.Shelley.Genesis.ShelleyGenesis'
data Genesis
  = Genesis
  { activeSlotsCoeff   :: Double
  , protocolParams     :: PParams
  , networkMagic       :: Word64
  , epochLength        :: Word64
  , systemStart        :: UTCTime
  , slotsPerKESPeriod  :: Word64
  , slotLength         :: NominalDiffTime
  , maxKESEvolutions   :: Word64
  , securityParam      :: Word64
  }
  deriving (Generic, Show, FromJSON, ToJSON, NFData)

genesesSameButTimeP :: Genesis -> Genesis -> Bool
genesesSameButTimeP l r =
  ((==) `on` activeSlotsCoeff) l r &&
  ((==) `on` protocolParams) l r &&
  ((==) `on` networkMagic) l r &&
  ((==) `on` epochLength) l r &&
  ((==) `on` slotsPerKESPeriod) l r &&
  ((==) `on` slotLength) l r &&
  ((==) `on` maxKESEvolutions) l r &&
  ((==) `on` securityParam) l r

-- | Partial 'Cardano.Ledger.Shelley.PParams.PParams'
data PParams
  = PParams
  { maxTxSize         :: Word64
  , maxBlockBodySize  :: Word64
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON, NFData)

data GeneratorProfile
  = GeneratorProfile
  { add_tx_size      :: Word64
  , inputs_per_tx    :: Word64
  , outputs_per_tx   :: Word64
  , tps              :: Double
  , tx_count         :: Word64
  , plutusMode       :: Maybe Bool
  , plutusLoopScript :: Maybe FilePath
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON, NFData)

newtype Commit   = Commit  { unCommit  :: Text } deriving newtype (Eq, Show, FromJSON, ToJSON, NFData)
newtype Branch   = Branch  { unBranch  :: Text } deriving newtype (Eq, Show, FromJSON, ToJSON, NFData)
newtype Version  = Version { unVersion :: Text } deriving newtype (Eq, Show, FromJSON, ToJSON, NFData)

unsafeShortenCommit :: Int -> Commit -> Commit
unsafeShortenCommit n (Commit c) = Commit (T.take n c)

data Manifest
  = Manifest
    { mNode          :: !Commit
    , mNodeApproxVer :: !Version
    , mNodeBranch    :: !Branch
    , mNodeStatus    :: !Text
    , mNetwork       :: !Commit
    , mLedger        :: !Commit
    , mPlutus        :: !Commit
    , mCrypto        :: !Commit
    , mBase          :: !Commit
    , mPrelude       :: !Commit
    }
  deriving (Eq, Generic, NFData, Show)

unsafeShortenManifest :: Int -> Manifest -> Manifest
unsafeShortenManifest n m@Manifest{..} =
  m { mNode    = unsafeShortenCommit n mNode
    , mNetwork = unsafeShortenCommit n mNetwork
    , mLedger  = unsafeShortenCommit n mLedger
    , mPlutus  = unsafeShortenCommit n mPlutus
    , mCrypto  = unsafeShortenCommit n mCrypto
    , mBase    = unsafeShortenCommit n mBase
    , mPrelude = unsafeShortenCommit n mPrelude
    }

instance FromJSON Manifest where
  parseJSON = withObject "Manifest" $ \v -> do
    mNode          <- v .: "cardano-node"
    mNodeBranch    <- v .:? "cardano-node-branch"  <&> fromMaybe (Branch "unknown")
    mNodeApproxVer <- v .:? "cardano-node-version" <&> fromMaybe (Version "unknown")
    mNodeStatus    <- v .: "cardano-node-status"
    mNetwork       <- v .: "ouroboros-network"
    mLedger        <- v .: "cardano-ledger"
    mPlutus        <- v .: "plutus"
    mCrypto        <- v .: "cardano-crypto"
    mBase          <- v .: "cardano-base"
    mPrelude       <- v .: "cardano-prelude"
    pure Manifest{..}

instance ToJSON Manifest where
  toJSON Manifest{..} =
    object
      [ "cardano-node"         .= mNode
      , "cardano-node-branch"  .= mNodeBranch
      , "cardano-node-version" .= mNodeApproxVer
      , "cardano-node-status"  .= mNodeStatus
      , "ouroboros-network"    .= mNetwork
      , "cardano-ledger"       .= mLedger
      , "plutus"               .= mPlutus
      , "cardano-crypto"       .= mCrypto
      , "cardano-base"         .= mBase
      , "cardano-prelude"      .= mPrelude
      ]

data Metadata
  = Metadata
  { tag       :: Text
  , batch     :: Text
  , ident     :: Text
  , profile   :: Text
  , era       :: Text
  , manifest  :: Manifest
  }
  deriving (Generic, NFData, Show, ToJSON)

instance FromJSON Metadata where
  parseJSON =
    withObject "Metadata" $ \v -> do

      tag      <- v .:  "tag"
      batch    <- v .:  "batch"
      ident    <- (v .:? "ident")
                  <&> fromMaybe batch
      profile  <- v .:  "profile"
      era      <- v .:  "era"
      manifest <- v .:  "manifest"

      pure Metadata{..}
