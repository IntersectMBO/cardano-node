{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
module Cardano.Analysis.API.Context (module Cardano.Analysis.API.Context) where

import Cardano.Prelude

import Control.Monad (fail)

import Data.Aeson ( FromJSON (..), ToJSON (..), Value
                  , withObject, object, (.:), (.:?), (.=), (.!=))
import Data.Aeson.Key qualified as AE
import Data.Aeson.KeyMap qualified as AE
import Data.Aeson.Types qualified as AE
import Data.Map.Strict qualified as Map
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

data PlutusParams
  = PlutusParams
  { ppType            :: Text
  , ppScript          :: Text
  }
  deriving (Eq, Generic, Show, NFData)

instance FromJSON PlutusParams where
  parseJSON = withObject "PlutusParams" $ \v ->
    PlutusParams
      <$> v .:? "type"    .!= ""
      <*> v .:? "script"  .!= ""

instance ToJSON PlutusParams where
  toJSON PlutusParams{..} =
    object
      [ "type"    .= ppType
      , "script"  .= ppScript
      ]

data GeneratorProfile
  = GeneratorProfile
  { add_tx_size      :: Word64
  , inputs_per_tx    :: Word64
  , outputs_per_tx   :: Word64
  , tps              :: Double
  , tx_count         :: Word64
  , plutusMode       :: Maybe Bool            -- legacy format
  , plutusAutoMode   :: Maybe Bool            -- legacy format
  , plutus           :: Maybe PlutusParams
  }
  deriving (Eq, Generic, Show, ToJSON, NFData)

instance FromJSON GeneratorProfile where
  parseJSON o = cleanup <$> AE.genericParseJSON AE.defaultOptions o
    where
      cleanup g = case g of
        GeneratorProfile{plutus = Just PlutusParams{..}}
          | T.null ppType || T.null ppScript -> g {plutus = Nothing}
        _ -> g

plutusLoopScript :: GeneratorProfile -> Maybe Text
plutusLoopScript GeneratorProfile{plutusMode, plutusAutoMode, plutus}
  | Just True <- (&&) <$> plutusAutoMode <*> plutusMode
    = Just "Loop"
  | otherwise
    = ppScript `fmap` plutus


newtype Commit   = Commit  { unCommit  :: Text } deriving newtype (Eq, Show, FromJSON, ToJSON, NFData)
newtype Branch   = Branch  { unBranch  :: Text } deriving newtype (Eq, Show, FromJSON, ToJSON, NFData)
newtype Version  = Version { unVersion :: Text } deriving newtype (Eq, Show, FromJSON, ToJSON, NFData)

unsafeShortenCommit :: Int -> Commit -> Commit
unsafeShortenCommit n (Commit c) = Commit (T.take n c)

data ComponentInfo
  = ComponentInfo
    { ciName    :: !Text
    , ciCommit  :: !Commit
    , ciBranch  :: !(Maybe Branch)
    , ciStatus  :: !(Maybe Text)
    , ciVersion :: !Version
    }
  deriving (Eq, Generic, NFData, Show)

componentSummary :: ComponentInfo -> Text
componentSummary ComponentInfo{..} =
 T.unwords [ unCommit ciCommit, "(" <> unVersion ciVersion <> ")" ]

unknownComponent :: Text -> ComponentInfo
unknownComponent ciName = ComponentInfo
  { ciCommit  = Commit "unknown"
  , ciBranch  = Nothing
  , ciStatus  = Nothing
  , ciVersion = Version "unknown"
  , ..
  }

instance FromJSON ComponentInfo where
  parseJSON = withObject "Component" $ \v -> do
    ciName    <- v .: "name"
    ciCommit  <- v .:? "commit" .!= Commit "unknown"      -- workaround for commit hash missing from manifest
    ciBranch  <- v .:? "branch"
    ciStatus  <- v .:? "status"
    ciVersion <- v .: "version"
    pure ComponentInfo{..}

instance ToJSON ComponentInfo where
  toJSON ComponentInfo{..} =
    object
      [ "name"    .= ciName
      , "commit"  .= ciCommit
      , "branch"  .= ciBranch
      , "status"  .= ciStatus
      , "version" .= ciVersion
      ]

newtype Manifest = Manifest { unManifest :: Map.Map Text ComponentInfo }
  deriving stock (Generic)
  deriving newtype (Eq, NFData, Show, FromJSON, ToJSON)

getComponent :: Text -> Manifest -> ComponentInfo
getComponent k = fromMaybe (unknownComponent k) . Map.lookup k . unManifest

componentBranch :: ComponentInfo -> Branch
componentBranch = fromMaybe (Branch "unknown") . ciBranch

unsafeShortenManifest :: Int -> Manifest -> Manifest
unsafeShortenManifest n m =
  m { unManifest = shortenComponentInfo <$> unManifest m }
 where
   shortenComponentInfo c@ComponentInfo{..} =
     c { ciCommit = unsafeShortenCommit n ciCommit }

-- WARNING:  Keep in sync with workbench/manifest.sh:WB_MANIFEST_PACKAGES
--    Better yet, move manifest collection _into_ locli.
--    ..but since the manifest is part of meta.json, that'll cause more thinking.
manifestPackages :: [Text]
manifestPackages =
  -- In the order of integration:
  [ "cardano-node"
  , "ouroboros-consensus"
  , "ouroboros-network"
  , "cardano-ledger-core"
  , "plutus-core"
  , "cardano-crypto"
  , "cardano-prelude"
  ]

data Metadata
  = Metadata
  { tag               :: Text
  , batch             :: Text
  , ident             :: Text
  , node_ghc_version  :: Text
  , profile           :: Text
  , era               :: Text
  , manifest          :: Manifest
  , profile_content   :: AE.KeyMap Value
  }
  deriving (Generic, NFData, Show, ToJSON)

instance FromJSON Metadata where
  parseJSON =
    withObject "Metadata" $ \v -> do

      tag              <- v .: "tag"
      batch            <- v .: "batch"
      manifest         <- (v .: "manifest")
                         <|> compatParseManifest v
      profile          <- v .: "profile"
      profile_content  <- v .: "profile_content"
      generator        <- profile_content .: "generator"

      ident            <- (v .:? "ident")
                         <&> fromMaybe (unVersion . ciVersion $
                                         getComponent "cardano-node" manifest)
      node_ghc_version <- v .:?  "node_ghc_version" .!= "unknown"
      eraDirect        <- v .:?  "era"
      eraProfile       <- profile_content .:? "era"
      eraGenerator     <- generator .:? "era"
      era <- case eraDirect <|> eraProfile <|> eraGenerator of
        Just x -> pure x
        Nothing -> fail "While parsing run metafile:  missing era specification"

      pure Metadata{..}
   where
     compatParseManifest v' = do
       -- Map legacy into hot newness.
       v <- v' .: "manifest"
       node      <- commitToCI v "cardano-node"
       network   <- commitToCI v "ouroboros-network"
       ledger    <- commitToCI v "cardano-ledger"
       plutus    <- commitToCI v "plutus"
       crypto    <- commitToCI v "cardano-crypto"
       base      <- commitToCI v "cardano-base"
       prelude   <- commitToCI v "cardano-prelude"
       let kvs = Map.fromList
             [ ("cardano-node",        node)
             , ("ouroboros-consensus", network)
             , ("ouroboros-network",   network)
             , ("cardano-ledger-core", ledger)
             , ("plutus-core",         plutus)
             , ("cardano-crypto",      crypto)
             , ("cardano-base",        base)
             , ("cardano-prelude",     prelude)
             ]
       pure (Manifest kvs)
      where
        commitToCI :: AE.KeyMap Value -> Text -> AE.Parser ComponentInfo
        commitToCI v ciName = do
          ciCommit <- v .: AE.fromText ciName
          pure ComponentInfo{ ciVersion = Version "unknown"
                            , ciBranch = Nothing
                            , ciStatus = Nothing
                            , ..}
