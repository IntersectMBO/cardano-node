{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-imports #-}
module Cardano.Report
  ( module Cardano.Report
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=))
import Data.ByteString qualified as BS
import Data.HashMap.Lazy qualified as HM
import Data.List (last)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time.Clock
import System.Posix.User

import Cardano.Analysis.API
import Cardano.Analysis.Context
import Cardano.Analysis.Ground
import Cardano.Analysis.Run hiding (Version)
import Cardano.Analysis.Run qualified as Run


newtype Author   = Author   { unAuthor   :: Text } deriving newtype (FromJSON, ToJSON)
newtype Revision = Revision { unRevision :: Int }  deriving newtype (FromJSON, ToJSON)
newtype ShortId  = ShortId  { unShortId  :: Text } deriving newtype (FromJSON, ToJSON)

data Report
  = Report
    { rAuthor       :: !Author
    , rDate         :: !UTCTime
    , rRevision     :: !Revision
    , rLocliVersion :: !Run.Version
    , rTarget       :: !Version
    }
instance ToJSON Report where
  toJSON Report{..} =
    object
      [ "author"     .= rAuthor
      , "date"       .= rDate
      , "revision"   .= rRevision
      , "locli"      .= rLocliVersion
      , "target"     .= rTarget
      ]

getReport :: Version -> Maybe Revision -> IO Report
getReport rTarget mrev = do
  rAuthor <- (getUserEntryForName =<< getLoginName) <&> Author . T.pack . userGecos
  rDate <- getCurrentTime
  let rRevision = fromMaybe (Revision 1) mrev
      rLocliVersion = Run.getVersion
  pure Report{..}

data Workload
  = WValue
  | WPlutus

instance ToJSON Workload where
  toJSON = \case
    WValue  -> "Value"
    WPlutus -> "Plutus"

data RunSpec
  = RunSpec
    { rsMeta      :: !Metadata
    , rsShortId   :: !ShortId
    , rsWorkload  :: !Workload
    , rsManifest  :: !Manifest
    }

instance ToJSON RunSpec where
  toJSON RunSpec{rsManifest=Manifest{..},..} =
    object
      [ "meta"       .= rsMeta
      , "id"         .= rsShortId
      , "workload"   .= rsWorkload
      , "branch"     .= mNodeBranch
      , "ver"        .= mNodeApproxVer
      , "rev"        .=
        object
        [ "node"         .= mNode
        , "network"      .= mNetwork
        , "ledger"       .= mLedger
        , "plutus"       .= mPlutus
        , "crypto"       .= mCrypto
        , "base"         .= mBase
        , "prelude"      .= mPrelude
        ]
      ]

liftRunSpec :: Run -> RunSpec
liftRunSpec Run{..} =
  RunSpec
  { rsMeta      = metadata
  , rsShortId   = ShortId "rc4"
  , rsWorkload  = WValue
  , rsManifest  = manifest metadata & unsafeShortenManifest 5
  }

generate :: InputDir -> Maybe TextInputFile -> Run -> [Run] -> IO (ByteString, Text)
generate (InputDir ede) mReport (liftRunSpec -> base) (fmap liftRunSpec -> runs) = do
  pure (mempty, mempty)
