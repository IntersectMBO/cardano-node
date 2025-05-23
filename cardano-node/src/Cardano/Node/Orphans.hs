{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans () where

import           Cardano.Api ()

import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.Genesis (GenesisConfigFlags (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots (Flag(..))
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.SizeInBytes (SizeInBytes (..))

import           Data.Aeson.Types
import qualified Data.Text as Text
import           Text.Printf (PrintfArg (..))

deriving instance Eq NodeDatabasePaths
deriving instance Show NodeDatabasePaths

instance PrintfArg SizeInBytes where
    formatArg (SizeInBytes s) = formatArg s

instance ToJSON AcceptedConnectionsLimit where
  toJSON AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit
          , acceptedConnectionsSoftLimit
          , acceptedConnectionsDelay
          } =
    object [ "AcceptedConnectionsLimit" .=
      object [ "hardLimit" .=
                  toJSON acceptedConnectionsHardLimit
             , "softLimit" .=
                  toJSON acceptedConnectionsSoftLimit
             , "delay" .=
                  toJSON acceptedConnectionsDelay
             ]
           ]

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"

instance FromJSON NodeDatabasePaths where
  parseJSON o@(Object{})=
    withObject "NodeDatabasePaths"
     (\v -> MultipleDbPaths
              <$> v .: "ImmutableDbPath"
              <*> v .: "VolatileDbPath"
     ) o
  parseJSON (String s) = return . OnePathForAllDbs $ Text.unpack s
  parseJSON _ = fail "NodeDatabasePaths must be an object or a string"

deriving newtype instance FromJSON (Flag symbol)
deriving newtype instance ToJSON (Flag symbol)

instance FromJSON GenesisConfigFlags where
  parseJSON = withObject "GenesisConfigFlags" $ \v ->
    GenesisConfigFlags
      <$> v .:? "EnableCSJ"       .!= True
      <*> v .:? "EnableLoEAndGDD" .!= True
      <*> v .:? "EnableLoP"       .!= True
      <*> v .:? "BlockFetchGracePeriod"
      <*> v .:? "BucketCapacity"
      <*> v .:? "BucketRate"
      <*> v .:? "CSJJumpSize"
      <*> v .:? "GDDRateLimit"
