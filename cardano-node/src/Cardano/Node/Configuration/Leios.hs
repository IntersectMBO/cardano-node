{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Configuration.Leios(
  LeiosDbConfig(..)
  ) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), object,
                   withObject, (.:), (.=))

-- | Which LeiosDB backend to run. Where its files live is not configurable
-- here: the SQLite backend has a volatile and an immutable partition, and
-- each follows the node's own 'DatabasePath' the way the VolatileDB and the
-- ImmutableDB do. Naming them separately could only put them somewhere that
-- contradicts that.
data LeiosDbConfig = LeiosDbInMemory
   | LeiosDbSQLite
   deriving (Eq, Show)

instance FromJSON LeiosDbConfig where
  parseJSON = withObject "LeiosDbConfig" $ \o -> do
    backend :: String <- o .: "Backend"
    case backend of
      "InMemory" -> return LeiosDbInMemory
      "SQLite" -> return LeiosDbSQLite
      _ -> fail $ "Invalid LeiosDb backend " <> backend <> ", did you mean InMemory or SQLite?"

instance ToJSON LeiosDbConfig where
  toJSON LeiosDbInMemory =
    object
      [ "Backend" .=  String "InMemory"
      ]
  toJSON LeiosDbSQLite =
    object
      [ "Backend" .= String "SQLite"
      ]
