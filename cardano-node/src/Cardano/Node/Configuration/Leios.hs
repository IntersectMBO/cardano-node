{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Configuration.Leios(
  LeiosDbConfig(..)
  ) where

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), object,
                   withObject, (.:), (.=))

data LeiosDbConfig = LeiosDbInMemory
   | LeiosDbSQLite !FilePath !FilePath
   deriving (Eq, Show)

instance FromJSON LeiosDbConfig where
  parseJSON = withObject "LeiosDbConfig" $ \o -> do
    backend :: String <- o .: "Backend"
    case backend of
      "InMemory" -> return LeiosDbInMemory
      "SQLite" -> do
        volPath <- o .: "VolatileFilepath"
        immPath <- o .: "ImmutableFilepath"
        return $ LeiosDbSQLite volPath immPath
      _ -> fail $ "Invalid LeiosDb backend " <> backend <> ", did you mean InMemory or SQLite?"

instance ToJSON LeiosDbConfig where
  toJSON LeiosDbInMemory =
    object
      [ "Backend" .=  String "InMemory"
      ]
  toJSON (LeiosDbSQLite volPath immPath) =
    object
      [ "Backend" .= String "SQLite",
        "VolatileFilepath" .= volPath,
        "ImmutableFilepath" .= immPath
      ]
