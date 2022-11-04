{-# LANGUAGE OverloadedStrings #-}

module  Cardano.Benchmarking.Publish.DBConnection
        ( withDB
        ) where

import           Control.Exception     (finally)
import           Data.ByteString.Char8 as BS (unpack)

import           Hasql.Connection      as DB


withDB :: DB.Settings -> (Connection -> IO a) -> IO a
withDB connString action
  = DB.acquire connString >>= either (error . connError) go
  where
    connError :: DB.ConnectionError -> String
    connError err
      = "DBConnectionError: " <> maybe "<unspecified>" BS.unpack err
    go conn
      = action conn `finally` DB.release conn
