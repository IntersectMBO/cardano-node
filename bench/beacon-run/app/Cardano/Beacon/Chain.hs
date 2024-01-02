{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module  Cardano.Beacon.Chain
        ( module Cardano.Beacon.Chain
        ) where

import           Control.Exception (SomeException (..), try)
import           Data.Aeson
import qualified Data.ByteString as B (readFile)
import           Data.Map as Map (Map, empty, lookup, size, toAscList)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Cardano.Beacon.Console
import           Cardano.Beacon.Types


data BeaconChain = BeaconChain {
  -- | Base directory of a synthetic chain, or cardano-node.
    chHomeDir       :: !FilePath

  -- | Path for the db passed to db-analyser. This is relative to @chHomeDir@.
  , chDbDir         :: !FilePath

  -- | Path to the node's config.json file. This is relative to @chHomeDir@.
  , chConfigFile    :: !FilePath

  -- | Starts analysis at the given SlotNo.
  , chFromSlot      :: !(Maybe Int)

  -- | Stops after analysing given number of blocks.
  , chProcessBlocks :: !(Maybe Int)

  -- | Free-form description of the chain fragment.
  , chDescription   :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON BeaconChain where
  parseJSON = genericParseJSON defaultOptions { sumEncoding = ObjectWithSingleField }

newtype ChainName = ChainName Text
        deriving (Eq, Ord, Show, FromJSON, FromJSONKey)
          via Text

newtype Chains = Chains {unChains :: Map ChainName BeaconChain}
        deriving (Show, FromJSON)
          via Map ChainName BeaconChain


emptyChains :: Chains
emptyChains = Chains Map.empty

countChains :: Chains -> Int
countChains = Map.size . unChains

lookupChain :: ChainName -> Chains -> Maybe BeaconChain
lookupChain name = (name `Map.lookup`) . unChains

renderChainsInfo :: Chains -> [String]
renderChainsInfo (Chains chains) =
  map (T.unpack . infoLine) (Map.toAscList chains)
  where
    infoLine (ChainName name, BeaconChain{ chDescription = descr }) =
      T.concat [alignRight 16 name, " -- ", fromMaybe "(no description provided)" descr]
    alignRight width = T.justifyRight width ' ' . T.take width

loadChainsInfo :: FilePath -> IO Chains
loadChainsInfo jsonFile =
  try (B.readFile jsonFile >>= throwDecodeStrict') >>= either
    warn
    pure
  where
    warn (SomeException e) = do
      printStyled StyleWarning $ unlines
        [ "There was an error reading chains info in '" ++ jsonFile ++ "'"
        , "I will default to no registered chains; the error was:"
        , show e
        ]
      pure emptyChains
