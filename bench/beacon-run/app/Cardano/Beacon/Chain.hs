{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module  Cardano.Beacon.Chain
        ( BeaconChain(..)
        , Chains(..)

        , countChains
        , loadChainsInfo
        , lookupChain
        ) where

import           Control.Exception (SomeException (..), try)
import           Data.Aeson
import qualified Data.ByteString as B (readFile)
import           Data.Map as Map (Map, empty, lookup, size)
import           GHC.Generics (Generic)

import           Cardano.Beacon.Console
import           Cardano.Beacon.Types


data BeaconChain = BeaconChain {
    chFromSlot      :: !(Maybe Int)
  , chProcessBlocks :: !(Maybe Int)
  , chDescription   :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON BeaconChain where
  parseJSON = genericParseJSON defaultOptions { sumEncoding = ObjectWithSingleField }

newtype Chains = Chains {unChains :: Map Chain BeaconChain}
        deriving Show
          via Map Chain BeaconChain

instance FromJSON Chains where
  parseJSON o = Chains <$> parseJSON o


emptyChains :: Chains
emptyChains = Chains Map.empty

countChains :: Chains -> Int
countChains = Map.size . unChains

lookupChain :: Chain -> Chains -> Maybe BeaconChain
lookupChain name = (name `Map.lookup`) . unChains

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
