{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.DataPoint.Forward.Types
  ( TestNodeInfo (..)
  , BlockchainStatus (..)
  ) where

import qualified Data.Aeson as A
import           Data.Text (Text)
import           GHC.Generics

data TestNodeInfo = TestNodeInfo
  { niName     :: !Text
  , niVersion  :: !Text
  , niCommit   :: !Text
  , niProtocol :: !Text
  } deriving (Eq, Generic, A.ToJSON, A.FromJSON, Show)

data BlockchainStatus = BlockchainStatus
  { bsEpoch :: !Int
  , bsSlot  :: !Int
  } deriving (Eq, Generic, A.ToJSON, A.FromJSON, Show)
