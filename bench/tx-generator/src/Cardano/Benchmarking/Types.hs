{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Cardano.Benchmarking.Types
Description : Types for running benchmarks.
-}
module Cardano.Benchmarking.Types
  ( module Cardano.Benchmarking.Types
  ) where

import           Data.Aeson (ToJSON)
import           GHC.Generics (Generic)


-- | Transactions we decided to announce now.
newtype ToAnnce tx = ToAnnce [tx]

-- | Transactions announced, yet unacked by peer.
newtype UnAcked tx = UnAcked [tx]

-- | Peer acknowledged this many txids of the outstanding window.
newtype Ack = Ack Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)

-- | Peer requested this many txids to add to the outstanding window.
newtype Req = Req Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)

-- | This many Txs sent to peer.
newtype Sent = Sent Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)
deriving stock instance Generic Sent

-- | This many Txs requested by the peer, but not available for sending.
newtype Unav = Unav Int deriving newtype (Enum, Eq, Integral, Num, Ord, Real, Show)
deriving stock instance Generic Unav

-- | This controls how to react to errors during the benchmark.
data SubmissionErrorPolicy
  = FailOnError
  | LogErrors
  deriving stock (Eq, Show)

instance ToJSON Sent
instance ToJSON Unav
