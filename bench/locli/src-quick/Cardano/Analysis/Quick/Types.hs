{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-partial-fields #-}

module Cardano.Analysis.Quick.Types
       (module Cardano.Analysis.Quick.Types)
       where

import           Cardano.Analysis.API.Ground (Host (..), JsonLogfile (..), LogObjectSource (..))
import           Cardano.Unlog.BackendDB (LoadRawResult (..))
import           Cardano.Unlog.LogObject (HostLogs, RunLogs)
import           Cardano.Unlog.LogObjectDB (SummaryDB (..))
import           Cardano.Util (I)

import           Codec.Serialise (Serialise (..))
import           Data.Profile (ProfileEntry)
import           Data.Text.Short (ShortText, fromShortByteString, toShortByteString)
import           GHC.Generics (Generic)

import           Database.Sqlite.Easy (SQLData)


instance Serialise ShortText where
  encode = encode . toShortByteString
  decode = decode >>= maybe (fail "fromShortByteString: not a ShortText") pure . fromShortByteString

deriving via ShortText instance Serialise Host
deriving via FilePath  instance Serialise JsonLogfile
deriving instance Serialise LogObjectSource

deriving instance Generic SummaryDB
deriving instance Serialise SummaryDB

deriving instance Serialise SQLData

deriving instance Generic LoadRawResult
deriving instance Serialise LoadRawResult

-- quick queries (and their result type) do not support encoding GHC profiling data
instance {-# OVERLAPS #-} Serialise [ProfileEntry I] where
  encode = mempty
  decode = pure []

deriving instance Serialise a => Serialise (HostLogs a)
deriving instance Serialise a => Serialise (RunLogs a)

{-
data RemoteQueryMeta = QueryRemote
  { qrmRunIds :: [String]
  , qrmNodes  :: [Host]
  , qrmQuery  :: ()
  }
-}

data RemoteQueryResult
  = RemoteQueryResult
    { rqrQuery  :: ()
    , rqrResult :: RunLogs LoadRawResult
    }
  | RemoteQueryError
    { rqrError  :: String
    }
  deriving (Generic, Serialise)
