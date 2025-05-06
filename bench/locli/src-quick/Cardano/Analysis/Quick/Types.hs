{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-partial-fields #-}

module Cardano.Analysis.Quick.Types
       (module Cardano.Analysis.Quick.Types)
       where

import           Cardano.Api (ExceptT)

import           Cardano.Analysis.API.Ground (Host (..), JsonLogfile (..), LogObjectSource (..))
import           Cardano.Unlog.BackendDB (LoadFromRunData (..), LoadRawResult (..))
import           Cardano.Unlog.LogObject (HostLogs, RunLogs)
import           Cardano.Unlog.LogObjectDB (SummaryDB (..))
import           Cardano.Util (I)

import           Codec.Serialise (Serialise (..), deserialiseOrFail, serialise)
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy as Lazy (ByteString)
import           Data.Kind (Type)
import           Data.Map.Strict as Map (Map, singleton)
import           Data.Profile (ProfileEntry)
import           Data.Text (Text)
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

data RemoteQueryResult l where
    RemoteQueryResult :: () -> Map String (RunLogs [l]) -> RemoteQueryResult l
    --{ rqrQuery  :: ()
    --, rqrResult :: Map String (RunLogs LoadRawResult)     -- map key: runId
    --}
    RemoteQueryError :: String -> RemoteQueryResult l
    --{ rqrError  :: String
    --}

instance Serialise l => Serialise (RemoteQueryResult l) where
  encode (RemoteQueryError e) = encode False <> encode e
  encode (RemoteQueryResult _ v) = encode True <> encode v

  decode = do
    success <- decode
    if success
      then RemoteQueryResult () <$> decode
      else RemoteQueryError <$> decode


{-
  This should eventually be part of a QuickQuery typeclass. This class is defined by:
  - a query + (possibly parametrizable) filter, making use of the LoadFromRunData typeclass
  - a (possibly parametrizable) reducer, making use of the Reducer typeclass
  - meaningful stdout output
  - optionally: file output, like e.g. CSV
  - optionally: a plot / plots
-}

type CSV = [[Text]]

type QuickQueryEnv = ([FilePath], [String])

-- type family   LoadFromConstraint l :: Constraint
-- type instance LoadFromConstraint l = LoadFromDB l
-- class (LoadFromConstraint l) => QuickQuery q l where

data QuickQueryAny where
  MkQuickQuery :: forall q . QuickQuery q => q -> QuickQueryAny

instance Show QuickQueryAny where
  show (MkQuickQuery q) = show q

type QuickQueryOnCLI = (String, String -> Maybe QuickQueryAny)

class (LoadFromRunData q, Show q) => QuickQuery q where
  type family QueryResult q :: Type

  qqQuery :: q -> QuickQueryEnv -> ExceptT String IO (RunLogs [LoadResult q])

  qqReduce :: q -> [LoadResult q] -> QueryResult q

  qqCSV   :: RunLogs (QueryResult q) -> CSV
  qqCSV   = const [[]]

  qqShow  :: RunLogs (QueryResult q) -> String
  qqShow  = const ""

  qqCLI2 :: [QuickQueryOnCLI]
  qqCLI2 = []


qqDecode :: Serialise (QueryResult q) => Lazy.ByteString -> Either String (RemoteQueryResult (QueryResult q))
qqDecode = first show . deserialiseOrFail

qqEncode :: Serialise (QueryResult q) => RunLogs (QueryResult q) -> Lazy.ByteString
qqEncode rs = serialise $ Map.singleton "runId_placeholder" rs

-- qqReduceGeneric :: (LoadFromRunData a, Reducer r) => a -> ([LoadResult a] -> [Elem r]) -> r -> [LoadResult a] -> Result r
-- qqReduceGeneric _ f r res = reduce r $ f res
