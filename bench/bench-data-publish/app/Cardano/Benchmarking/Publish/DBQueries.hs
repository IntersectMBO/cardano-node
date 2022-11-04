{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module  Cardano.Benchmarking.Publish.DBQueries
        ( dbGetRuns
        , dbPublishRun
        , dbStoreRun
        , dbRefreshView
        ) where

import           Control.Monad                         (unless)
import           Data.ByteString.Char8                 (ByteString)
import           Hasql.Decoders                        as Dec
import           Hasql.Encoders                        as Enc (bool, noParams,
                                                               nonNullable,
                                                               param)
import qualified Hasql.Session                         as DB
import           Hasql.Statement

import           Cardano.Benchmarking.Publish.DBSchema
import           Cardano.Benchmarking.Publish.Types


getRunId :: ByteString -> Statement MetaStub (Maybe Int)
getRunId schema
  = Statement sql encClusterRun (rowMaybe decInt4) False
  where
    sql = "SELECT id FROM " <> schema <> ".cluster_run\
          \ WHERE run_profile=$1 AND run_commit=$2 AND run_at=$3"

createRunId :: ByteString -> Statement MetaStub Int
createRunId schema
  = Statement sql encClusterRun (singleRow decInt4) False
  where
    sql = "INSERT INTO " <> schema <> ".cluster_run (run_profile, run_commit, run_at)\
          \ VALUES ($1,$2,$3)\
          \ ON CONFLICT ON CONSTRAINT un_run_profile DO NOTHING\
          \ RETURNING id"

getRuns :: ByteString -> Statement () [(Int, MetaStub, Bool)]
getRuns schema
  = Statement sql Enc.noParams (rowList decClusterRun) False
  where
    sql = "SELECT * FROM " <> schema <> ".cluster_run ORDER BY id"

setRunPublished :: ByteString -> Statement (Bool, MetaStub) (Maybe Bool)
setRunPublished schema
  = Statement sql enc (rowMaybe decBool) False
  where
    enc = (fst >$< param (Enc.nonNullable Enc.bool)) <> (snd >$< encClusterRun)
    sql = "WITH cte AS\
          \ (SELECT id,run_published FROM " <> schema <> ".cluster_run WHERE run_profile=$2 AND run_commit=$3 AND run_at=$4)\
          \ UPDATE " <> schema <> ".cluster_run cr\
          \ SET run_published=$1\
          \ FROM cte\
          \ WHERE cr.id=cte.id\
          \ RETURNING cte.run_published"

setMeta :: ByteString -> Statement (Int, ByteString) ()
setMeta schema
  = Statement sql encRunInfo Dec.noResult False
  where
    sql = "INSERT INTO " <> schema <> ".run_info VALUES ($1,$2)\
          \ ON CONFLICT ON CONSTRAINT un_info_run_id DO UPDATE\
          \ SET meta=$1"

setResult :: ByteString -> Statement (Int, Maybe ByteString, Maybe ByteString) ()
setResult schema
  = Statement sql encRunResult Dec.noResult False
  where
    sql = "INSERT INTO " <> schema <> ".run_result VALUES ($1,$2,$3)\
          \ ON CONFLICT ON CONSTRAINT un_result_run_id DO UPDATE\
          \ SET blockprop=$1, clusterperf=$2"



-- returns whether the run has been created (True) or updated (False)
dbStoreRun :: DBSchema -> ClusterRun -> DB.Session Bool
dbStoreRun (DBSchema schemaName) ClusterRun{..}
  = do
    (runId, created) <-
      DB.statement metaStub (getRunId schemaName) >>= \case
        Nothing -> do
          runId <- DB.statement metaStub (createRunId schemaName)
          DB.statement (runId, runMeta) (setMeta schemaName)
          pure (runId, True)
        Just runId ->
          pure (runId, False)
    DB.statement (runId, runBlockProp, runClusterPerf) (setResult schemaName)
    pure created

dbRefreshView :: DBSchema -> DB.Session ()
dbRefreshView (DBSchema schemaName)
  = DB.sql $ "REFRESH MATERIALIZED VIEW " <> schemaName <> ".run;"

dbGetRuns :: DBSchema -> DB.Session [(Int, MetaStub, Bool)]
dbGetRuns (DBSchema schemaName)
  = DB.statement () (getRuns schemaName)

-- if this action did actually change the published status, it implicitly refreshes the materialized view
-- returns False if run was not found on DB
dbPublishRun :: DBSchema -> MetaStub -> Bool -> DB.Session Bool
dbPublishRun schema@(DBSchema schemaName) run published
  = do
    previous <- DB.statement (published, run) (setRunPublished schemaName)
    case previous of
      Just published' -> do
        unless (published == published') (dbRefreshView schema)
        pure True
      _ -> pure False
