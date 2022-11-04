{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Except                (runExceptT)
import           Data.Bool                                 (bool)
import           Data.ByteString.Char8                     as BS (empty, pack,
                                                                  readFile)

import           System.Directory
import           System.Directory.Extra                    (listDirectories)
import           System.Environment                        (lookupEnv)
import           System.FilePath
import           System.Posix.User                         (getLoginName)
import           Text.Printf

import           Data.Aeson
import           Hasql.Connection                          as DB (Connection,
                                                                  Settings,
                                                                  settings)
import qualified Hasql.Session                             as DB

import           Cardano.Benchmarking.Publish.DBConnection
import           Cardano.Benchmarking.Publish.DBQueries
import           Cardano.Benchmarking.Publish.DBSchema
import           Cardano.Benchmarking.Publish.Types
import           Command


main :: IO ()
main
  = do
    conf <- parseCommandLine
    dbSettings <- getDBSettings (appDB conf)
    eval conf dbSettings

eval :: Config -> DB.Settings -> IO ()
eval Config{..} dbSettings
  = case appCommand of

    Bootstrap anonRole_
      | not appForce -> putStrLn "'bootstrap' requires -f (force); it is a destructive operation"
      | otherwise -> withDB dbSettings $ \conn -> do
          result <- runExceptT $ do
              bootstrap dbSchema conn
              updateViews dbSchema (BS.pack anonRole_) conn
          case result of
            Left err -> errorMsg err
            Right views -> putStrLn $
              "successfully bootstrapped schema: '" ++ appDBSchema ++ "'\n\
              \views exposed to API (role '" ++ anonRole_ ++ "'): " ++ show views

    UpdateViews anonRole_ ->
      withDB dbSettings $ \conn -> do
        result <- runExceptT $
          updateViews dbSchema (BS.pack anonRole_) conn
        case result of
          Left err -> errorMsg err
          Right views -> putStrLn $
            "views exposed to API (role '" ++ anonRole_ ++ "'): " ++ show views ++
            "\nNB. if any view has been *renamed*, please drop the old one manually from the DB!"

    ImportAll targetDir -> do
      runMetas <- searchRuns targetDir
      putStrLn $ "found runs: " ++ show (length runMetas)
      unless (null runMetas) $
        withDB dbSettings $ \conn ->
          storeRunsToDB dbSchema conn runMetas

    Import targetFile ->
      withDB dbSettings $ \conn ->
        storeRunsToDB dbSchema conn [targetFile]

    List ->
      withDB dbSettings $ \conn ->
        dbGetRuns dbSchema `DB.run` conn >>= \case
          Left err -> errorMsg $ show err
          Right runs -> do
            forM_ runs $ \(ix, meta, publ) ->
              printf "%3i %s -- %s\n" ix (isPublished publ) (show meta)
            printf "----\n%i runs\n" (length runs)

    Publish publish target ->
      loadMetaStub target >>= \case
        Left err -> errorMsg err
        Right meta -> withDB dbSettings $ \conn -> do
          putStr $ bool "un-" "" publish ++ "publishing: " ++ target ++ " -- "
          dbPublishRun dbSchema meta publish `DB.run` conn >>= \case
            Left e      -> errorMsg $ show e
            Right found -> putStrLn $ bool "(run not in DB)" "DONE" found

  where
    dbSchema      = DBSchema (BS.pack appDBSchema)
    isPublished p = bool '-' '+' p : "published"
    errorMsg msg  = putStrLn $ "ERROR: (" ++ show appCommand ++ ") -- " ++ msg

storeRunsToDB :: DBSchema -> DB.Connection -> [FilePath] -> IO ()
storeRunsToDB dbSchema conn metaFiles
  = do
    anyCreated <- foldM storeClusterRun False metaFiles

    -- for any change to the run list itself (not the associated results)
    -- we need to refresh the materialized view
    when anyCreated $
      void $ dbRefreshView dbSchema `DB.run` conn
  where
    errorMsg result msg = putStrLn ("ERROR: " ++ msg) >> pure result
    storeClusterRun created metaFile
      = do
        putStr $ "storing: " ++ metaFile ++ " -- "
        loadClusterRun metaFile >>= \case
          Left err -> errorMsg created err
          Right run -> dbStoreRun dbSchema run `DB.run` conn
            >>= either
              (errorMsg created . show)
              (\created' -> putStrLn (bool "UPDATED" "CREATED" created') >> pure (created || created'))

searchRuns :: FilePath -> IO [FilePath]
searchRuns targetDir
  = do
    subDirs <- listDirectories targetDir
    filterM doesFileExist $ map (</> "meta.json") subDirs

normalizeMetaFilePath :: FilePath -> FilePath
normalizeMetaFilePath metaFile
  | takeFileName metaFile == "meta.json" = metaFile
  | otherwise = metaFile </> "meta.json"

loadMetaStub :: FilePath -> IO (Either String MetaStub)
loadMetaStub
  = eitherDecodeFileStrict' . normalizeMetaFilePath

-- given a path to its directory or meta.json, loads all pertaining data into a ClusterRun
loadClusterRun :: FilePath -> IO (Either String ClusterRun)
loadClusterRun metaFile_
  = do
    runMeta <- BS.readFile metaFile
    case eitherDecodeStrict' runMeta of
        Left e -> pure $ Left e
        Right metaStub -> do
          runBlockProp <- tryReadFile $ analysis </> "blockprop.json"
          runClusterPerf <- tryReadFile $ analysis </> "clusterperf.json"
          pure $! Right $! ClusterRun{..}
  `catch`
    \(SomeException e) -> pure (Left $ show e)
  where
    metaFile = normalizeMetaFilePath metaFile_
    analysis = takeDirectory metaFile </> "analysis"
    tryReadFile f
      = doesFileExist f >>= bool (pure Nothing) (Just <$> BS.readFile f)

getDBSettings :: DBCredentials -> IO DB.Settings
getDBSettings NoCreds
  = maybe BS.empty BS.pack <$> lookupEnv envVarDBURI
getDBSettings (PostgresURI uri)
  = pure $ BS.pack uri
getDBSettings DBCreds{..}
  = do
    envUser <- BS.pack <$> getLoginName
    envPass <- maybe BS.empty BS.pack <$> lookupEnv envVarDBPass
    pure $ DB.settings
      (maybe "localhost" BS.pack dbHost)
      (maybe 5432 fromIntegral dbPort)
      (maybe envUser BS.pack dbUser)
      (maybe envPass BS.pack dbPass)
      (BS.pack dbName)
