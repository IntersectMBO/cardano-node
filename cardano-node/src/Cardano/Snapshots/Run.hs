{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Snapshots.Run (
    canonicalizeSnapshots,
    NodeDatabasePaths,
) where

import qualified Cardano.Api.Consensus as Api
import Cardano.Node.Configuration.LedgerDB
import Cardano.Node.Configuration.POM
import Cardano.Node.Protocol
import Cardano.Node.Types (ConfigYamlFilePath (..))
import Control.Exception
import Control.Monad (forM_)
import Control.Monad.Except
import Data.Monoid (Last (..))
import Ouroboros.Consensus.Cardano.SnapshotConversion
import Ouroboros.Consensus.Node (NodeDatabasePaths (..), immutableDbPath)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

canonicalizeSnapshots :: FilePath -> Maybe NodeDatabasePaths -> IO ()
canonicalizeSnapshots cfg (Last -> db) = do
    configYamlPc <- parseNodeConfigurationFP $ Just $ ConfigYamlFilePath cfg

    let cfgFromFile = defaultPartialNodeConfiguration <> configYamlPc

        mOut = getLast (pncCanonicalSnapshotOutputPath cfgFromFile)

        mOtherConfigs = do
            a <- getLast (pncDatabaseFile cfgFromFile <> db)
            b <- getLast (pncLedgerDbConfig cfgFromFile)
            c <- getLast (pncProtocolConfig cfgFromFile)
            d <- getLast (pncProtocolFiles cfgFromFile)
            pure (a, b, c, d)

    case (mOut, mOtherConfigs) of
        (Nothing, _) -> pure ()
        (_, Nothing) -> error "Impossible, some arguments were missing yet there should be at least a default value for those"
        (Just out, Just (immutableDbPath -> dbPath, LedgerDbConfiguration _ _ _ selector _, pInfo, cfgFiles)) -> do
            snaps <- listDirectory (dbPath </> "ledger")
            someConsensusProto <-
                runThrowExceptT $
                    mkConsensusProtocol
                        pInfo
                        (Just cfgFiles)
            case someConsensusProto of
                SomeConsensusProtocol Api.CardanoBlockType pInfoArgs -> do
                    let inFmt = case selector of
                            V1LMDB{} -> LMDB
                            V2InMemory{} -> Mem
                            V2LSM Nothing -> flip LSM (dbPath </> "lsm")
                            V2LSM (Just lsmDb) -> flip LSM (dbPath </> lsmDb)
                    forM_ snaps $ \snap -> do
                        exists <- doesFileExist (out </> snap </> "meta")
                        if exists
                            then putStrLn $ "Snapshot at " <> dbPath </> "ledger" </> snap <> " already converted"
                            else do
                                putStrLn $ "Converting snapshot at " <> dbPath </> "ledger" </> snap
                                runThrowExceptT $ convertSnapshot False (fst $ Api.protocolInfo @IO pInfoArgs) (inFmt (dbPath </> "ledger" </> snap)) (Mem $ out </> snap)
                    putStrLn "Done"
                _ -> pure ()

runThrowExceptT :: (Exception e) => ExceptT e IO a -> IO a
runThrowExceptT act = runExceptT act >>= either throwIO pure
