{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module MonadicMain where

import qualified Cardano.Api as Api (AsType (..), CardanoEra (..), IsCardanoEra (..), Tx (..))
import qualified Cardano.Benchmarking.Script.Action as C (action)
import qualified Cardano.Benchmarking.Script.Core as C (TxListElem (..))
import qualified Cardano.Benchmarking.Script.Env as C (emptyEnv, runActionM', setBenchTracers)
import qualified Cardano.Benchmarking.Script.Selftest as C (runSelftest, testScript)
import qualified Cardano.Benchmarking.Script.Types as C (Action (..), SubmitMode (..))
import qualified Cardano.Benchmarking.Tracer as C (initNullTracers)
import           Control.Arrow ((|||))
import           Control.Monad (when, zipWithM)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.RWS.Strict (runRWST)
import qualified Crypto.Hash as Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as ByteString (pack, readFile, unpack)
import qualified Data.List as List (and, intersperse)
import qualified MonadicGen.Cardano.Benchmarking.Script.Action as M (action)
import qualified MonadicGen.Cardano.Benchmarking.Script.Core as M (TxListElem (..))
import qualified MonadicGen.Cardano.Benchmarking.Script.Env as M (emptyEnv, runActionM',
                   setBenchTracers)
import qualified MonadicGen.Cardano.Benchmarking.Script.Selftest as M (runSelftest, testScript)
import qualified MonadicGen.Cardano.Benchmarking.Script.Types as M (Action (..), SubmitMode (..))
import qualified MonadicGen.Cardano.Benchmarking.Tracer as M (initNullTracers)
import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)
import qualified Paths_tx_generator as Paths (getDataFileName)
import           System.Exit (die, exitSuccess)
import qualified System.Posix.Process as Posix (ProcessStatus, forkProcess, getProcessStatus)
import qualified System.Posix.Types as Posix (ProcessID)

currentFile :: String = "/tmp/current.out"
monadicFile :: String = "/tmp/monadic.out"

main :: IO ()
main = do
  putStrLn "Monadic Cleanup Testcase"
  protocolFile <-  Paths.getDataFileName "data/protocol-parameters.json"
  let currentActions :: [C.Action]
          = C.testScript protocolFile $ C.DumpToFile currentFile
      monadicActions :: [M.Action]
          = M.testScript protocolFile $ M.DumpToFile monadicFile
  putStrLn "About to print monadicActions"
  print monadicActions
  putStrLn "About to print currentActions"
  print monadicActions
  putStrLn "About to run monadic selftest"
  runSelftest' M.runSelftest monadicFile
  putStrLn $ "monadicActions: len " ++ show (length monadicActions)
  putStrLn $ "|" ++ replicate 78 '-' ++ "|"
  putStrLn "About to run current selftest"
  runSelftest' C.runSelftest currentFile
  putStrLn $ "currentActions: len " ++ show (length currentActions)
  putStrLn $ "|" ++ replicate 78 '-' ++ "|"
  let currentActions' :: [C.Action]
      currentActions' = map ignoreFileC currentActions
      monadicActions' :: [M.Action]
      monadicActions' = map ignoreFileM monadicActions
      ignoreFileC :: C.Action -> C.Action
      ignoreFileC = \case
        act@(C.Submit era (C.DumpToFile _) params gen)
          -> C.Submit era C.DiscardTX params gen
        act -> act
      ignoreFileM :: M.Action -> M.Action
      ignoreFileM = \case
        act@(M.Submit era (M.DumpToFile _) params gen)
          -> M.Submit era M.DiscardTX params gen
        act -> act
      currentActDigest, monadicActDigest :: Hash.Digest Hash.SHA256
      currentActDigest = Hash.hash . ByteString.pack $ show currentActions'
      monadicActDigest = Hash.hash . ByteString.pack $ show monadicActions'
  putStrLn $ "currentActDigest = " ++ show currentActDigest
  putStrLn $ "monadicActDigest = " ++ show monadicActDigest
  when (show currentActions' /= show monadicActions')
    $ die "Action strings differed!"
  when (currentActDigest /= monadicActDigest)
    $ die "Action digests differed!"
  putStrLn "Action sequences identical barring dump file names."
  currentTxFile <- ByteString.readFile currentFile
  monadicTxFile <- ByteString.readFile monadicFile
  let currentTxDigest, monadicTxDigest :: Hash.Digest Hash.SHA256
      currentTxDigest = Hash.hash currentTxFile
      monadicTxDigest = Hash.hash monadicTxFile
  putStrLn $ "currentTxDigest = " ++ show currentTxDigest
  putStrLn $ "monadicTxDigest = " ++ show monadicTxDigest
  when (currentTxDigest /= monadicTxDigest)
    $ die "Tx digests differed!"
  putStrLn "Logged Tx sequences identical barring dump file names."
  (res , env , w ) <- do
    withIOManager
      $ \iocp ->
          flip (`runRWST` iocp) C.emptyEnv
            $ runExceptT
            $ do
              C.setBenchTracers C.initNullTracers
              mapM_ C.action currentActions
  (res', env', w') <- do
    withIOManager
      $ \iocp ->
          flip (`runRWST` iocp) M.emptyEnv
            $ runExceptT
            $ do
              M.setBenchTracers M.initNullTracers
              mapM_ M.action monadicActions
  putStrLn $ "|" ++ replicate 78 '-' ++ "|"
  let nrLines = 10
      nrTxs = 10
  putStrLn "current txs"
  mapM_ putStrLn . List.intersperse (replicate 80 '-')
                 . take nrTxs
                 $ map (take (nrLines * 80) . show) w'
  putStrLn $ "|" ++ replicate 78 '=' ++ "|"
  putStrLn "monadic txs"
  mapM_ putStrLn . List.intersperse (replicate 80 '-')
                 . take nrTxs
                 $ map (take (nrLines * 80) . show) w'
  putStrLn $ "|" ++ replicate 78 '-' ++ "|"
  checks <- zipWithM step w w'
  putStrLn $ "tx match checks "
                 ++ if List.and checks then "failed" else "succeeded" where
    step :: C.TxListElem -> M.TxListElem -> IO Bool
    C.TxListElem (tx :: Api.IsCardanoEra era => Api.Tx era)
            `step` M.TxListElem (tx' :: Api.IsCardanoEra era' => Api.Tx era')
      = case ((Api.cardanoEra :: Api.CardanoEra era, tx), (Api.cardanoEra :: Api.CardanoEra era', tx')) of
          ((Api.AllegraEra, t), (Api.AllegraEra, t'))
            | t == t'
            -> do
                  -- putStrLn . take 80 $ "Shelley match: " ++ show (tx, tx')
                  pure False
          ((e, t), (e', t')) -> do
            putStrLn "mismatch or unhandled eras: "
            putStrLn . take 80 $ "currentTx: " ++ show (e , t')
            putStrLn . take 80 $ "monadicTx: " ++ show (e', t')
            pure True

runSelftest' :: Show a => (IOManager -> Maybe FilePath -> IO (Either a ()))
                                        -> FilePath -> IO ()
runSelftest' rst f =
  withIOManager $ \iocp -> rst iocp (Just f) >>= die . show ||| pure
