{-# LANGUAGE ScopedTypeVariables #-}

module MonadicMain where

import qualified Cardano.Benchmarking.Script.Selftest as C (runSelftest, testScript)
import qualified Cardano.Benchmarking.Script.Types as C (Action (..), SubmitMode (..))
import           Control.Arrow ((|||))
import           Control.Monad (when)
import qualified Crypto.Hash as Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as ByteString (pack, readFile, unpack)
import qualified MonadicGen.Cardano.Benchmarking.Script.Selftest as M (runSelftest, testScript)
import qualified MonadicGen.Cardano.Benchmarking.Script.Types as M (Action (..), SubmitMode (..))
import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)
import qualified Paths_tx_generator as Paths (getDataFileName)
import           System.Exit (die, exitSuccess)
import qualified System.Posix.Process as Posix (ProcessStatus, forkProcess, getProcessStatus)
import qualified System.Posix.Types as Posix (ProcessID)

main :: IO ()
main = do
  putStrLn "Monadic Cleanup Testcase"
  protocolFile <-  Paths.getDataFileName "data/protocol-parameters.json"
  let currentActions :: [C.Action] = C.testScript protocolFile C.DiscardTX
      monadicActions :: [M.Action] = M.testScript protocolFile M.DiscardTX
  putStrLn "About to print monadicActions"
  print monadicActions
  putStrLn "About to run monadic selftest"
  runSelftest' M.runSelftest "/tmp/monadic.out"
  putStrLn $ "monadicActions: len " ++ show (length monadicActions)
  putStrLn $ "|" ++ replicate 78 '-' ++ "|"
  putStrLn "About to run current selftest"
  runSelftest' C.runSelftest "/tmp/current.out"
  putStrLn $ "currentActions: len " ++ show (length currentActions)
  putStrLn $ "|" ++ replicate 78 '-' ++ "|"
  let currentActDigest, monadicActDigest :: Hash.Digest Hash.SHA256
      currentActDigest = Hash.hash . ByteString.pack $ show currentActions
      monadicActDigest = Hash.hash . ByteString.pack $ show monadicActions
  putStrLn $ "currentActDigest = " ++ show currentActDigest
  putStrLn $ "monadicActDigest = " ++ show monadicActDigest
  when (show currentActions /= show monadicActions) $ die "Action strings differed!"
  when (currentActDigest /= monadicActDigest) $ die "Action digests differed!"
  putStrLn "Action sequences identical barring dump file names."
  currentTxFile <- ByteString.readFile "/tmp/current.out"
  monadicTxFile <- ByteString.readFile "/tmp/monadic.out"
  let currentTxDigest, monadicTxDigest :: Hash.Digest Hash.SHA256
      currentTxDigest = Hash.hash currentTxFile
      monadicTxDigest = Hash.hash monadicTxFile
  putStrLn $ "currentTxDigest = " ++ show currentTxDigest
  putStrLn $ "monadicTxDigest = " ++ show monadicTxDigest
  when (currentTxDigest /= monadicTxDigest) $ die "Tx digests differed!"
  putStrLn "Tx sequences identical barring dump file names."


runSelftest' :: Show a => (IOManager -> Maybe FilePath -> IO (Either a ()))
                                        -> FilePath -> IO ()
runSelftest' rst f =
  withIOManager $ \iocp -> rst iocp (Just f) >>= die . show ||| pure
