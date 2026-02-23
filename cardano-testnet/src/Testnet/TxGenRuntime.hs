{-# LANGUAGE EmptyDataDeriving #-}
module Testnet.TxGenRuntime (startTxGenRuntime) where

import           System.FilePath ((</>))
import           System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, ProcessHandle)

import           Testnet.Filepath (TmpAbsolutePath, makeLogDir)
import           Testnet.Process.RunIO (liftIOAnnotated, procFlex)

import           RIO (IOMode (WriteMode), RIO, finally, hClose, openFile, throwString)
import           RIO.Directory (createDirectoryIfMissing)

-- | Start the tx-generator runtime, create file handles.
startTxGenRuntime
  -- :: HasCallStack
  :: TmpAbsolutePath
  -- ^ The temporary absolute path.
  -> [String]
  -- ^ The list of arguments for the tx-generator command.
  -> RIO env ProcessHandle
startTxGenRuntime tp args = do
  let logDir = makeLogDir tp
      txGenLogDir = logDir </> "tx-generator"

  createDirectoryIfMissing True txGenLogDir

  let txGenStdoutFile = txGenLogDir </> "stdout.log"
      txGenStderrFile = txGenLogDir </> "stderr.log"
      txGenPidFile = txGenLogDir </> "tx-generator.pid"

  hTxGenStdout <- openFile txGenStdoutFile WriteMode
  hTxGenStderr <- openFile txGenStderrFile WriteMode

  (do
    txGenCmd <- procFlex "tx-generator" "TX_GENERATOR" args
    (_, _, _, hProcess) <- liftIOAnnotated $ createProcess $ txGenCmd { std_in = CreatePipe, std_out = UseHandle hTxGenStdout, std_err = UseHandle hTxGenStderr }

    -- We then log the pid in the temp dir structure.
    pid <- maybe (throwString "Failed to get PID") return =<< liftIOAnnotated (getPid hProcess)
    liftIOAnnotated $ writeFile txGenPidFile $ show pid

    return hProcess
    ) `finally` do
      hClose hTxGenStdout
      hClose hTxGenStderr

