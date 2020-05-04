{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Shelley.Run.Query
  ( runQueryCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api (queryPParamsFromLocalState)

import           Cardano.CLI.Ops (CliError (..), getLocalTip, withIOManagerE)
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))

import           Cardano.Common.LocalSocket (chooseSocketPath)

import           Cardano.Config.Protocol (mkConsensusProtocol)
import           Cardano.Config.Types (CLISocketPath, ConfigYamlFilePath, NodeConfiguration (..),
                     SomeConsensusProtocol (..), parseNodeConfigurationFP)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, left)

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Ouroboros.Consensus.Cardano (Protocol (..))

import           Ouroboros.Network.Block (getTipPoint)

import           Shelley.Spec.Ledger.PParams (PParams)

runQueryCmd :: QueryCmd -> ExceptT CliError IO ()
runQueryCmd (QueryProtocolParameters configFp mbSockPath outFile) =
  runQueryProtocolParameters configFp mbSockPath outFile
runQueryCmd cmd = liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd

runQueryProtocolParameters
  :: ConfigYamlFilePath
  -> Maybe CLISocketPath
  -> OutputFile
  -> ExceptT CliError IO ()
runQueryProtocolParameters configFp mbSockPath (OutputFile outFile) = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sockPath <- pure $ chooseSocketPath (ncSocketPath nc) mbSockPath
    SomeConsensusProtocol p <- firstExceptT ProtocolError $ mkConsensusProtocol nc Nothing
    case p of
      ptcl@ProtocolRealTPraos{} -> do
        tip <- withIOManagerE $ \iocp -> liftIO $ getLocalTip p iocp sockPath
        pparams <- firstExceptT NodeLocalStateQueryError $
          queryPParamsFromLocalState ptcl sockPath (getTipPoint tip)
        writeProtocolParameters outFile pparams
      _ -> left $ IncorrectProtocolSpecifiedError (ncProtocol nc)

writeProtocolParameters :: FilePath -> PParams -> ExceptT CliError IO ()
writeProtocolParameters fpath pparams =
  handleIOExceptT (IOError fpath) $ LBS.writeFile fpath (encodePretty pparams)
