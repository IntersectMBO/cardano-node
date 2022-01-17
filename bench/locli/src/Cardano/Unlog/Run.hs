{-# LANGUAGE ImportQualifiedPost #-}
-- | Dispatch for running all the CLI commands
module Cardano.Unlog.Run
  ( Command(..)
  , CommandErrors
  , renderCommandError
  , runCommand
  -- * Re-exports
  , gitRev
  ) where

import Cardano.Prelude

import Control.Monad.Trans.Except.Extra (firstExceptT)
import Data.Aeson                 qualified as AE
import Data.ByteString.Lazy.Char8 qualified as LBS

import Cardano.Analysis.Driver (AnalysisCmdError, renderAnalysisCmdError,
           runAnalysisCommand)
import Cardano.Unlog.Commands (AnalysisCommand)

import Cardano.Analysis.Version

-- | Sub-commands of 'locli'.
newtype Command =

  -- | Analysis commands
    AnalysisCommand AnalysisCommand
  deriving Show

data CommandErrors
  = AnalysisError AnalysisCommand AnalysisCmdError
  deriving Show

runCommand :: Command -> ExceptT CommandErrors IO ()
runCommand (AnalysisCommand c) = do
  liftIO $ LBS.putStrLn $ AE.encode getVersion
  firstExceptT (AnalysisError c) $ runAnalysisCommand c

renderCommandError :: CommandErrors -> Text
renderCommandError (AnalysisError cmd err) =
  renderAnalysisCmdError cmd err
