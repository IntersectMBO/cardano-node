{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Cardano.Node.GitRevFromGit (
      gitRevFromGit
    ) where

import           Cardano.Prelude hiding (handleJust)
import           Prelude (String)

import           Control.Exception.Safe (handleJust)
import qualified Language.Haskell.TH as TH
import           System.Exit (ExitCode (..))
import           System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import           System.Process (readProcessWithExitCode)

-- | Git revision found by running git rev-parse. If git could not be
-- executed, then this will be an empty string.
gitRevFromGit :: TH.Q TH.Exp
gitRevFromGit = TH.LitE . TH.StringL <$> TH.runIO runGitRevParse
    where
        runGitRevParse :: IO String
        runGitRevParse = handleJust missingGit (const $ pure "") $ do
            (exitCode, output, _) <-
                readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
            pure $ case exitCode of
                ExitSuccess -> output
                _           -> ""

        missingGit e = if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing
