{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Cardano.CLI.Environment (getEnvCli)
import qualified Cardano.Crypto.Init as Crypto
import           Cardano.Git.Rev (gitRev)
import           Cardano.TxSubmit (TxSubmitCommand (..), opts, runTxSubmitWebapi)

import qualified Data.Text as T
import           Data.Version (showVersion)
import qualified Options.Applicative as Opt
import           System.Info (arch, compilerName, compilerVersion, os)

import           Paths_cardano_submit_api (version)

main :: IO ()
main = do
  Crypto.cryptoInit

  envCli <- getEnvCli

  Opt.execParser (opts envCli) >>= \case
    TxSubmitRun params -> runTxSubmitWebapi params
    TxSubmitVersion -> do
      putStrLn $
        mconcat
          [ "cardano-submit-api "
          , showVersion version
          , " - "
          , os
          , "-"
          , arch
          , " - "
          , compilerName
          , "-"
          , showVersion compilerVersion
          , "\ngit rev "
          , T.unpack $(gitRev)
          ]
