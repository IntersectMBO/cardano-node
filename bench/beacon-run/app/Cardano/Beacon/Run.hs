{-# LANGUAGE OverloadedStrings #-}

module  Cardano.Beacon.Run
        ( ConsoleStyle(..)
        , RunEnvironment(..)

        , envBeaconDir
        , envEchoing
        , envEmpty

        , shellCurlGitHubAPI
        , shellNixBuildVersion
        ) where

import           Control.Exception (SomeException (..), try)
import           Control.Monad (void, when)
import           Data.ByteString.Char8 as BSC (ByteString, pack, readFile, unpack, writeFile)
import           Data.Maybe (fromJust)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath ((</>))
import           System.Process hiding (env)

import           Cardano.Beacon.Chain (Chains)
import           Cardano.Beacon.CLI (BeaconOptions (..))
import           Cardano.Beacon.Console
import           Cardano.Beacon.Types


data RunEnvironment = Env
  { runChains     :: Maybe Chains
  , runCommit     :: Maybe CommitInfo
  , runInstall    :: Maybe InstallInfo
  , runOptions    :: BeaconOptions
  }

envEchoing :: RunEnvironment -> EchoCommand
envEchoing = optEchoing . runOptions

envBeaconDir :: RunEnvironment -> FilePath
envBeaconDir = optBeaconDir . runOptions

envEmpty :: BeaconOptions -> RunEnvironment
envEmpty = Env Nothing Nothing Nothing



runShellEchoing :: EchoCommand -> String -> [String] -> IO String
runShellEchoing echo cmd args = do
  when (echo == EchoCommand) $
    printStyled StyleEcho asOneLine
  try (readCreateProcess (shell asOneLine) "") >>= either
    (\(SomeException e) -> printFatalAndDie (show e))
    pure
  where
    asOneLine = concat [cmd, " ", unwords args]

-- cf. https://docs.github.com/en/rest/commits/commits?apiVersion=2022-11-28#get-a-commit
shellCurlGitHubAPI :: RunEnvironment -> FilePath -> String -> IO ByteString
shellCurlGitHubAPI env outFile queryPath = do
  _ <- runShellEchoing (envEchoing env) "curl" curlArgs
  BSC.readFile outFile
  where
    curlArgs =
      [ "-s -L"
      , "-H \"Accept: application/vnd.github+json\""
      , "-H \"X-GitHub-Api-Version: 2022-11-28\""
      , "-o", outFile
      , "https://api.github.com" ++ queryPath
      ]

postProcessJSON :: IO ()
postProcessJSON = do
  postProc <- pack <$> runShellEchoing EchoCommand "jq" jqArgs
  BSC.writeFile fname postProc
  where
    fname = "dummy.json"
    jqArgs =
      [ "-M"
      , "'map(inputs)'"
      , fname
      ]

shellNixBuildVersion :: RunEnvironment -> Version -> IO InstallInfo
shellNixBuildVersion env ver@Version{verCompiler = compiler} = do
  exists <- doesFileExist install
  if exists
    then printStyled StyleInfo "target binary already built and linked"
    else do
      createDirectoryIfMissing False binDir
      void $ runShellEchoing echoing "nix" nixBuildArgs

  nix_path <- runShellEchoing echoing "readlink" [output]
  return $ InstallInfo install (head . lines $ nix_path) ver
  where
    echoing = envEchoing env
    sha     = ciCommitSHA1 . fromJust . runCommit $ env
    binName = take 9 sha ++ "-" ++ compiler
    binDir  = envBeaconDir env </> "bin"
    output  = binDir </> binName
    install = output </> "bin" </> "db-analyser"

    nixBuildArgs =
      [ "build"
      , "github:IntersectMBO/ouroboros-consensus/"
          ++ sha
          ++ "#hydraJobs.x86_64-linux.native."
          ++ compiler
          ++ ".exesNoAsserts.ouroboros-consensus-cardano.db-analyser"
      , "-o", output
      ]
