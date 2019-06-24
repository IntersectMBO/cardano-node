{-# LANGUAGE GADTs #-}

module CLI (
    -- * CLI
    CLI(..)
  , Command(..)
  , parseCLI
    --
  , SigningFilePath,      fromSigningPath
  , VerificationFilePath, fromVerificationPath
  ) where

import           Data.Foldable (asum)
import           Data.Semigroup ((<>))
import           Data.String (IsString)
import           Options.Applicative

import           NodeLib

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data CLI = CLI {
    command      :: Command
  }

data Command =
    DummyGenesis !FilePath
  | RTByronGenesis !FilePath
  | FullByronGenesis
  | KeyGen !SigningFilePath !VerificationFilePath

newtype SigningFilePath      = SigningFilePath      { fromSigningPath      :: FilePath }
newtype VerificationFilePath = VerificationFilePath { fromVerificationPath :: FilePath }

parseCLI :: Parser CLI
parseCLI = CLI
    <$> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
    command' "dummy-genesis" "Write out a dummy genesis into FILE." $
      DummyGenesis
      <$> parseFilePath "genesis-output" "Path of the genesis JSON output file."
  , command' "full-byron-genesis" "Generate a fully-parametrised Byron genesis from scratch." $
      pure FullByronGenesis
  , command' "rt-byron-genesis" "Roundtrip a Byron genesis JSON file." $
      RTByronGenesis
      <$> parseFilePath "genesis" "Path of the genesis input JSON file."
  , command' "keygen" "Generate a keypair." $
      KeyGen
      <$> (SigningFilePath      <$> parseFilePath "out-signing"      "Signing key output file path.")
      <*> (VerificationFilePath <$> parseFilePath "out-verification" "Verification key output file path.")
  ]

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname role =
    strOption (
            long optname
         <> metavar "FILEPATH"
         <> help role
    )
