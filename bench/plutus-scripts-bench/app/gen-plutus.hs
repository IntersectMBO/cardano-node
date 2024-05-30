{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import           Cardano.Benchmarking.PlutusScripts

import qualified Data.ByteString.Lazy as LBS (hPut)
import           Data.List (sort)
import           Options.Applicative
import           System.Exit (die)
import           System.IO (IOMode (..), openFile, stdout, hClose)

data Options =
     List
   | Print
      { optOut :: Maybe FilePath
      , optMod :: String
      } deriving (Eq, Show)

opts :: Parser Options
opts =
  subparser $ listParser <> printParser
  where
    listParser = command "list" $ info (p <**> helper) $ progDesc "list available scripts"
      where p = pure List

    printParser = command "print" $ info (p <**> helper) $ progDesc "serialize script"
      where
        p = Print
              <$> optional (strOption
                (  long "output"
                <> short 'o'
                <> metavar "FILE"
                <> help "Write output to FILE"
                ))
              <*> strArgument (metavar "SCRIPT" <> help "Write SCRIPT to output")

pref :: ParserPrefs
pref = prefs showHelpOnEmpty

main :: IO ()
main = customExecParser pref (info opts fullDesc) >>= \case
  List -> mapM_ putStrLn (sort listPlutusScripts)
  Print{..} -> do
    s <- case findPlutusScript optMod of
          Just s  -> pure s
          Nothing -> die $ "unable to find plutus script for `" ++ optMod ++ "'"
    h <- case optOut of
          Just file -> openFile file WriteMode
          Nothing   -> pure stdout
    LBS.hPut h $ encodePlutusScript s
    hClose h
