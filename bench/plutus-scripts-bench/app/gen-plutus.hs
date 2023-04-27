{-# LANGUAGE RecordWildCards #-}

import Cardano.Benchmarking.PlutusScripts (findPlutusScript, encodePlutusScript)
import qualified Data.ByteString.Lazy as LBS (hPut)
import Options.Applicative
import System.Exit (die)
import System.IO (IOMode(..), openFile, stdout)

data Options = Options
   { optOut :: Maybe FilePath
   , optMod :: String
   } deriving (Eq, Read, Show)

opts :: Parser Options
opts = Options
       <$>
        optional (strOption
                   (  long "output"
                   <> short 'o'
                   <> metavar "FILE"
                   <> help "Write output to FILE"
                   )
                 )
       <*>
        strArgument
        (  metavar "SCRIPT"
        <> help "Write SCRIPT to output"
        )

main :: IO ()
main =
  do
    Options {..} <- execParser (info opts fullDesc)
    s <- case findPlutusScript optMod of
          Just s  -> pure s
          Nothing -> die $ "unable to find plutus script for `" ++ optMod ++ "'"
    h <- case optOut of
          Just file -> openFile file WriteMode
          Nothing   -> pure stdout
    LBS.hPut h $ encodePlutusScript s
