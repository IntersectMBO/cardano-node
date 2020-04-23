{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
module Cardano.CLI.Shelley.Run.Genesis
  ( runGenesisCreate
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)

import           Cardano.Chain.Common (Lovelace, unsafeGetLovelace)
import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers (GenesisDir (..))
import           Cardano.Config.Shelley.Genesis

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           System.FilePath ((</>))

runGenesisCreate :: GenesisDir -> Maybe SystemStart -> Lovelace -> ExceptT CliError IO ()
runGenesisCreate (GenesisDir gendir) mStart amount = do
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart
  template <- readShelleyGenesis (gendir </> "genesis.spec.json")
  writeShelleyGenesis (gendir </> "genesis.json") (updateTemplate start amount template)

  liftIO . putStrLn $ "runGenesisCreate " ++ show (gendir, start, amount)



-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT CliError IO UTCTime
getCurrentTimePlus30 =
    plus30sec <$> liftIO getCurrentTime
  where
    plus30sec :: UTCTime -> UTCTime
    plus30sec = addUTCTime (30 :: NominalDiffTime)


readShelleyGenesis :: FilePath -> ExceptT CliError IO (ShelleyGenesis TPraosStandardCrypto)
readShelleyGenesis fpath = do
  lbs <- handleIOExceptT (IOError fpath) $ LBS.readFile fpath
  firstExceptT (AesonDecode fpath . Text.pack) . hoistEither $ Aeson.eitherDecode' lbs

updateTemplate :: SystemStart -> Lovelace -> ShelleyGenesis TPraosStandardCrypto -> ShelleyGenesis TPraosStandardCrypto
updateTemplate start amount template =
  template
    { sgStartTime = start
    , sgMaxLovelaceSupply = unsafeGetLovelace amount
    }

writeShelleyGenesis :: FilePath -> ShelleyGenesis TPraosStandardCrypto -> ExceptT CliError IO ()
writeShelleyGenesis fpath sg =
  handleIOExceptT (IOError fpath) $ LBS.writeFile fpath (encodePretty sg)
