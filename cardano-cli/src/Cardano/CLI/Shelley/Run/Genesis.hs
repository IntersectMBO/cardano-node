{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module Cardano.CLI.Shelley.Run.Genesis
  ( runGenesisCreate
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Cardano.Api hiding (writeAddress)
--TODO: prefer versions from Cardano.Api where possible

import           Cardano.Config.Shelley.Address (AddressRole (..), ShelleyAddress,
                    genBootstrapAddress, writeAddress)
import           Cardano.Config.Shelley.ColdKeys (KeyError, KeyRole (..), OperatorKeyRole (..),
                    readVerKey)
import           Cardano.Config.Shelley.Genesis (ShelleyGenesisError (..))

import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Run.KeyGen (runGenesisKeyGenDelegate, runGenesisKeyGenGenesis)
import           Cardano.CLI.Shelley.Parsers (GenesisDir (..), OpCertCounterFile (..),
                    SigningKeyFile (..), VerificationKeyFile (..))
import           Cardano.Config.Shelley.Genesis

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT,
                   hoistEither, left, right)

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys (GenKeyHash, KeyHash)
import qualified Shelley.Spec.Ledger.Keys as Ledger

import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath ((</>), takeFileName, takeExtension)

runGenesisCreate :: GenesisDir -> Word -> Maybe SystemStart -> Lovelace -> ExceptT CliError IO ()
runGenesisCreate (GenesisDir gendir) count mStart amount = do
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart
  template <- readShelleyGenesis (gendir </> "genesis.spec.json")

  utxoAddrs <- forM [ 1 .. count ] $ createRequiredKeys gendir
  genDlgs <- readGenDelegsMap gendir

  writeShelleyGenesis (gendir </> "genesis.json") (updateTemplate start amount genDlgs utxoAddrs template)

  liftIO $ putTextLn "Genesis file, genesis delegates and genesis distribution created."

-- -------------------------------------------------------------------------------------------------

-- Represents the filepath's basename of a particular key (genesis key, delegate keys, etc)
newtype BaseName
  = BaseName String
  deriving (Eq, Ord)

textBaseName :: BaseName -> Text
textBaseName (BaseName a) = Text.pack a

createRequiredKeys :: FilePath -> Word -> ExceptT CliError IO ShelleyAddress
createRequiredKeys gendir index = do
  createDelegateKeys (gendir </> "delegate-keys") index
  createGenesisKeys (gendir </> "genesis-keys") index
  createUtxoAddresses (gendir </> "utxo-keys") index

createDelegateKeys :: FilePath -> Word -> ExceptT CliError IO ()
createDelegateKeys dir index = do
  liftIO $ createDirectoryIfMissing True dir
  let strIndex = show index
  runGenesisKeyGenDelegate
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".skey")
        (OpCertCounterFile $ dir </> "delegate-opcert" ++ strIndex ++ ".counter")

createGenesisKeys :: FilePath -> Word -> ExceptT CliError IO ()
createGenesisKeys dir index = do
  liftIO $ createDirectoryIfMissing True dir
  let strIndex = show index
  runGenesisKeyGenGenesis
        (VerificationKeyFile $ dir </> "genesis" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "genesis" ++ strIndex ++ ".skey")


createUtxoAddresses :: FilePath -> Word -> ExceptT CliError IO ShelleyAddress
createUtxoAddresses dir index = do
  liftIO $ createDirectoryIfMissing True dir
  addr <- liftIO genBootstrapAddress
  let strIndex = show index
  firstExceptT AddressCliError $
    writeAddress BootstrapAddr (dir </> "address" ++ strIndex ++ ".vkey") addr
  pure addr


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

updateTemplate
    :: SystemStart -> Lovelace
    -> (Map (GenKeyHash TPraosStandardCrypto) (KeyHash TPraosStandardCrypto)) -> [ShelleyAddress]
    -> ShelleyGenesis TPraosStandardCrypto -> ShelleyGenesis TPraosStandardCrypto
updateTemplate start amount delKeys utxoAddrs template =
    template
      { sgStartTime = start
      , sgMaxLovelaceSupply = fromIntegral totalCoin
      , sgGenDelegs = delKeys
      , sgInitialFunds = Map.fromList utxoList
      }
  where
    totalCoin :: Integer
    totalCoin = unLoveLace amount

    eachAddrCoin :: Integer
    eachAddrCoin = totalCoin `div` fromIntegral (length utxoAddrs)

    utxoList :: [(ShelleyAddress, Coin)]
    utxoList = fst $ List.foldl' folder ([], totalCoin) utxoAddrs

    folder :: ([(ShelleyAddress, Coin)], Integer) -> ShelleyAddress -> ([(ShelleyAddress, Coin)], Integer)
    folder (acc, rest) addr
      | rest > eachAddrCoin + fromIntegral (length utxoAddrs) = ((addr, Coin eachAddrCoin) : acc, rest - eachAddrCoin)
      | otherwise = ((addr, Coin rest) : acc, 0)

writeShelleyGenesis :: FilePath -> ShelleyGenesis TPraosStandardCrypto -> ExceptT CliError IO ()
writeShelleyGenesis fpath sg =
  handleIOExceptT (IOError fpath) $ LBS.writeFile fpath (encodePretty sg)

readGenDelegsMap :: FilePath -> ExceptT CliError IO (Map (GenKeyHash TPraosStandardCrypto) (KeyHash TPraosStandardCrypto))
readGenDelegsMap gendir = do
    gkm <- firstExceptT KeyCliError $ readGenesisKeys (gendir </> "genesis-keys")
    dkm <- firstExceptT KeyCliError $ readDelegateKeys (gendir </> "delegate-keys")

    -- Both maps should have an identical set of keys (as in Map keys)
    -- because we should have generated an equal amount of genesis keys
    -- and delegate keys.
    let genesiskeyBaseNames = Map.keys gkm
        delegatekeyBaseNames = Map.keys dkm
        eitherDelegationKeyPairs = [ combine gkm dkm gBn dBn
                                   | gBn <- genesiskeyBaseNames
                                   , dBn <- delegatekeyBaseNames
                                   ]
    case partitionEithers eitherDelegationKeyPairs of
      ([], xs) -> right $ Map.fromList xs
      (errors, _) -> left $ ShelleyGenesisError (MultipleMissingKeys errors)

  where
    combine :: Map BaseName (Ledger.VKey TPraosStandardCrypto)
            -- ^ Genesis Keys
            -> Map BaseName (Ledger.VKey TPraosStandardCrypto)
            -- ^ Delegate Keys
            -> BaseName
            -- ^ Genesis Key basename
            -> BaseName
            -- ^ Delegate Key basename
            -> Either ShelleyGenesisError (GenKeyHash TPraosStandardCrypto, KeyHash TPraosStandardCrypto)
    combine gkm dkm gBn dBn =
      case (Map.lookup gBn gkm, Map.lookup dBn dkm) of
        (Just (Ledger.VKey a), Just b) -> Right (Ledger.hashKey (Ledger.VKeyGenesis a), Ledger.hashKey b)
        (Nothing, Just _) -> Left $ MissingGenesisKey (textBaseName gBn)
        (Just _, Nothing) -> Left $ MissingDelegateKey (textBaseName dBn)
        _ -> Left $ MissingGenesisAndDelegationKey (textBaseName gBn) (textBaseName dBn)

readGenesisKeys :: FilePath -> ExceptT KeyError IO (Map BaseName (Ledger.VKey TPraosStandardCrypto))
readGenesisKeys gendir = do
  files <- filter isVkey <$> liftIO (listDirectory gendir)
  fmap Map.fromList <$> traverse (readBaseNameVerKey GenesisKey) $ map (gendir </>) files

readDelegateKeys :: FilePath -> ExceptT KeyError IO (Map BaseName (Ledger.VKey TPraosStandardCrypto))
readDelegateKeys deldir = do
  files <- filter isVkey <$> liftIO (listDirectory deldir)
  fmap Map.fromList <$> traverse (readBaseNameVerKey (OperatorKey GenesisDelegateKey)) $ map (deldir </>) files

readBaseNameVerKey :: KeyRole -> FilePath -> ExceptT KeyError IO (BaseName, Ledger.VKey TPraosStandardCrypto)
readBaseNameVerKey role fpath =
  (BaseName (takeFileName fpath),) <$> readVerKey role fpath

isVkey :: FilePath -> Bool
isVkey fp = takeExtension fp == ".vkey"
