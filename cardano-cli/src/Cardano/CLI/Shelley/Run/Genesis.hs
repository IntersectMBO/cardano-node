{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Genesis
  ( runGenesisCreate
  , runGenesisAddr
  , runGenesisTxIn
  ) where

import           Cardano.Prelude

import           Cardano.Api hiding (writeAddress)
--TODO: prefer versions from Cardano.Api where possible

import           Cardano.Config.Shelley.Address (ShelleyAddress)
import           Cardano.Config.Shelley.ColdKeys (KeyRole (..), OperatorKeyRole (..),
                    readVerKey)

import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Run.KeyGen (runGenesisKeyGenDelegate, runGenesisKeyGenGenesis,
                    runGenesisKeyGenUTxO)
import           Cardano.CLI.Shelley.Parsers (GenesisDir (..), OpCertCounterFile (..),
                    SigningKeyFile (..), VerificationKeyFile (..))
import           Cardano.Config.Shelley.Genesis

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isDigit)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Ouroboros.Consensus.Shelley.Node

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.TxData as Shelley

import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath ((</>), takeExtension)



runGenesisAddr :: VerificationKeyFile -> ExceptT CliError IO ()
runGenesisAddr (VerificationKeyFile vkeyPath) =
    firstExceptT KeyCliError $ do
      vkey <- readVerKey GenesisUTxOKey vkeyPath
      let addr = shelleyVerificationKeyAddress
                   (VerificationKeyShelley vkey) Mainnet
      liftIO $ Text.putStrLn $ addressToHex addr


runGenesisTxIn :: VerificationKeyFile -> ExceptT CliError IO ()
runGenesisTxIn (VerificationKeyFile vkeyPath) =
    firstExceptT KeyCliError $ do
      vkey <- readVerKey GenesisUTxOKey vkeyPath
      case shelleyVerificationKeyAddress (VerificationKeyShelley vkey) Mainnet of
        AddressShelley addr -> do let txin = fromShelleyTxIn (initialFundsPseudoTxIn addr)
                                  liftIO $ Text.putStrLn $ renderTxIn txin
        AddressByron _addr -> panic "Please supply only shelley addresses"
  where
    fromShelleyTxIn :: Shelley.TxIn TPraosStandardCrypto -> TxIn
    fromShelleyTxIn (Shelley.TxIn txid txix) =
        TxIn (fromShelleyTxId txid) (fromIntegral txix)

    fromShelleyTxId :: Shelley.TxId TPraosStandardCrypto -> TxId
    fromShelleyTxId (Shelley.TxId (Crypto.UnsafeHash h)) =
        TxId (Crypto.UnsafeHash h)


runGenesisCreate :: GenesisDir
                 -> Word  -- ^ num genesis & delegate keys to make
                 -> Word  -- ^ num utxo keys to make
                 -> Maybe SystemStart
                 -> Lovelace
                 -> ExceptT CliError IO ()
runGenesisCreate (GenesisDir rootdir)
                 genNumGenesisKeys genNumUTxOKeys
                 mStart amount = do
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart
  template <- readShelleyGenesis (rootdir </> "genesis.spec.json")

  liftIO $ do
    createDirectoryIfMissing False rootdir
    createDirectoryIfMissing False gendir
    createDirectoryIfMissing False deldir
    createDirectoryIfMissing False utxodir

  forM_ [ 1 .. genNumGenesisKeys ] $ \index -> do
    createGenesisKeys  gendir  index
    createDelegateKeys deldir index

  forM_ [ 1 .. genNumUTxOKeys ] $ \index ->
    createUtxoKeys utxodir index

  genDlgs <- readGenDelegsMap gendir deldir
  utxoAddrs <- readInitialFundAddresses utxodir

  let finalGenesis = updateTemplate start amount genDlgs utxoAddrs template

  writeShelleyGenesis (rootdir </> "genesis.json") finalGenesis
  where
    gendir  = rootdir </> "genesis-keys"
    deldir  = rootdir </> "delegate-keys"
    utxodir = rootdir </> "utxo-keys"

-- -------------------------------------------------------------------------------------------------

createDelegateKeys :: FilePath -> Word -> ExceptT CliError IO ()
createDelegateKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenDelegate
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".skey")
        (OpCertCounterFile $ dir </> "delegate-opcert" ++ strIndex ++ ".counter")

createGenesisKeys :: FilePath -> Word -> ExceptT CliError IO ()
createGenesisKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenGenesis
        (VerificationKeyFile $ dir </> "genesis" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "genesis" ++ strIndex ++ ".skey")


createUtxoKeys :: FilePath -> Word -> ExceptT CliError IO ()
createUtxoKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenUTxO
        (VerificationKeyFile $ dir </> "utxo" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "utxo" ++ strIndex ++ ".skey")


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


-- Local type aliases
type VerKey r              = Ledger.VKey r TPraosStandardCrypto
type VerKeyGenesis         = VerKey Ledger.Genesis
type VerKeyGenesisDelegate = VerKey Ledger.GenesisDelegate

type KeyHash r              = Ledger.KeyHash r TPraosStandardCrypto
type KeyHashGenesis         = KeyHash Ledger.Genesis
type KeyHashGenesisDelegate = KeyHash Ledger.GenesisDelegate


updateTemplate
    :: SystemStart -> Lovelace
    -> Map KeyHashGenesis KeyHashGenesisDelegate
    -> [ShelleyAddress]
    -> ShelleyGenesis TPraosStandardCrypto
    -> ShelleyGenesis TPraosStandardCrypto
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

-- -------------------------------------------------------------------------------------------------

readGenDelegsMap :: FilePath -> FilePath -> ExceptT CliError IO (Map KeyHashGenesis KeyHashGenesisDelegate)
readGenDelegsMap gendir deldir = do
    gkm <- readGenesisKeys gendir
    dkm <- readDelegateKeys deldir
    -- Both maps should have an identical set of keys.
    -- The mapMaybe will silently drop any map elements for which the key does not exist
    -- in both maps.
    pure $ Map.fromList (map (rearrange gkm dkm) $ Map.keys gkm)
  where
    rearrange :: Map Int (Ledger.VKey 'Ledger.Genesis TPraosStandardCrypto)
             -> Map Int (Ledger.VKey 'Ledger.GenesisDelegate TPraosStandardCrypto)
             -> Int
             -> (KeyHashGenesis, KeyHashGenesisDelegate)
    rearrange gkm dkm i =
      case (Map.lookup i gkm, Map.lookup i dkm) of
        (Just a, Just b) -> (Ledger.hashKey a, Ledger.hashKey b)
        _otherwise -> panic "readGenDelegsMap"


readGenesisKeys :: FilePath -> ExceptT CliError IO (Map Int VerKeyGenesis)
readGenesisKeys gendir = do
  files <- filter isVkey <$> liftIO (listDirectory gendir)
  fmap Map.fromList <$> traverse (readIndexedVerKey GenesisKey) $ map (gendir </>) files

readDelegateKeys :: FilePath -> ExceptT CliError IO (Map Int VerKeyGenesisDelegate)
readDelegateKeys deldir = do
  files <- filter isVkey <$> liftIO (listDirectory deldir)
  fmap Map.fromList <$> traverse (readIndexedVerKey (OperatorKey GenesisDelegateKey)) $ map (deldir </>) files


-- The file path is of the form "delegate-keys/delegate3.vkey".
-- This function reads the file and extracts the index (in this case 3).
readIndexedVerKey :: Typeable r
                   => KeyRole -> FilePath
                   -> ExceptT CliError IO (Int, VerKey r)
readIndexedVerKey role fpath =
   case extractIndex fpath of
     Nothing -> panic "readIndexedVerKey role fpath"
     Just i -> (i,) <$> firstExceptT KeyCliError (readVerKey role fpath)
  where
    extractIndex :: FilePath -> Maybe Int
    extractIndex fp =
      case filter isDigit fp of
        [] -> Nothing
        xs -> readMaybe xs

readInitialFundAddresses :: FilePath -> ExceptT CliError IO [ShelleyAddress]
readInitialFundAddresses utxodir = do
    files <- filter isVkey <$> liftIO (listDirectory utxodir)
    vkeys <- firstExceptT KeyCliError $
               traverse (readVerKey GenesisUTxOKey)
                        (map (utxodir </>) files)
    return [ addr | vkey <- vkeys
           , addr <- case shelleyVerificationKeyAddress (VerificationKeyShelley vkey) Mainnet of
                       AddressShelley addr' -> return addr'
                       _ -> panic "Please supply only shelley verification keys"
           ]
    --TODO: need to support testnets, not just Mainnet
    --TODO: need less insane version of shelleyVerificationKeyAddress with
    -- shelley-specific types

isVkey :: FilePath -> Bool
isVkey fp = takeExtension fp == ".vkey"
