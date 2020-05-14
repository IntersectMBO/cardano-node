{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Genesis
  ( ShelleyGenesisCmdError
  , runGenesisCmd
  ) where

import           Cardano.Prelude

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isDigit)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath ((</>), takeExtension)
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)

import           Cardano.Api hiding (writeAddress)
--TODO: prefer versions from Cardano.Api where possible
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Ouroboros.Consensus.Shelley.Node

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.TxData as Shelley

import           Cardano.Config.Shelley.Address (ShelleyAddress)
import           Cardano.Config.Shelley.ColdKeys (KeyRole (..), OperatorKeyRole (..),
                    readVerKey)
import           Cardano.Config.Shelley.Genesis
import           Cardano.Config.Shelley.ColdKeys
import           Cardano.Config.Shelley.OCert

import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.KeyGen (ShelleyKeyGenError, runColdKeyGen)

data ShelleyGenesisCmdError
  = ShelleyGenesisCmdKeyError !KeyError
  | ShelleyGenesisCmdIOError !FilePath !IOException
  | ShelleyGenesisCmdAesonDecode !FilePath !Text
  | ShelleyGenesisCmdOperationalCert !OperationalCertError
  | ShelleyGenesisCmdKeyGenError !ShelleyKeyGenError
  deriving Show

runGenesisCmd :: GenesisCmd -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCmd (GenesisKeyGenGenesis vk sk) = runGenesisKeyGenGenesis vk sk
runGenesisCmd (GenesisKeyGenDelegate vk sk ctr) = runGenesisKeyGenDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenUTxO vk sk) = runGenesisKeyGenUTxO vk sk
runGenesisCmd (GenesisKeyHash vk) = runGenesisKeyHash vk
runGenesisCmd (GenesisVerKey vk sk) = runGenesisVerKey vk sk
runGenesisCmd (GenesisTxIn vk) = runGenesisTxIn vk
runGenesisCmd (GenesisAddr vk) = runGenesisAddr vk
runGenesisCmd (GenesisCreate gd gn un ms am) = runGenesisCreate gd gn un ms am

--
-- Genesis command implementations
--

runGenesisKeyGenGenesis :: VerificationKeyFile -> SigningKeyFile
                        -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenGenesis vkf skf =
  firstExceptT ShelleyGenesisCmdKeyGenError $ runColdKeyGen GenesisKey vkf skf


runGenesisKeyGenDelegate :: VerificationKeyFile
                         -> SigningKeyFile
                         -> OpCertCounterFile
                         -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenDelegate vkeyPath skeyPath (OpCertCounterFile ocertCtrPath) = do
    firstExceptT ShelleyGenesisCmdKeyGenError $ runColdKeyGen (OperatorKey GenesisDelegateKey) vkeyPath skeyPath
    firstExceptT ShelleyGenesisCmdOperationalCert $
      writeOperationalCertIssueCounter ocertCtrPath initialCounter
  where
    initialCounter = 0


runGenesisKeyGenUTxO :: VerificationKeyFile -> SigningKeyFile
                     -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenUTxO vkf skf =
  firstExceptT ShelleyGenesisCmdKeyGenError $ runColdKeyGen GenesisUTxOKey vkf skf


runGenesisKeyHash :: VerificationKeyFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyHash (VerificationKeyFile vkeyPath) =
    firstExceptT ShelleyGenesisCmdKeyError $ do
      (vkey, _role) <- readVerKeySomeRole genesisKeyRoles vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey (vkey :: VerKey Ledger.Genesis)
      liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex khash


runGenesisVerKey :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisVerKey (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT ShelleyGenesisCmdKeyError $ do
      (skey, role) <- readSigningKeySomeRole genesisKeyRoles skeyPath
      let vkey :: VerKey Ledger.Genesis
          vkey = deriveVerKey skey
      writeVerKey role vkeyPath vkey


genesisKeyRoles :: [KeyRole]
genesisKeyRoles =
  [ GenesisKey
  , GenesisUTxOKey
  , OperatorKey GenesisDelegateKey
  ]


runGenesisTxIn :: VerificationKeyFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisTxIn (VerificationKeyFile vkeyPath) =
    firstExceptT ShelleyGenesisCmdKeyError $ do
      vkey <- readVerKey GenesisUTxOKey vkeyPath
      case shelleyVerificationKeyAddress (PaymentVerificationKeyShelley vkey) Nothing of
        AddressShelley addr -> do let txin = fromShelleyTxIn (initialFundsPseudoTxIn addr)
                                  liftIO $ Text.putStrLn $ renderTxIn txin
        AddressShelleyReward _rwdAcct -> panic "Please only supply shelley addresses. Reward accound found."
        AddressByron _addr -> panic "Please supply only shelley addresses"
  where
    fromShelleyTxIn :: Shelley.TxIn TPraosStandardCrypto -> TxIn
    fromShelleyTxIn (Shelley.TxIn txid txix) =
        TxIn (fromShelleyTxId txid) (fromIntegral txix)

    fromShelleyTxId :: Shelley.TxId TPraosStandardCrypto -> TxId
    fromShelleyTxId (Shelley.TxId (Crypto.UnsafeHash h)) =
        TxId (Crypto.UnsafeHash h)


runGenesisAddr :: VerificationKeyFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisAddr (VerificationKeyFile vkeyPath) =
    firstExceptT ShelleyGenesisCmdKeyError $ do
      vkey <- readVerKey GenesisUTxOKey vkeyPath
      let addr = shelleyVerificationKeyAddress
                   (PaymentVerificationKeyShelley vkey) Nothing
      liftIO $ Text.putStrLn $ addressToHex addr


--
-- Create Genesis command implementation
--

runGenesisCreate :: GenesisDir
                 -> Word  -- ^ num genesis & delegate keys to make
                 -> Word  -- ^ num utxo keys to make
                 -> Maybe SystemStart
                 -> Maybe Lovelace
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCreate (GenesisDir rootdir)
                 genNumGenesisKeys genNumUTxOKeys
                 mStart mAmount = do
  liftIO $ do
    createDirectoryIfMissing False rootdir
    createDirectoryIfMissing False gendir
    createDirectoryIfMissing False deldir
    createDirectoryIfMissing False utxodir

  template <- readShelleyGenesis (rootdir </> "genesis.spec.json")

  forM_ [ 1 .. genNumGenesisKeys ] $ \index -> do
    createGenesisKeys  gendir  index
    createDelegateKeys deldir index

  forM_ [ 1 .. genNumUTxOKeys ] $ \index ->
    createUtxoKeys utxodir index

  genDlgs <- readGenDelegsMap gendir deldir
  utxoAddrs <- readInitialFundAddresses utxodir
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

  let finalGenesis = updateTemplate start mAmount genDlgs utxoAddrs template

  writeShelleyGenesis (rootdir </> "genesis.json") finalGenesis
  where
    gendir  = rootdir </> "genesis-keys"
    deldir  = rootdir </> "delegate-keys"
    utxodir = rootdir </> "utxo-keys"

-- -------------------------------------------------------------------------------------------------

createDelegateKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createDelegateKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenDelegate
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".skey")
        (OpCertCounterFile $ dir </> "delegate-opcert" ++ strIndex ++ ".counter")

createGenesisKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createGenesisKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenGenesis
        (VerificationKeyFile $ dir </> "genesis" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "genesis" ++ strIndex ++ ".skey")


createUtxoKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createUtxoKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenUTxO
        (VerificationKeyFile $ dir </> "utxo" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "utxo" ++ strIndex ++ ".skey")


-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT ShelleyGenesisCmdError IO UTCTime
getCurrentTimePlus30 =
    plus30sec <$> liftIO getCurrentTime
  where
    plus30sec :: UTCTime -> UTCTime
    plus30sec = addUTCTime (30 :: NominalDiffTime)


readShelleyGenesis :: FilePath -> ExceptT ShelleyGenesisCmdError IO (ShelleyGenesis TPraosStandardCrypto)
readShelleyGenesis fpath = do
    readAndDecode
      `catchError` \err ->
        case err of
          ShelleyGenesisCmdIOError _ ioe
            | isDoesNotExistError ioe -> writeDefault
          _                           -> throwError err
  where
    readAndDecode = do
      lbs <- handleIOExceptT (ShelleyGenesisCmdIOError fpath) $ LBS.readFile fpath
      firstExceptT (ShelleyGenesisCmdAesonDecode fpath . Text.pack) . hoistEither $
        Aeson.eitherDecode' lbs

    defaults :: ShelleyGenesis TPraosStandardCrypto
    defaults = shelleyGenesisDefaults

    writeDefault = do
      handleIOExceptT (ShelleyGenesisCmdIOError fpath) $
        LBS.writeFile fpath (encodePretty defaults)
      return defaults


-- Local type aliases
type VerKeyGenesis         = VerKey Ledger.Genesis
type VerKeyGenesisDelegate = VerKey Ledger.GenesisDelegate

type KeyHash r              = Ledger.KeyHash r TPraosStandardCrypto
type KeyHashGenesis         = KeyHash Ledger.Genesis
type KeyHashGenesisDelegate = KeyHash Ledger.GenesisDelegate


updateTemplate
    :: SystemStart -> Maybe Lovelace
    -> Map KeyHashGenesis KeyHashGenesisDelegate
    -> [ShelleyAddress]
    -> ShelleyGenesis TPraosStandardCrypto
    -> ShelleyGenesis TPraosStandardCrypto
updateTemplate start mAmount delKeys utxoAddrs template =
    template
      { sgStartTime = start
      , sgMaxLovelaceSupply = fromIntegral totalCoin
      , sgGenDelegs = delKeys
      , sgInitialFunds = Map.fromList utxoList
      }
  where
    totalCoin :: Integer
    totalCoin = maybe (fromIntegral (sgMaxLovelaceSupply template))
                      unLoveLace mAmount

    eachAddrCoin :: Integer
    eachAddrCoin = totalCoin `div` fromIntegral (length utxoAddrs)

    utxoList :: [(ShelleyAddress, Coin)]
    utxoList = fst $ List.foldl' folder ([], totalCoin) utxoAddrs

    folder :: ([(ShelleyAddress, Coin)], Integer) -> ShelleyAddress -> ([(ShelleyAddress, Coin)], Integer)
    folder (acc, rest) addr
      | rest > eachAddrCoin + fromIntegral (length utxoAddrs) = ((addr, Coin eachAddrCoin) : acc, rest - eachAddrCoin)
      | otherwise = ((addr, Coin rest) : acc, 0)

writeShelleyGenesis :: FilePath -> ShelleyGenesis TPraosStandardCrypto -> ExceptT ShelleyGenesisCmdError IO ()
writeShelleyGenesis fpath sg =
  handleIOExceptT (ShelleyGenesisCmdIOError fpath) $ LBS.writeFile fpath (encodePretty sg)

-- -------------------------------------------------------------------------------------------------

readGenDelegsMap :: FilePath -> FilePath -> ExceptT ShelleyGenesisCmdError IO (Map KeyHashGenesis KeyHashGenesisDelegate)
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


readGenesisKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO (Map Int VerKeyGenesis)
readGenesisKeys gendir = do
  files <- filter isVkey <$> liftIO (listDirectory gendir)
  fmap Map.fromList <$> traverse (readIndexedVerKey GenesisKey) $ map (gendir </>) files

readDelegateKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO (Map Int VerKeyGenesisDelegate)
readDelegateKeys deldir = do
  files <- filter isVkey <$> liftIO (listDirectory deldir)
  fmap Map.fromList <$> traverse (readIndexedVerKey (OperatorKey GenesisDelegateKey)) $ map (deldir </>) files


-- The file path is of the form "delegate-keys/delegate3.vkey".
-- This function reads the file and extracts the index (in this case 3).
readIndexedVerKey :: Typeable r
                   => KeyRole -> FilePath
                   -> ExceptT ShelleyGenesisCmdError IO (Int, VerKey r)
readIndexedVerKey role fpath =
   case extractIndex fpath of
     Nothing -> panic "readIndexedVerKey role fpath"
     Just i -> (,) i <$> firstExceptT ShelleyGenesisCmdKeyError (readVerKey role fpath)
  where
    extractIndex :: FilePath -> Maybe Int
    extractIndex fp =
      case filter isDigit fp of
        [] -> Nothing
        xs -> readMaybe xs

readInitialFundAddresses :: FilePath -> ExceptT ShelleyGenesisCmdError IO [ShelleyAddress]
readInitialFundAddresses utxodir = do
    files <- filter isVkey <$> liftIO (listDirectory utxodir)
    vkeys <- firstExceptT ShelleyGenesisCmdKeyError $
               traverse (readVerKey GenesisUTxOKey)
                        (map (utxodir </>) files)
    return [ addr | vkey <- vkeys
           , addr <- case shelleyVerificationKeyAddress (PaymentVerificationKeyShelley vkey) Nothing of
                       AddressShelley addr' -> return addr'
                       _ -> panic "Please supply only shelley verification keys"
           ]
    --TODO: need to support testnets, not just Mainnet
    --TODO: need less insane version of shelleyVerificationKeyAddress with
    -- shelley-specific types

isVkey :: FilePath -> Bool
isVkey fp = takeExtension fp == ".vkey"
