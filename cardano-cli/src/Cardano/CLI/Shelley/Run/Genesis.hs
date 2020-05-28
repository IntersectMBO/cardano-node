{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Genesis
  ( ShelleyGenesisCmdError
  , renderShelleyGenesisCmdError
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
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT,
                   hoistEither, left)

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

import           Cardano.CLI.Helpers (textToByteString)
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.KeyGen (ShelleyKeyGenError,
                   renderShelleyKeyGenError, runColdKeyGen)

data ShelleyGenesisCmdError
  = ShelleyGenesisCmdGenesisKeyGenError !ShelleyKeyGenError
  | ShelleyGenesisCmdGenesisUTxOKeyError !KeyError
  | ShelleyGenesisCmdOnlyShelleyAddresses
  | ShelleyGenesisCmdOnlyShelleyAddressesNotReward
  | ShelleyGenesisCmdOperatorKeyGenError !ShelleyKeyGenError
  | ShelleyGenesisCmdReadGenesisAesonDecodeError !FilePath !Text
  | ShelleyGenesisCmdReadGenesisIOError !FilePath !IOException
  | ShelleyGenesisCmdReadGenesisUTxOVerKeyError !KeyError
  | ShelleyGenesisCmdReadIndexedVerKeyError !KeyError
  | ShelleyGenesisCmdReadSignKeyError !KeyError
  | ShelleyGenesisCmdReadVerKeyError !KeyError
  | ShelleyGenesisCmdUTxOKeyGenError !ShelleyKeyGenError
  | ShelleyGenesisCmdWriteDefaultGenesisIOError !FilePath !IOException
  | ShelleyGenesisCmdWriteGenesisIOError !FilePath !IOException
  | ShelleyGenesisCmdWriteOperationalCertError !FilePath !OperationalCertError
  deriving Show

renderShelleyGenesisCmdError :: ShelleyGenesisCmdError -> Text
renderShelleyGenesisCmdError err =
  case err of
    ShelleyGenesisCmdUTxOKeyGenError keyErr ->
      "Error while generating the genesis UTxO keys:" <> renderShelleyKeyGenError keyErr
    ShelleyGenesisCmdReadVerKeyError keyErr ->
      "Error while reading genesis verification key: " <> renderKeyError keyErr
    ShelleyGenesisCmdReadSignKeyError keyErr ->
      "Error while reading the genesis signing key: " <> renderKeyError keyErr
    ShelleyGenesisCmdReadGenesisUTxOVerKeyError keyErr ->
      "Error while reading the genesis UTxO verification key: " <> renderKeyError keyErr
    ShelleyGenesisCmdOnlyShelleyAddressesNotReward ->
      "Please only supply Shelley addresses. Reward account found."
    ShelleyGenesisCmdOnlyShelleyAddresses ->
      "Please supply only shelley addresses."
    ShelleyGenesisCmdOperatorKeyGenError keyErr ->
      "Error generatoring genesis operational key: " <> renderShelleyKeyGenError keyErr
    ShelleyGenesisCmdReadIndexedVerKeyError keyErr ->
      "Error reading indexed verification key: " <> renderKeyError keyErr
    ShelleyGenesisCmdGenesisUTxOKeyError keyErr ->
      "Error reading genesis UTxO key: " <> renderKeyError keyErr
    ShelleyGenesisCmdReadGenesisAesonDecodeError fp decErr ->
      "Error while decoding Shelley genesis at: " <> textShow fp <> " Error: " <> textShow decErr
    ShelleyGenesisCmdReadGenesisIOError fp ioException ->
      "Error while reading Shelley genesis at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyGenesisCmdWriteDefaultGenesisIOError fp ioException ->
      "Error while writing default genesis at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyGenesisCmdWriteGenesisIOError fp ioException ->
      "Error while writing Shelley genesis at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyGenesisCmdWriteOperationalCertError fp opCertErr ->
      "Error while writing Shelley genesis operational certificate at: "
         <> textShow fp
         <> " Error: "
         <> renderOperationalCertError opCertErr
    ShelleyGenesisCmdGenesisKeyGenError keyGenErr ->
      "Error generating the genesis keys: " <> renderShelleyKeyGenError keyGenErr


runGenesisCmd :: GenesisCmd -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCmd (GenesisKeyGenGenesis vk sk) = runGenesisKeyGenGenesis vk sk
runGenesisCmd (GenesisKeyGenDelegate vk sk ctr) = runGenesisKeyGenDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenUTxO vk sk) = runGenesisKeyGenUTxO vk sk
runGenesisCmd (GenesisKeyHash vk) = runGenesisKeyHash vk
runGenesisCmd (GenesisVerKey vk sk) = runGenesisVerKey vk sk
runGenesisCmd (GenesisTxIn vk nw mOutFile) = runGenesisTxIn vk nw mOutFile
runGenesisCmd (GenesisAddr vk nw mOutFile) = runGenesisAddr vk nw mOutFile
runGenesisCmd (GenesisCreate gd gn un ms am nw) = runGenesisCreate gd gn un ms am nw

--
-- Genesis command implementations
--

runGenesisKeyGenGenesis :: VerificationKeyFile -> SigningKeyFile
                        -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenGenesis vkf skf =
  firstExceptT ShelleyGenesisCmdGenesisKeyGenError $ runColdKeyGen GenesisKey vkf skf


runGenesisKeyGenDelegate :: VerificationKeyFile
                         -> SigningKeyFile
                         -> OpCertCounterFile
                         -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenDelegate vkeyPath skeyPath (OpCertCounterFile ocertCtrPath) = do
    firstExceptT ShelleyGenesisCmdOperatorKeyGenError $ runColdKeyGen (OperatorKey GenesisDelegateKey) vkeyPath skeyPath
    firstExceptT (ShelleyGenesisCmdWriteOperationalCertError ocertCtrPath) $
      writeOperationalCertIssueCounter ocertCtrPath initialCounter
  where
    initialCounter = 0


runGenesisKeyGenUTxO :: VerificationKeyFile -> SigningKeyFile
                     -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenUTxO vkf skf =
  firstExceptT ShelleyGenesisCmdUTxOKeyGenError $ runColdKeyGen GenesisUTxOKey vkf skf


runGenesisKeyHash :: VerificationKeyFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyHash (VerificationKeyFile vkeyPath) =
    firstExceptT ShelleyGenesisCmdReadVerKeyError $ do
      (vkey, _role) <- readVerKeySomeRole genesisKeyRoles vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey (vkey :: VerKey Ledger.Genesis)
      liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex khash


runGenesisVerKey :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisVerKey (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT ShelleyGenesisCmdReadSignKeyError $ do
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


runGenesisTxIn :: VerificationKeyFile -> Network -> Maybe OutputFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisTxIn (VerificationKeyFile vkeyPath) network mOutFile = do
      vkey <- firstExceptT ShelleyGenesisCmdReadGenesisUTxOVerKeyError $ readVerKey GenesisUTxOKey vkeyPath
      case shelleyVerificationKeyAddress network (PaymentVerificationKeyShelley vkey) Nothing of
        AddressShelley addr -> do let txin = renderTxIn $ fromShelleyTxIn (initialFundsPseudoTxIn addr)
                                  case mOutFile of
                                    Just (OutputFile fpath) -> liftIO . BS.writeFile fpath $ textToByteString txin
                                    Nothing -> liftIO $ Text.putStrLn txin
        AddressShelleyReward _rwdAcct -> left ShelleyGenesisCmdOnlyShelleyAddressesNotReward
        AddressByron _addr -> left ShelleyGenesisCmdOnlyShelleyAddresses
  where
    fromShelleyTxIn :: Shelley.TxIn TPraosStandardCrypto -> TxIn
    fromShelleyTxIn (Shelley.TxIn txid txix) =
        TxIn (fromShelleyTxId txid) (fromIntegral txix)

    fromShelleyTxId :: Shelley.TxId TPraosStandardCrypto -> TxId
    fromShelleyTxId (Shelley.TxId (Crypto.UnsafeHash h)) =
        TxId (Crypto.UnsafeHash h)


runGenesisAddr :: VerificationKeyFile -> Network -> Maybe OutputFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisAddr (VerificationKeyFile vkeyPath) network mOutFile =
    firstExceptT ShelleyGenesisCmdReadGenesisUTxOVerKeyError $ do
      vkey <- readVerKey GenesisUTxOKey vkeyPath
      let addr = shelleyVerificationKeyAddress network
                   (PaymentVerificationKeyShelley vkey) Nothing
          hexAddr = addressToHex addr
      case mOutFile of
        Just (OutputFile fpath) -> liftIO . BS.writeFile fpath $ textToByteString hexAddr
        Nothing -> liftIO $ Text.putStrLn hexAddr


--
-- Create Genesis command implementation
--

runGenesisCreate :: GenesisDir
                 -> Word  -- ^ num genesis & delegate keys to make
                 -> Word  -- ^ num utxo keys to make
                 -> Maybe SystemStart
                 -> Maybe Lovelace
                 -> Network
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCreate (GenesisDir rootdir)
                 genNumGenesisKeys genNumUTxOKeys
                 mStart mAmount network = do
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
  utxoAddrs <- readInitialFundAddresses utxodir network
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
          ShelleyGenesisCmdReadGenesisIOError _ ioe
            | isDoesNotExistError ioe -> writeDefault
          _                           -> left err
  where
    readAndDecode = do
      lbs <- handleIOExceptT (ShelleyGenesisCmdReadGenesisIOError fpath) $ LBS.readFile fpath
      firstExceptT (ShelleyGenesisCmdReadGenesisAesonDecodeError fpath . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs

    defaults :: ShelleyGenesis TPraosStandardCrypto
    defaults = shelleyGenesisDefaults

    writeDefault = do
      handleIOExceptT (ShelleyGenesisCmdWriteDefaultGenesisIOError fpath) $
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
      { sgSystemStart = start
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
  handleIOExceptT (ShelleyGenesisCmdWriteGenesisIOError fpath) $ LBS.writeFile fpath (encodePretty sg)

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
     Just i -> (,) i <$> firstExceptT ShelleyGenesisCmdReadIndexedVerKeyError (readVerKey role fpath)
  where
    extractIndex :: FilePath -> Maybe Int
    extractIndex fp =
      case filter isDigit fp of
        [] -> Nothing
        xs -> readMaybe xs

readInitialFundAddresses :: FilePath -> Network -> ExceptT ShelleyGenesisCmdError IO [ShelleyAddress]
readInitialFundAddresses utxodir nw = do
    files <- filter isVkey <$> liftIO (listDirectory utxodir)
    vkeys <- firstExceptT ShelleyGenesisCmdGenesisUTxOKeyError $
               traverse (readVerKey GenesisUTxOKey)
                        (map (utxodir </>) files)
    return [ addr | vkey <- vkeys
           , addr <- case shelleyVerificationKeyAddress nw (PaymentVerificationKeyShelley vkey) Nothing of
                       AddressShelley addr' -> return addr'
                       _ -> panic "Please supply only shelley verification keys"
           ]
    --TODO: need to support testnets, not just Mainnet
    --TODO: need less insane version of shelleyVerificationKeyAddress with
    -- shelley-specific types

isVkey :: FilePath -> Bool
isVkey fp = takeExtension fp == ".vkey"
