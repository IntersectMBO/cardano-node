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
import           System.FilePath ((</>), takeExtension, takeExtensions)
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT,
                   hoistEither, left, newExceptT)

import           Cardano.Api hiding (writeAddress)
import           Cardano.Api.Typed (AsType (..), Error (..), FileError, Hash (..),
                   OperationalCertificateIssueCounter (..), TextEnvelopeError,
                   VerificationKey, VrfKey, generateSigningKey, getVerificationKey,
                   readFileTextEnvelope, verificationKeyHash, writeFileTextEnvelope)

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Ouroboros.Consensus.Shelley.Node

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Genesis as Ledger
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.TxData as Shelley

import           Cardano.Config.Shelley.Address (ShelleyAddress)
import           Cardano.Config.Shelley.ColdKeys (KeyRole (..), OperatorKeyRole (..),
                    readVerKey)
import           Cardano.Config.Shelley.Genesis
import           Cardano.Config.Shelley.ColdKeys
import           Cardano.Config.Shelley.OCert
import           Cardano.Config.Shelley.VRF
import           Cardano.Config.TextView (TextViewTitle (..))

import           Cardano.CLI.Helpers (textToByteString)
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.KeyGen (ShelleyKeyGenError,
                   renderShelleyKeyGenError)

data ShelleyGenesisCmdError
  = ShelleyGenesisCmdGenesisKeyGenError !ShelleyKeyGenError
  | ShelleyGenesisCmdGenesisUTxOKeyError !KeyError
  | ShelleyGenesisCmdOnlyShelleyAddresses
  | ShelleyGenesisCmdOnlyShelleyAddressesNotReward
  | ShelleyGenesisCmdOperatorKeyGenError !ShelleyKeyGenError
  | ShelleyGenesisCmdOperatorVrfKeyGenError !VRFError
  | ShelleyGenesisCmdReadGenesisAesonDecodeError !FilePath !Text
  | ShelleyGenesisCmdReadGenesisIOError !FilePath !IOException
  | ShelleyGenesisCmdReadGenesisUTxOVerKeyError !KeyError
  | ShelleyGenesisCmdReadIndexedVerKeyError !KeyError
  | ShelleyGenesisCmdReadIndexedVrfKeyError !(FileError TextEnvelopeError)
  | ShelleyGenesisCmdReadSignKeyError !KeyError
  | ShelleyGenesisCmdReadVerKeyError !KeyError
  | ShelleyGenesisCmdUTxOKeyGenError !ShelleyKeyGenError
  | ShelleyGenesisCmdWriteDefaultGenesisIOError !FilePath !IOException
  | ShelleyGenesisCmdWriteGenesisIOError !FilePath !IOException
  | ShelleyGenesisCmdWriteOperationalCertError !FilePath !OperationalCertError
  | ShelleyGenesisMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | ShelleyGenesisFilesNoIndex [FilePath]
  | ShelleyGenesisFilesDupIndex [FilePath]
  | ShelleyGenesisCmdWriteFileError !(FileError ())
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
      "Error generating genesis operational key: " <> renderShelleyKeyGenError keyErr
    ShelleyGenesisCmdOperatorVrfKeyGenError keyErr ->
      "Error generating genesis delegate VRF key: " <> renderVRFError keyErr
    ShelleyGenesisCmdReadIndexedVerKeyError keyErr ->
      "Error reading indexed verification key: " <> renderKeyError keyErr
    ShelleyGenesisCmdReadIndexedVrfKeyError fileErr ->
      "Error reading indexed VRF verification key: " <> Text.pack (displayError fileErr)
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
    ShelleyGenesisMismatchedGenesisKeyFiles gfiles dfiles vfiles ->
      "Mismatch between the files found:\n"
        <> "Genesis key file indexes:      " <> textShow gfiles
        <> "delegate key file indexes:     " <> textShow dfiles
        <> "delegate VRF key file indexes: " <> textShow vfiles
    ShelleyGenesisFilesNoIndex files ->
      "The genesis keys files are expected to have a numeric index but these do not:\n"
        <> Text.unlines (map Text.pack files)
    ShelleyGenesisFilesDupIndex files ->
      "The genesis keys files are expected to have a unique numeric index but these do not:\n"
        <> Text.unlines (map Text.pack files)
    ShelleyGenesisCmdWriteFileError fileErr ->
      Text.pack (displayError fileErr)


runGenesisCmd :: GenesisCmd -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCmd (GenesisKeyGenGenesis vk sk) = runGenesisKeyGenGenesis vk sk
runGenesisCmd (GenesisKeyGenDelegate vk sk ctr) = runGenesisKeyGenDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenUTxO vk sk) = runGenesisKeyGenUTxO vk sk
runGenesisCmd (GenesisCmdKeyHash vk) = runGenesisKeyHash vk
runGenesisCmd (GenesisVerKey vk sk) = runGenesisVerKey vk sk
runGenesisCmd (GenesisTxIn vk nw mOutFile) = runGenesisTxIn vk nw mOutFile
runGenesisCmd (GenesisAddr vk nw mOutFile) = runGenesisAddr vk nw mOutFile
runGenesisCmd (GenesisCreate gd gn un ms am nw) = runGenesisCreate gd gn un ms am nw

--
-- Genesis command implementations
--

runGenesisKeyGenGenesis :: VerificationKeyFile -> SigningKeyFile
                        -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenGenesis (VerificationKeyFile vkeyPath)
                        (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewTitle
    skeyDesc = TextViewTitle "Genesis Signing Key"
    vkeyDesc = TextViewTitle "Genesis Verification Key"


runGenesisKeyGenDelegate :: VerificationKeyFile
                         -> SigningKeyFile
                         -> OpCertCounterFile
                         -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenDelegate (VerificationKeyFile vkeyPath)
                         (SigningKeyFile skeyPath)
                         (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisDelegateKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter
          initialCounter
          -- TODO: Commenting this out as we're temporarily supporting the old op
          -- cert issue counter format.
          -- (castVerificationKey vkey)  -- Cast to a 'StakePoolKey'
  where
    skeyDesc, vkeyDesc, ocertCtrDesc :: TextViewTitle
    skeyDesc = TextViewTitle "Genesis delegate operator key"
    vkeyDesc = TextViewTitle "Genesis delegate operator key"
    ocertCtrDesc = TextViewTitle $ "Next certificate issue number: " <> show initialCounter

    initialCounter :: Natural
    initialCounter = 0


runGenesisKeyGenDelegateVRF :: VerificationKeyFile -> SigningKeyFile
                            -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenDelegateVRF (VerificationKeyFile vkeyPath)
                            (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewTitle
    skeyDesc = TextViewTitle "VRF Signing Key"
    vkeyDesc = TextViewTitle "VRF Verification Key"


runGenesisKeyGenUTxO :: VerificationKeyFile -> SigningKeyFile
                     -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenUTxO (VerificationKeyFile vkeyPath)
                     (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisUTxOKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewTitle
    skeyDesc = TextViewTitle "Genesis Initial UTxO Signing Key"
    vkeyDesc = TextViewTitle "Genesis Initial UTxO Verification Key"


runGenesisKeyHash :: VerificationKeyFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyHash (VerificationKeyFile vkeyPath) =
    firstExceptT ShelleyGenesisCmdReadVerKeyError $ do
      (vkey, _role) <- readVerKeySomeRole genesisKeyRoles vkeyPath
      let Ledger.KeyHash khash = Ledger.hashKey (vkey :: VerKeyGenesis)
      liftIO $ BS.putStrLn $ Crypto.getHashBytesAsHex khash


runGenesisVerKey :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisVerKey (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    firstExceptT ShelleyGenesisCmdReadSignKeyError $ do
      (skey, role) <- readSigningKeySomeRole genesisKeyRoles skeyPath
      let vkey :: VerKeyGenesis
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
        AddressShelley addr -> do let txin = renderTxIn
                                           . fromShelleyTxIn
                                           . Ledger.initialFundsPseudoTxIn
                                           $ addr
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
        (OpCertCounterFile $ dir </> "delegate" ++ strIndex ++ ".counter")
  runGenesisKeyGenDelegateVRF
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vrf.vkey")
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".vrf.skey")

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
type VerKeyGenesis         = Ledger.VKey Ledger.Genesis TPraosStandardCrypto
type VerKeyGenesisDelegate = Ledger.VKey Ledger.GenesisDelegate TPraosStandardCrypto

type KeyHashGenesis         = Ledger.KeyHash Ledger.Genesis TPraosStandardCrypto
type KeyHashGenesisDelegate = Ledger.KeyHash Ledger.GenesisDelegate TPraosStandardCrypto

updateTemplate
    :: SystemStart
    -> Maybe Lovelace
    -> Map KeyHashGenesis (KeyHashGenesisDelegate, Hash VrfKey)
    -> [ShelleyAddress]
    -> ShelleyGenesis TPraosStandardCrypto
    -> ShelleyGenesis TPraosStandardCrypto
updateTemplate (SystemStart start) mAmount delKeys utxoAddrs template =
    template
      { sgSystemStart = start
      , sgMaxLovelaceSupply = fromIntegral totalCoin
      , sgGenDelegs = shelleyDelKeys
      , sgInitialFunds = Map.fromList utxoList
      }
  where
    shelleyDelKeys = map (\(gd, VrfKeyHash h) -> Ledger.GenDelegPair gd h) delKeys

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

readGenDelegsMap :: FilePath -> FilePath
                 -> ExceptT ShelleyGenesisCmdError IO
                           (Map KeyHashGenesis (KeyHashGenesisDelegate, Hash VrfKey))
readGenDelegsMap gendir deldir = do
    gkm <- readGenesisKeys gendir
    dkm <- readDelegateKeys deldir
    vkm <- readDelegateVrfKeys deldir

    let combinedMap :: Map Int (VerKeyGenesis, (VerKeyGenesisDelegate, VerificationKey VrfKey))
        combinedMap =
          Map.intersectionWith (,)
            gkm
            (Map.intersectionWith (,)
               dkm vkm)

    -- All the maps should have an identical set of keys. Complain if not.
    let gkmExtra = gkm Map.\\ combinedMap
        dkmExtra = dkm Map.\\ combinedMap
        vkmExtra = vkm Map.\\ combinedMap
    unless (Map.null gkmExtra && Map.null dkmExtra && Map.null vkmExtra) $
      throwError $ ShelleyGenesisMismatchedGenesisKeyFiles
                     (Map.keys gkm) (Map.keys dkm) (Map.keys vkm)

    let delegsMap :: Map KeyHashGenesis (KeyHashGenesisDelegate, Hash VrfKey)
        delegsMap =
          Map.fromList [ (gh, (dh, vh))
                       | (g,(d,v)) <- Map.elems combinedMap
                       , let gh = Ledger.hashKey g
                             dh = Ledger.hashKey d
                             vh = verificationKeyHash v
                       ]

    pure delegsMap


readGenesisKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO
                                       (Map Int VerKeyGenesis)
readGenesisKeys gendir = do
  files <- liftIO (listDirectory gendir)
  fileIxs <- extractFileNameIndexes [ gendir </> file
                                    | file <- files
                                    , takeExtension file == ".vkey" ]
  firstExceptT ShelleyGenesisCmdReadIndexedVerKeyError $
    Map.fromList <$>
      sequence
        [ (,) ix <$>  readVerKey GenesisKey file
        | (file, ix) <- fileIxs ]


readDelegateKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO
                                        (Map Int VerKeyGenesisDelegate)
readDelegateKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <- extractFileNameIndexes [ deldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vkey" ]
  firstExceptT ShelleyGenesisCmdReadIndexedVerKeyError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readVerKey (OperatorKey GenesisDelegateKey) file
        | (file, ix) <- fileIxs ]

readDelegateVrfKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO
                                           (Map Int (VerificationKey VrfKey))
readDelegateVrfKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <- extractFileNameIndexes [ deldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vrf.vkey" ]
  firstExceptT ShelleyGenesisCmdReadIndexedVrfKeyError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> newExceptT (readFileTextEnvelope (AsVerificationKey AsVrfKey) file)
        | (file, ix) <- fileIxs ]


-- | The file path is of the form @"delegate-keys/delegate3.vkey"@.
-- This function reads the file and extracts the index (in this case 3).
--
extractFileNameIndex :: FilePath -> Maybe Int
extractFileNameIndex fp =
  case filter isDigit fp of
    [] -> Nothing
    xs -> readMaybe xs

extractFileNameIndexes :: [FilePath]
                       -> ExceptT ShelleyGenesisCmdError IO [(FilePath, Int)]
extractFileNameIndexes files = do
    case [ file | (file, Nothing) <- filesIxs ] of
      []     -> return ()
      files' -> throwError (ShelleyGenesisFilesNoIndex files')
    case filter (\g -> length g > 1)
       . groupBy ((==) `on` snd)
       . sortBy (compare `on` snd)
       $ [ (file, ix) | (file, Just ix) <- filesIxs ] of
      [] -> return ()
      (g:_) -> throwError (ShelleyGenesisFilesDupIndex (map fst g))

    return [ (file, ix) | (file, Just ix) <- filesIxs ]
  where
    filesIxs = [ (file, extractFileNameIndex file) | file <- files ]

readInitialFundAddresses :: FilePath -> Network -> ExceptT ShelleyGenesisCmdError IO [ShelleyAddress]
readInitialFundAddresses utxodir nw = do
    files <- liftIO (listDirectory utxodir)
    vkeys <- firstExceptT ShelleyGenesisCmdGenesisUTxOKeyError $
               sequence
                 [ readVerKey GenesisUTxOKey (utxodir </> file)
                 | file <- files
                 , takeExtension file == ".vkey" ]
    return [ addr | vkey <- vkeys
           , addr <- case shelleyVerificationKeyAddress nw (PaymentVerificationKeyShelley vkey) Nothing of
                       AddressShelley addr' -> return addr'
                       _ -> panic "Please supply only shelley verification keys"
           ]
    --TODO: need to support testnets, not just Mainnet
    --TODO: need less insane version of shelleyVerificationKeyAddress with
    -- shelley-specific types
