{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Helpers
  ( CertificateFile(..)
  , GenesisFile(..)
  , NewCertificateFile(..)
  , NewDirectory(..)
  , NewSigningKeyFile(..)
  , NewTxFile(..)
  , NewVerificationKeyFile(..)
  , SigningKeyFile(..)
  , TxFile(..)
  , VerificationKeyFile(..)
  , certificateValidation
  , dumpGenesis
  , eitherThrow
  , ensureNewFile
  , ensureNewFileLBS
  , ensureNewFileText
  , genesisUTxOTxIn
  , prettyAddress
  , prettySigningKeyPub
  , readPassword
  , readSigningKey
  , readVerificationKey
  , signTxId
  , txSpendUTxOByronPBFT
  , txSpendGenesisUTxOByronPBFT
  , withRealPBFT
  , writeSecrets
  ) where

import           Cardano.Prelude
import           Prelude (String, error)

import qualified Data.ByteArray as BA
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.String (IsString, fromString)
import           Data.Text (pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V
import qualified Formatting as F
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions, createDirectory, doesPathExist)
#endif
import           System.FilePath ((</>))
import           System.IO (hGetLine, hSetEcho, hFlush, stdout, stdin)
import           Text.Printf (printf)

import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB, GenTx, mkByronTx)
import           Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig)

import           Cardano.Binary (Annotated(..), reAnnotate, serialize')
import           Cardano.Chain.Common
import qualified Cardano.Chain.Common as CC.Common
import           Cardano.Chain.Delegation hiding (Map, epoch)
import           Cardano.Chain.Genesis
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Chain.UTxO
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Common.Protocol (SomeProtocol(..), fromProtocol)
import           Cardano.Config.CommonCLI (CommonCLI, mkConfiguration)
import           Cardano.Config.Partial (PartialCardanoConfiguration (..))
import           Cardano.Config.Types (CardanoConfiguration(..))
import           Cardano.CLI.Ops
import           Cardano.Crypto (SigningKey (..), ProtocolMagicId)
import qualified Cardano.Crypto as Crypto
import           Ouroboros.Consensus.Demo.Run (RunDemo)
import qualified Ouroboros.Consensus.Protocol as Consensus

newtype CertificateFile =
    CertificateFile { ceFp :: FilePath }
    deriving (Eq, Ord, Show, IsString)

newtype GenesisFile =
  GenesisFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewCertificateFile =
    NewCertificateFile { nFp :: FilePath }
    deriving (Eq, Ord, Show, IsString)

newtype NewDirectory =
  NewDirectory FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewSigningKeyFile =
  NewSigningKeyFile { nSkFp :: FilePath}
  deriving (Eq, Ord, Show, IsString)

newtype NewTxFile =
  NewTxFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewVerificationKeyFile =
  NewVerificationKeyFile FilePath
   deriving (Eq, Ord, Show, IsString)

newtype SigningKeyFile =
  SigningKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype TxFile =
  TxFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype VerificationKeyFile =
  VerificationKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)




-------------------------------------------------------------------------------
-- Supporting functions
-------------------------------------------------------------------------------

certificateValidation
  :: ProtocolMagicId
  -> ACertificate a
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey
  -> CertificateFile
  -> IO ()
certificateValidation magic cert delegateVK' issuerVK' certFp =
  let magic' = Annotated magic (serialize' magic)
      epoch  = unAnnotated $ aEpoch cert
      cert'  = cert { aEpoch = Annotated epoch (serialize' epoch) }
      vk    :: forall r. F.Format r (Crypto.VerificationKey -> r)
      vk     = Crypto.fullVerificationKeyF
      f     :: forall a. F.Format Text a -> a
      f      = F.sformat
      issues =
        [ f("Certificate does not have a valid signature.")
        | not (isValid magic' cert') ] <>

        [ f("Certificate issuer ".vk." doesn't match expected: ".vk)
          (issuerVK   cert)   issuerVK'
        |  issuerVK   cert /= issuerVK' ] <>

        [ f("Certificate delegate ".vk." doesn't match expected: ".vk)
          (delegateVK cert)   delegateVK'
        |  delegateVK cert /= delegateVK' ]
   in unless (null issues) $ throwIO $ CertificateValidationErrors (ceFp certFp) issues

dumpGenesis :: CLIOps IO -> FilePath -> GenesisData -> GeneratedSecrets -> IO ()
dumpGenesis CLIOps{..} outDir genesisData GeneratedSecrets{..} = do
  exists <- doesPathExist outDir
  if exists
    then throwIO $ OutputMustNotAlreadyExist outDir
    else createDirectory outDir

  let genesisJSONFile = outDir <> "/genesis.json"
  LB.writeFile genesisJSONFile =<< coSerialiseGenesis genesisData

  let dlgCertMap = unGenesisDelegation $ gdHeavyDelegation genesisData
      isCertForSK :: SigningKey -> Certificate -> Bool
      isCertForSK sk UnsafeACertificate{..} = delegateVK == Crypto.toVerification sk
      findDelegateCert :: SigningKey -> IO Certificate
      findDelegateCert sk =
        case flip find (Map.elems dlgCertMap) . isCertForSK $ sk of
          Nothing -> throwIO $ NoGenesisDelegationForKey $ prettySigningKeyPub sk
          Just x  -> pure x
      wOut :: String -> String -> (a -> IO LB.ByteString) -> [a] -> IO ()
      wOut = writeSecrets outDir
  dlgCerts <- mapM findDelegateCert gsRichSecrets

  wOut "genesis-keys"    "key"  coSerialiseGenesisKey     gsDlgIssuersSecrets
  wOut "delegate-keys"   "key"  coSerialiseDelegateKey    gsRichSecrets
  wOut "poor-keys"       "key"  coSerialisePoorKey        gsPoorSecrets
  wOut "delegation-cert" "json" coSerialiseDelegationCert dlgCerts
  wOut "avvm-seed"       "seed" (pure . LB.fromStrict)     gsFakeAvvmSeeds

eitherThrow :: (e -> CliError) -> Either e a ->  IO a
eitherThrow masker (Left e) = throwIO $ masker e
eitherThrow _ (Right x) = pure x

-- TODO:  we'd be better served by a combination of a temporary file
--        with an atomic rename.

-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
ensureNewFile writer outFile blob = do
  exists <- doesPathExist outFile
  when exists $
    throwIO $ OutputMustNotAlreadyExist outFile
  writer outFile blob

ensureNewFileLBS :: FilePath -> LB.ByteString -> IO ()
ensureNewFileLBS = ensureNewFile LB.writeFile

ensureNewFileText :: FilePath -> TL.Text -> IO ()
ensureNewFileText = ensureNewFile TL.writeFile

-- | Given a genesis, and a pair of signing key and address, reconstruct a TxIn
--   corresponding to the genesis UTxO entry.
genesisUTxOTxIn :: CC.Genesis.Config -> SigningKey -> Address -> CC.UTxO.TxIn
genesisUTxOTxIn gc genSk genAddr =
  let vk         = Crypto.toVerification genSk
      handleMissingAddr :: Maybe CC.UTxO.TxIn -> CC.UTxO.TxIn
      handleMissingAddr  = fromMaybe . error
        $  "\nGenesis UTxO has no address\n"
        <> (unpack $ prettyAddress genAddr)
        <> "\n\nIt has the following, though:\n\n"
        <> Cardano.Prelude.concat (unpack . prettyAddress <$> Map.keys initialUtxo)

      initialUtxo :: Map Address (CC.UTxO.TxIn, CC.UTxO.TxOut)
      initialUtxo =
            Map.fromList
          . mapMaybe (\(inp, out) -> mkEntry inp genAddr <$> keyMatchesUTxO vk out)
          . fromCompactTxInTxOutList
          . Map.toList
          . CC.UTxO.unUTxO
          . CC.UTxO.genesisUtxo
          $ gc
        where
          mkEntry :: CC.UTxO.TxIn
                  -> Address
                  -> CC.UTxO.TxOut
                  -> (Address, (CC.UTxO.TxIn, CC.UTxO.TxOut))
          mkEntry inp addr out = (addr, (inp, out))

      keyMatchesUTxO :: Crypto.VerificationKey -> CC.UTxO.TxOut -> Maybe CC.UTxO.TxOut
      keyMatchesUTxO key out =
        if CC.Common.checkVerKeyAddress key (CC.UTxO.txOutAddress out)
        then Just out else Nothing

      fromCompactTxInTxOutList :: [(CC.UTxO.CompactTxIn, CC.UTxO.CompactTxOut)]
                               -> [(CC.UTxO.TxIn, CC.UTxO.TxOut)]
      fromCompactTxInTxOutList =
          map (bimap CC.UTxO.fromCompactTxIn CC.UTxO.fromCompactTxOut)
  in handleMissingAddr $ fst <$> Map.lookup genAddr initialUtxo

prettyAddress :: CC.Common.Address -> Text
prettyAddress addr = TL.toStrict
  $  F.format CC.Common.addressF         addr <> "\n"
  <> F.format CC.Common.addressDetailedF addr

prettySigningKeyPub :: SigningKey -> Text
prettySigningKeyPub sKey  =
  let vk = Crypto.toVerification sKey
  in TL.toStrict
    $  "public key hash: " <> (F.format Crypto.hashHexF . CC.Common.addressHash $ vk) <> "\n"
    <> "     public key: " <> (Builder.toLazyText . Crypto.formatFullVerificationKey $ vk)

-- TODO:  we need to support password-protected secrets.
readSigningKey :: CLIOps IO -> SigningKeyFile -> IO SigningKey
readSigningKey co (SigningKeyFile fp) =
  coDeserialiseDelegateKey co fp =<< LB.readFile fp

-- TODO:  we'd be better served by a combination of a temporary file
--        with an atomic rename.

readVerificationKey :: VerificationKeyFile -> IO Crypto.VerificationKey
readVerificationKey (VerificationKeyFile fp) = do
  vkB <- SB.readFile fp
  case Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB of
    Left e -> throwIO . VerificationKeyDeserialisationFailed fp $ pack $ show e
    Right x -> pure x


txSpendGenesisUTxOByronPBFT :: CC.Genesis.Config -> SigningKey -> Address -> NonEmpty TxOut -> GenTx (ByronBlockOrEBB ByronConfig)
txSpendGenesisUTxOByronPBFT gc sk genRichAddr outs =
  let vk         = Crypto.toVerification sk
      txattrs    = mkAttributes ()
      tx         = UnsafeTx (pure txIn) outs txattrs
      txIn      :: CC.UTxO.TxIn
      txIn       = handleMissingAddr $ fst <$> Map.lookup genRichAddr initialUtxo
      handleMissingAddr :: Maybe CC.UTxO.TxIn -> CC.UTxO.TxIn
      handleMissingAddr  = fromMaybe . error
        $  "\nGenesis richmen UTxO has no address\n"
        <> (unpack $ prettyAddress genRichAddr)
        <> "\n\nIt has the following, though:\n\n"
        <> Cardano.Prelude.concat (unpack . prettyAddress <$> Map.keys initialUtxo)

      -- UTxO in the genesis block for the rich men
      initialUtxo :: Map Address (CC.UTxO.TxIn, CC.UTxO.TxOut)
      initialUtxo =
            Map.fromList
          . mapMaybe (\(inp, out) -> mkEntry inp genRichAddr <$> keyMatchesUTxO vk out)
          . fromCompactTxInTxOutList
          . Map.toList
          . CC.UTxO.unUTxO
          . CC.UTxO.genesisUtxo
          $ gc
        where
          mkEntry :: CC.UTxO.TxIn
                  -> Address
                  -> CC.UTxO.TxOut
                  -> (Address, (CC.UTxO.TxIn, CC.UTxO.TxOut))
          mkEntry inp addr out = (addr, (inp, out))

      keyMatchesUTxO :: Crypto.VerificationKey -> CC.UTxO.TxOut -> Maybe CC.UTxO.TxOut
      keyMatchesUTxO key out =
        if CC.Common.checkVerKeyAddress key (CC.UTxO.txOutAddress out)
        then Just out else Nothing

      fromCompactTxInTxOutList :: [(CC.UTxO.CompactTxIn, CC.UTxO.CompactTxOut)]
                               -> [(CC.UTxO.TxIn, CC.UTxO.TxOut)]
      fromCompactTxInTxOutList =
          map (bimap CC.UTxO.fromCompactTxIn CC.UTxO.fromCompactTxOut)
      cheat     :: CC.UTxO.TxWitness
      cheat      = V.fromList [
        CC.UTxO.VKWitness
            vk
            (Crypto.sign
              (configProtocolMagicId gc)
              Crypto.SignTx
              sk
              -- Below, we have to cheat to spend a genesis UTxO entry:
              -- sign ourselves, not the input Tx.
              -- Ledger knows.
              (CC.UTxO.TxSigData (Crypto.hash tx))
              )
        ]
      ATxAux atx awit = mkTxAux tx cheat
  in mkByronTx $ ATxAux (reAnnotate atx) (reAnnotate awit)

readPassword :: String -> IO Crypto.PassPhrase
readPassword prompt = do
  let readOne :: String -> IO String
      readOne pr = do
        hPutStr stdout pr >> hFlush stdout
        hSetEcho stdout False
        pp <- hGetLine stdin
        hSetEcho stdout True
        hPutStrLn stdout ("" :: String)
        pure pp
      loop = do
        (v1, v2) <- (,) <$> readOne prompt <*> readOne "Repeat to validate: "
        if v1 == v2
          then pure v1
          else hPutStrLn stdout ("Sorry, entered passwords don't match." :: String)
               >> loop
  Crypto.PassPhrase . BA.convert . UTF8.fromString <$> loop

signTxId :: ProtocolMagicId -> SigningKey -> TxId -> CC.UTxO.TxInWitness
signTxId pmid sk txid = CC.UTxO.VKWitness
  (Crypto.toVerification sk)
  (Crypto.sign
    pmid
    Crypto.SignTx
    sk
    (CC.UTxO.TxSigData txid))

txSpendUTxOByronPBFT
  :: CC.Genesis.Config
  -> SigningKey
  -> NonEmpty TxIn
  -> NonEmpty TxOut
  -> GenTx (ByronBlockOrEBB ByronConfig)
txSpendUTxOByronPBFT gc sk ins outs =
  let txattrs  = mkAttributes ()
      tx       = UnsafeTx ins outs txattrs
      wit      = signTxId (configProtocolMagicId gc) sk (Crypto.hash tx)
      ATxAux atx awit =
        mkTxAux tx . V.fromList . take (NE.length ins) $ repeat wit
  in mkByronTx $ ATxAux (reAnnotate atx) (reAnnotate awit)

-- | Perform an action that expects ProtocolInfo for Byron/PBFT,
--   with attendant configuration.
withRealPBFT
  :: CLIOps IO
  -> PartialCardanoConfiguration
  -> CommonCLI
  -> (RunDemo (ByronBlockOrEBB ByronConfig)
      => CardanoConfiguration
      -> Consensus.Protocol (ByronBlockOrEBB ByronConfig)
      -> IO ())
  -> IO ()
withRealPBFT CLIOps{coProtocol} pcc common action = do
  cc <- mkConfiguration pcc common
  SomeProtocol p <- fromProtocol cc coProtocol
  case p of
    proto@Consensus.ProtocolRealPBFT{} -> do
      action cc proto
    _ -> throwIO $ ProtocolNotSupported coProtocol

writeSecrets :: FilePath -> String -> String -> (a -> IO LB.ByteString) -> [a] -> IO ()
writeSecrets outDir prefix suffix secretOp xs =
  forM_ (zip xs $ [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    secretOp secret >>= LB.writeFile filename
#ifdef UNIX
    setFileMode                      filename ownerReadMode
#else
    setPermissions filename (emptyPermissions {readable = True})
#endif
