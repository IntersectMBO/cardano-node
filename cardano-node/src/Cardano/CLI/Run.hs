{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.CLI.Run (
    decideKeyMaterialOps
  , CliError (..)
  , Command(..)
  , KeyMaterialOps (..)
  , runCommand
  , SystemVersion(..)
  ) where

import           Prelude (String)
import qualified Prelude as Prelude

import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad
import           Data.Bits (shiftL)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.Semigroup ((<>))
import           Data.String (fromString)
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Formatting as F
import           Options.Applicative
import           System.Directory (createDirectory, doesPathExist)
import           System.FilePath ((</>))
import           System.IO (hGetLine, hSetEcho, hFlush, stdout, stdin)
import           Text.Printf (printf)
import           Data.Time (UTCTime)
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions)
#endif

import qualified Crypto.SCRAPE as Scrape

import           Cardano.Prelude hiding (option)

import           Cardano.Binary (Annotated(..), serialize')
import           Cardano.Chain.Common
import qualified Cardano.Chain.Common as CC
import           Cardano.Chain.Delegation hiding (epoch)
import           Cardano.Chain.Slotting (EpochNumber)
import           Cardano.Crypto (SigningKey (..), ProtocolMagic, ProtocolMagicId)
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy
import qualified Cardano.Crypto.Random as CCr
import qualified Cardano.Crypto.Hashing as CCr
import qualified Cardano.Crypto.Signing as CCr
import           Cardano.Chain.Genesis
--Cardano.Crypto.Signing.VerificationKey
import           Cardano.Node.CanonicalJSON

import qualified Cardano.Legacy.Byron as Legacy


data CliError
  -- Basic user errors
  = OutputMustNotAlreadyExist FilePath
  -- Validation errors
  | CertificateValidationErrors FilePath [Text]
  -- Serialization errors
  | ProtocolParametersParseFailed FilePath Text
  | GenesisReadError FilePath GenesisDataError
  | SigningKeyDeserialisationFailed FilePath Codec.CBOR.Read.DeserialiseFailure
  | VerificationKeyDeserialisationFailed FilePath Text
  | DlgCertificateDeserialisationFailed FilePath Text
    -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  -- Inconsistencies
  | DelegationError GenesisDelegationError
  | GenesisSpecError Text
  | GenesisGenerationError GenesisDataGenerationError
  -- Invariants/assertions -- does it belong here?
  | NoGenesisDelegationForKey Text

instance Show CliError where
  show (OutputMustNotAlreadyExist fp)
    = "Output file/directory must not already exist: " <> fp
  show (CertificateValidationErrors fp errs)
    = Prelude.unlines $
      "Errors while validating certificate '" <> fp <> "':":
      (("  " <>) . T.unpack <$> errs)
  show (ProtocolParametersParseFailed fp err)
    = "Protocol parameters file '" <> fp <> "' read failure: "<> T.unpack err
  show (GenesisReadError fp err)
    = "Genesis file '" <> fp <> "' read failure: "<> show err
  show (SigningKeyDeserialisationFailed fp err)
    = "Signing key '" <> fp <> "' read failure: "<> show err
  show (VerificationKeyDeserialisationFailed fp err)
    = "Verification key '" <> fp <> "' read failure: "<> T.unpack err
  show (DlgCertificateDeserialisationFailed fp err)
    = "Delegation certificate '" <> fp <> "' read failure: "<> T.unpack err
  show (DelegationError err)
    = "Error while issuing delegation: " <> show err
  show (GenesisSpecError err)
    = "Error in genesis specification: " <> T.unpack err
  show (GenesisGenerationError err)
    = "Genesis generation failed: " <> show err
  show (NoGenesisDelegationForKey key)
    = "Newly-generated genesis doesn't delegate to operational key: " <> T.unpack key

instance Exception CliError

-- | Key material operations for a specific system era, e.g.
--   Byron/Classic, Byron/PBFT etc.
data KeyMaterialOps m
  = KeyMaterialOps
  { kmoSerialiseGenesisKey       :: SigningKey    -> m LB.ByteString
  , kmoSerialiseDelegateKey      :: SigningKey    -> m LB.ByteString
  , kmoSerialisePoorKey          :: PoorSecret    -> m LB.ByteString
  , kmoSerialiseGenesis          :: GenesisData   -> m LB.ByteString
  , kmoSerialiseDelegationCert   :: Certificate   -> m LB.ByteString
  , kmoDeserialiseDelegateKey    :: FilePath
                                 -> LB.ByteString -> m SigningKey
  }

data Command
  = Genesis
    !FilePath
    !UTCTime
    !FilePath
    !BlockCount
    !ProtocolMagic
    !TestnetBalanceOptions
    !FakeAvvmOptions
    !LovelacePortion
    !(Maybe Integer)
  | PrettySigningKeyPublic
    !FilePath
  | MigrateDelegateKeyFrom
    !SystemVersion
    !FilePath
    !FilePath
  | DumpHardcodedGenesis
    !FilePath
  | PrintGenesisHash
    !FilePath
  | PrintSigningKeyAddress
    !NetworkMagic -- TODO:  consider deprecation in favor of ProtocolMagicId,
                  --        once Byron is out of the picture.
    !FilePath
  | Keygen
    !FilePath
    !Bool
  | ToVerification
    !FilePath
    !FilePath
  | Redelegate
    !ProtocolMagicId
    !EpochNumber
    !FilePath
    !FilePath
    !FilePath
  | CheckDelegation
    !ProtocolMagicId
    !FilePath
    !FilePath
    !FilePath
  | SubmitTx


runCommand :: KeyMaterialOps IO -> Command -> IO ()
runCommand kmo@KeyMaterialOps{..}
         (Genesis
           outDir
           startTime
           protocolParametersFile
           blockCount
           protocolMagic
           giTestBalance
           giFakeAvvmBalance
           giAvvmBalanceFactor
           giSeed) = do
  protoParamsRaw <- LB.readFile protocolParametersFile
  protocolParameters <- case canonicalDecPre protoParamsRaw of
    Left e  -> throwIO $ ProtocolParametersParseFailed protocolParametersFile e
    Right x -> pure x

  -- We're relying on the generator to fake AVVM and delegation.
  mGenesisDlg <- runExceptT $ mkGenesisDelegation []
  genesisDelegation <- case mGenesisDlg of
    Left e  -> throwIO $ DelegationError e
    Right x -> pure x

  seed <- case giSeed of
    Nothing -> CCr.runSecureRandom . CCr.randomNumber $ shiftL 1 32
    Just x  -> pure x

  let genesisAvvmBalances = GenesisAvvmBalances mempty
  let mGenesisSpec =
        mkGenesisSpec
        genesisAvvmBalances -- :: !GenesisAvvmBalances
        genesisDelegation   -- :: !GenesisDelegation
        protocolParameters  -- :: !ProtocolParameters
        blockCount          -- :: !BlockCount
        protocolMagic       -- :: !ProtocolMagic
        genesisInitializer  -- :: !GenesisInitializer
      genesisInitializer =
        GenesisInitializer
        giTestBalance       -- :: !TestnetBalanceOptions
        giFakeAvvmBalance   -- :: !FakeAvvmOptions
        giAvvmBalanceFactor -- :: !LovelacePortion
        giUseHeavyDlg       -- :: !Bool
        seed                -- :: !Integer
      giUseHeavyDlg =
        True                -- Not using delegate keys unsupported.

  genesisSpec <- case mGenesisSpec of
    Left e  -> throwIO $ GenesisSpecError e
    Right x -> pure x

  mGData <- runExceptT $ generateGenesisData startTime genesisSpec
  (genesisData, generatedSecrets) <- case mGData of
    Left e  -> throwIO $ GenesisGenerationError e
    Right x -> pure x

  dumpGenesis kmo outDir genesisData generatedSecrets

runCommand kmo@KeyMaterialOps{..} (PrettySigningKeyPublic secretPath) =
  putStrLn =<< T.unpack
             . prettySigningKeyPub
             <$> readSigningKey kmo secretPath

runCommand kmo (MigrateDelegateKeyFrom
                  fromVer
                  secretPathTo
                  secretPathFrom) =
        LB.writeFile secretPathTo
    =<< kmoSerialiseDelegateKey kmo
    =<< readSigningKey fromKMO secretPathFrom
  where
    fromKMO = decideKeyMaterialOps fromVer

runCommand kmo (DumpHardcodedGenesis outDir) =
  dumpGenesis kmo outDir
              (configGenesisData Dummy.dummyConfig)
              Dummy.dummyGeneratedSecrets

runCommand KeyMaterialOps{..} (PrintGenesisHash fp) = do
  gdE <- runExceptT (readGenesisData fp)
  case gdE of
    Left e  -> throwIO $ GenesisReadError fp e
    Right x -> putStrLn . F.format CCr.hashHexF
               . unGenesisHash
               $ snd x

runCommand kmo@KeyMaterialOps{..} (PrintSigningKeyAddress netMagic secPath) =
  putStrLn . T.unpack . prettyAddress
           . CC.makeVerKeyAddress netMagic
           . CCr.toVerification
           =<< readSigningKey kmo secPath

runCommand KeyMaterialOps{..}
           (Keygen outFile disablePassword) = do

  passph <- if disablePassword
            then pure CCr.emptyPassphrase
            else readPassword $
                 "Enter password to encrypt '" <> outFile <> "': "

  (_vk, esk) <- CCr.runSecureRandom $ CCr.safeKeyGen passph

  ensureNewFileLBS outFile
    =<< (kmoSerialiseDelegateKey $ SigningKey $ CCr.eskPayload esk)

runCommand kmo (ToVerification
                  secretPath
                  outFile) = do
  ensureNewFileText outFile
    . Builder.toLazyText . CCr.formatFullVerificationKey . CCr.toVerification
    =<< readSigningKey kmo secretPath

runCommand kmo@KeyMaterialOps{..}
           (Redelegate protoMagic epoch genesisSF delegateVF outCertF) = do
  sk <- readSigningKey kmo genesisSF
  vk <- readVerificationKey delegateVF
  let signer = CCr.noPassSafeSigner sk
  -- TODO:  we need to support password-protected secrets.

  let cert = mkCertificate protoMagic signer vk epoch
  ensureNewFileLBS outCertF =<< kmoSerialiseDelegationCert cert

runCommand KeyMaterialOps{..}
           (CheckDelegation magic certF issuerVF delegateVF) = do
  issuerVK'   <- readVerificationKey issuerVF
  delegateVK' <- readVerificationKey delegateVF
  certBS      <- LB.readFile certF
  cert :: Certificate <- case canonicalDecPre certBS of
    Left e  -> throwIO $ DlgCertificateDeserialisationFailed certF e
    Right x -> pure x

  let magic' = Annotated magic (serialize' magic)
      epoch  = unAnnotated $ aEpoch cert
      cert'  = cert { aEpoch = Annotated epoch (serialize' epoch) }
      vk    :: forall r. F.Format r (CCr.VerificationKey -> r)
      vk     = CCr.fullVerificationKeyF
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
  unless (null issues) $
    throwIO $ CertificateValidationErrors certF issues

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

-- TODO:  we need to support password-protected secrets.
readSigningKey :: KeyMaterialOps IO -> FilePath -> IO SigningKey
readSigningKey kmo fp =
  kmoDeserialiseDelegateKey kmo fp =<< LB.readFile fp

readVerificationKey :: FilePath -> IO CCr.VerificationKey
readVerificationKey fp = do
  vkB <- SB.readFile fp
  case CCr.parseFullVerificationKey . fromString $ UTF8.toString vkB of
    Left e -> throwIO . VerificationKeyDeserialisationFailed fp $ show e
    Right x -> pure x

-- TODO:  we'd be better served by a combination of a temporary file
--        with an atomic rename.
ensureNewFile' :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
ensureNewFile' writer outFile blob = do
  exists <- doesPathExist outFile
  when exists $
    throwIO $ OutputMustNotAlreadyExist outFile
  writer outFile blob

ensureNewFileLBS :: FilePath -> LB.ByteString -> IO ()
ensureNewFileLBS = ensureNewFile' LB.writeFile

ensureNewFileText :: FilePath -> TL.Text -> IO ()
ensureNewFileText = ensureNewFile' TL.writeFile

readPassword :: String -> IO CCr.PassPhrase
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
  CCr.PassPhrase . BA.convert . UTF8.fromString <$> loop

dumpGenesis :: KeyMaterialOps IO -> FilePath -> GenesisData -> GeneratedSecrets -> IO ()
dumpGenesis KeyMaterialOps{..} outDir genesisData GeneratedSecrets{..} = do
  exists <- doesPathExist outDir
  if exists
    then throwIO $ OutputMustNotAlreadyExist outDir
    else createDirectory outDir

  let genesisJSONFile = outDir <> "/genesis.json"
  LB.writeFile genesisJSONFile =<< kmoSerialiseGenesis genesisData

  let dlgCertMap = unGenesisDelegation $ gdHeavyDelegation genesisData
      isCertForSK :: SigningKey -> Certificate -> Bool
      isCertForSK sk UnsafeACertificate{..} = delegateVK == CCr.toVerification sk
      findDelegateCert :: SigningKey -> IO Certificate
      findDelegateCert sk =
        case flip find (Map.elems dlgCertMap) . isCertForSK $ sk of
          Nothing -> throwIO $ NoGenesisDelegationForKey $ prettySigningKeyPub sk
          Just x  -> pure x
      wOut :: String -> String -> (a -> IO LB.ByteString) -> [a] -> IO ()
      wOut = writeSecrets outDir
  dlgCerts <- mapM findDelegateCert gsRichSecrets

  wOut "genesis-keys"    "key"  kmoSerialiseGenesisKey     gsDlgIssuersSecrets
  wOut "delegate-keys"   "key"  kmoSerialiseDelegateKey    gsRichSecrets
  wOut "poor-keys"       "key"  kmoSerialisePoorKey        gsPoorSecrets
  wOut "delegation-cert" "json" kmoSerialiseDelegationCert dlgCerts
  wOut "avvm-seed"       "seed" (pure . LB.fromStrict)     gsFakeAvvmSeeds

prettySigningKeyPub :: SigningKey -> Text
prettySigningKeyPub (CCr.toVerification -> vk) = TL.toStrict
  $  "public key hash: " <> (F.format CCr.hashHexF . CC.addressHash $ vk) <> "\n"
  <> "     public key: " <> (Builder.toLazyText . CCr.formatFullVerificationKey $ vk)

prettyAddress :: CC.Address -> Text
prettyAddress addr = TL.toStrict
  $  F.format CC.addressF         addr <> "\n"
  <> F.format CC.addressDetailedF addr

data SystemVersion
  = ByronLegacy
  | ByronPBFT
  deriving Show

decideKeyMaterialOps :: SystemVersion -> KeyMaterialOps IO
decideKeyMaterialOps =
  let serialiseSigningKey (SigningKey x) = toLazyByteString $ CCr.toCBORXPrv x
  in \case
  ByronLegacy ->
    KeyMaterialOps
    { kmoSerialiseGenesisKey          = pure . serialiseSigningKey
    , kmoSerialiseDelegateKey         = \sk->
        toLazyByteString . Legacy.encodeLegacyDelegateKey . Legacy.LegacyDelegateKey sk
        <$> CCr.runSecureRandom Scrape.keyPairGenerate
    , kmoSerialisePoorKey             = pure . serialiseSigningKey . poorSecretToKey
    , kmoSerialiseGenesis             = pure . canonicalEncPre
    , kmoSerialiseDelegationCert      = pure . canonicalEncPre
    , kmoDeserialiseDelegateKey       = \f ->
        flip (.) (deserialiseFromBytes Legacy.decodeLegacyDelegateKey) $
        \case Left  e -> throwIO $ SigningKeyDeserialisationFailed f e
              Right x -> pure . Legacy.lrkSigningKey . snd $ x
    }
  ByronPBFT ->
    KeyMaterialOps
    { kmoSerialiseGenesisKey          = pure . serialiseSigningKey
    , kmoSerialiseDelegateKey         = pure . serialiseSigningKey
    , kmoSerialisePoorKey             = pure . serialiseSigningKey . poorSecretToKey
    , kmoSerialiseGenesis             = pure . canonicalEncPre
    , kmoSerialiseDelegationCert      = pure . canonicalEncPre
    , kmoDeserialiseDelegateKey       = \f ->
        flip (.) (deserialiseFromBytes CCr.fromCBORXPrv) $
        \case Left  e -> throwIO $ SigningKeyDeserialisationFailed f e
              Right x -> pure . SigningKey . snd $ x
    }

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
