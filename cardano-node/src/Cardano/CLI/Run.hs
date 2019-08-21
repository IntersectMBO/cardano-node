{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.CLI.Run (
    CliError (..)
  , ClientCommand(..)
  , parseClientCommand
  , runCommand
  ) where

import           Prelude (String)
import qualified Prelude as Prelude

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Read (DeserialiseFailure, IDecode(..), deserialiseFromBytes, deserialiseIncremental)
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (Serialise(..), deserialise, deserialiseOrFail)
import           Control.Monad
import           Control.Tracer
import           Data.Bits (shiftL)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal  as LB
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
import           Cardano.BM.Tracing
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
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.Node.Configuration.Presets (mainnetConfiguration)
--Cardano.Crypto.Signing.VerificationKey

import           Cardano.CLI.Ops
import           Cardano.Node.CanonicalJSON
import           Cardano.Node.CLI
import           Cardano.Node.Parsers
import           Cardano.Node.Topology
import           Cardano.Node.TxSubmission


data ClientCommand
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
    !Protocol
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
    { stTopology :: TopologyInfo
    , stTx       :: FilePath
    , stCommon   :: CommonCLI
    }

parseClientCommand :: Parser ClientCommand
parseClientCommand =
  subparser
  (mconcat
    [ commandGroup "Genesis"
    , command' "genesis"                        "Perform genesis." $
      Genesis
      <$> parseFilePath    "genesis-output-dir"       "A yet-absent directory where genesis JSON file along with secrets shall be placed."
      <*> parseUTCTime     "start-time"               "Start time of the new cluster to be enshrined in the new genesis."
      <*> parseFilePath    "protocol-parameters-file" "JSON file with protocol parameters."
      <*> parseK
      <*> parseProtocolMagic
      <*> parseTestnetBalanceOptions
      <*> parseFakeAvvmOptions
      <*> (LovelacePortion . fromInteger . fromMaybe 1 <$>
           (optional $
             parseIntegral  "avvm-balance-factor"      "AVVM balances will be multiplied by this factor (defaults to 1)."))
      <*> optional (parseIntegral    "secret-seed"              "Optionally specify the seed of generation.")
    , command' "dump-hardcoded-genesis"         "Write out a hard-coded genesis." $
      DumpHardcodedGenesis
      <$> parseFilePath    "genesis-output-dir"       "A yet-absent directory where genesis JSON file along with secrets shall be placed."
    , command' "print-genesis-hash"             "Compute hash of a genesis file." $
      PrintGenesisHash
      <$> parseFilePath    "genesis-json"             "Genesis JSON file to hash."
    ])
  <|> subparser
  (mconcat
    [ commandGroup "Keys"
    , command' "keygen"                         "Generate a signing key." $
      Keygen
      <$> parseFilePath    "secret"                   "Non-existent file to write the secret key to."
      <*> parseFlag        "no-password"              "Disable password protection."
    , command' "to-verification"                "Extract a verification key in its base64 form." $
      ToVerification
      <$> parseFilePath    "secret"                   "Secret key file to extract from."
      <*> parseFilePath    "to"                       "Non-existent file to write the base64-formatted verification key to."
    , command' "signing-key-public"             "Pretty-print a signing key's verification key (not a secret)." $
      PrettySigningKeyPublic
      <$> parseFilePath    "secret"                   "File name of the secret key to pretty-print."
    , command' "signing-key-address"            "Print address of a signing key." $
      PrintSigningKeyAddress
      <$> parseNetworkMagic
      <*> parseFilePath    "secret"                   "Secret key, whose address is to be printed."
    , command' "migrate-delegate-key-from"      "Migrate a delegate key from an older version." $
      MigrateDelegateKeyFrom
      <$> parseProtocol
      <*> parseFilePath    "to"                       "Output secret key file."
      <*> parseFilePath    "from"                     "Secret key file to migrate."
    ])
  <|> subparser
  (mconcat
    [ commandGroup "Delegation"
    , command' "redelegate"                     "Redelegate genesis authority to a different verification key." $
      Redelegate
      <$> parseProtocolMagicId "protocol-magic"
      <*> (EpochNumber <$>
            parseIntegral   "since-epoch"              "First epoch of effective delegation.")
      <*> parseFilePath    "secret"                   "The genesis key to redelegate from."
      <*> parseFilePath    "delegate-key"             "The operation verification key to delegate to."
      <*> parseFilePath    "certificate"              "Non-existent file to write the certificate to."
    , command' "check-delegation"               "Verify that a given certificate constitutes a valid delegation relationship betwen keys." $
      CheckDelegation
      <$> parseProtocolMagicId "protocol-magic"
      <*> parseFilePath    "certificate"              "The certificate embodying delegation to verify."
      <*> parseFilePath    "issuer-key"               "The genesis key that supposedly delegates."
      <*> parseFilePath    "delegate-key"             "The operation verification key supposedly delegated to."
    ])
  <|> subparser
  (mconcat
    [ commandGroup "Transactions"
    , command' "submit-tx"                      "Submit a raw, signed transaction, in its on-wire representation." $
      SubmitTx
      <$> parseTopologyInfo "PBFT node ID to submit Tx to."
      <*> parseFilePath    "tx"                       "File containing the raw transaction."
      <*> parseCommonCLI
    ])

runCommand :: CLIOps IO -> ClientCommand -> IO ()
runCommand co@CLIOps{..}
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

  dumpGenesis co outDir genesisData generatedSecrets

runCommand co@CLIOps{..} (PrettySigningKeyPublic secretPath) =
  putStrLn =<< T.unpack
             . prettySigningKeyPub
             <$> readSigningKey co secretPath

runCommand co (MigrateDelegateKeyFrom
                  fromVer
                  secretPathTo
                  secretPathFrom) =
        LB.writeFile secretPathTo
    =<< coSerialiseDelegateKey co
    =<< flip readSigningKey secretPathFrom
    =<< fromCO
  where
    fromCO = decideCLIOps fromVer

runCommand co (DumpHardcodedGenesis outDir) =
  dumpGenesis co outDir
              (configGenesisData Dummy.dummyConfig)
              Dummy.dummyGeneratedSecrets

runCommand CLIOps{..} (PrintGenesisHash fp) = do
  gdE <- runExceptT (readGenesisData fp)
  case gdE of
    Left e  -> throwIO $ GenesisReadError fp e
    Right x -> putStrLn . F.format CCr.hashHexF
               . unGenesisHash
               $ snd x

runCommand co@CLIOps{..} (PrintSigningKeyAddress netMagic secPath) =
  putStrLn . T.unpack . prettyAddress
           . CC.makeVerKeyAddress netMagic
           . CCr.toVerification
           =<< readSigningKey co secPath

runCommand CLIOps{..}
           (Keygen outFile disablePassword) = do

  passph <- if disablePassword
            then pure CCr.emptyPassphrase
            else readPassword $
                 "Enter password to encrypt '" <> outFile <> "': "

  (_vk, esk) <- CCr.runSecureRandom $ CCr.safeKeyGen passph

  ensureNewFileLBS outFile
    =<< (coSerialiseDelegateKey $ SigningKey $ CCr.eskPayload esk)

runCommand co (ToVerification
                  secretPath
                  outFile) = do
  ensureNewFileText outFile
    . Builder.toLazyText . CCr.formatFullVerificationKey . CCr.toVerification
    =<< readSigningKey co secretPath

runCommand co@CLIOps{..}
           (Redelegate protoMagic epoch genesisSF delegateVF outCertF) = do
  sk <- readSigningKey co genesisSF
  vk <- readVerificationKey delegateVF
  let signer = CCr.noPassSafeSigner sk
  -- TODO:  we need to support password-protected secrets.

  let cert = mkCertificate protoMagic signer vk epoch
  ensureNewFileLBS outCertF =<< coSerialiseDelegationCert cert

runCommand CLIOps{..}
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

runCommand CLIOps{..}
           SubmitTx{stTopology, stTx, stCommon} = do

  cc <- mkConfiguration mainnetConfiguration stCommon

  SomeProtocol p <- fromProtocol cc coProtocol

  case p of
    ProtocolRealPBFT{} -> do
      txBS <- LB.readFile stTx
      case deserialiseOrFail txBS of
        Left  e  -> throwIO $ TxDeserialisationFailed stTx e
        Right tx -> handleTxSubmission p stTopology tx stdoutTracer
    x -> throwIO $ ProtocolNotSupported coProtocol

deriving instance Generic (GenTx (ByronBlockOrEBB ByronConfig))
instance Serialise (GenTx (ByronBlockOrEBB ByronConfig)) where
  decode = decodeByronGenTx
  encode = encodeByronGenTx

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

-- TODO:  we need to support password-protected secrets.
readSigningKey :: CLIOps IO -> FilePath -> IO SigningKey
readSigningKey co fp =
  coDeserialiseDelegateKey co fp =<< LB.readFile fp

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
      isCertForSK sk UnsafeACertificate{..} = delegateVK == CCr.toVerification sk
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

prettySigningKeyPub :: SigningKey -> Text
prettySigningKeyPub (CCr.toVerification -> vk) = TL.toStrict
  $  "public key hash: " <> (F.format CCr.hashHexF . CC.addressHash $ vk) <> "\n"
  <> "     public key: " <> (Builder.toLazyText . CCr.formatFullVerificationKey $ vk)

prettyAddress :: CC.Address -> Text
prettyAddress addr = TL.toStrict
  $  F.format CC.addressF         addr <> "\n"
  <> F.format CC.addressDetailedF addr

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
