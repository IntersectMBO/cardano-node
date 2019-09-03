{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Cardano.CLI.Run (
    CliError (..)
  , ClientCommand(..)
  , runCommand
  --
  , GenesisFile(..)
  , NewDirectory(..)
  , SigningKeyFile(..)
  , NewSigningKeyFile(..)
  , VerificationKeyFile(..)
  , NewVerificationKeyFile(..)
  , CertificateFile(..)
  , NewCertificateFile(..)
  , TxFile(..)
  , NewTxFile(..)
  ) where

import           Prelude (String, show)
import           Cardano.Prelude hiding (option, show, trace)
import           Test.Cardano.Prelude (canonicalDecodePretty)

import           Codec.Serialise (serialise, deserialiseOrFail)
import           Control.Tracer
import           Data.Bits (shiftL)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Semigroup ((<>))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Formatting as F
import           System.Directory (doesPathExist)
import           System.IO (hGetLine, hSetEcho, hFlush, stdout, stdin)
import           Data.Time (UTCTime)

import           Cardano.Binary (Annotated(..), serialize')
import qualified Cardano.Chain.Common as CC.Common
import           Cardano.Chain.Common
import           Cardano.Chain.Delegation hiding (Map, epoch)
import           Cardano.Chain.Genesis
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.Chain.UTxO
import           Cardano.Crypto (SigningKey (..), ProtocolMagic, ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Crypto.Signing as Crypto
import           Cardano.Config.Partial (PartialCardanoConfiguration (..))
import           Cardano.Config.Types (CardanoConfiguration(..))
import           Cardano.Node.Configuration.Presets (mainnetConfiguration)
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops
import           Cardano.CLI.Tx
import           Cardano.CLI.Tx.Submission
import           Cardano.Config.CommonCLI
import           Cardano.Common.Orphans ()
import           Cardano.Common.Protocol
import           Cardano.Node.Configuration.Topology


newtype GenesisFile =
  GenesisFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewDirectory =
  NewDirectory FilePath
  deriving (Eq, Ord, Show, IsString)

newtype CertificateFile =
  CertificateFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewCertificateFile =
  NewCertificateFile { nFp :: FilePath }
  deriving (Eq, Ord, Show, IsString)

newtype TxFile =
  TxFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewTxFile =
  NewTxFile FilePath
  deriving (Eq, Ord, Show, IsString)

data ClientCommand
  = Genesis
    NewDirectory
    UTCTime
    FilePath              -- This one is going to be replaced with elementwise CLI args soon,
    BlockCount            --   so no big gain in newtyping it.
    ProtocolMagic
    TestnetBalanceOptions
    FakeAvvmOptions
    LovelacePortion
    (Maybe Integer)
  | PrettySigningKeyPublic
    SigningKeyFile
  | MigrateDelegateKeyFrom
    Protocol
    NewSigningKeyFile
    SigningKeyFile
  | DumpHardcodedGenesis
    NewDirectory
  | PrintGenesisHash
    GenesisFile
  | PrintSigningKeyAddress
    NetworkMagic         -- TODO:  consider deprecation in favor of ProtocolMagicId,
                         --        once Byron is out of the picture.
    SigningKeyFile
  | Keygen
    NewSigningKeyFile
    Bool
  | ToVerification
    SigningKeyFile
    NewVerificationKeyFile

    --- Delegation Related Commands ---

  | IssueDelegationCertificate
    ProtocolMagicId
    EpochNumber
    -- ^ The epoch from which the delegation is valid.
    SigningKeyFile
    -- ^ The issuer of the certificate, who delegates their right to sign blocks.
    VerificationKeyFile
    -- ^ The delegate, who gains the right to sign blocks on behalf of the issuer.
    NewCertificateFile
    -- ^ Filepath of the newly created delegation certificate.
  | CheckDelegation
    ProtocolMagicId
    CertificateFile
    VerificationKeyFile
    VerificationKeyFile

    -----------------------------------

  | SubmitTx
    TopologyInfo
    TxFile
    -- ^ Filepath of transaction to submit.
    CommonCLI
  | SpendGenesisUTxO
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of genesis UTxO owner.
    Address
    -- ^ Genesis UTxO address.
    (NonEmpty TxOut)
    -- ^ Tx output.
    CommonCLI
  | SpendUTxO
    NewTxFile
    -- ^ Filepath of the newly created transaction.
    SigningKeyFile
    -- ^ Signing key of Tx underwriter.
    (NonEmpty TxIn)
    -- ^ Inputs available for spending to the Tx underwriter's key.
    (NonEmpty TxOut)
    -- ^ Genesis UTxO output Address.
    CommonCLI

runCommand :: CLIOps IO -> ClientCommand -> IO ()
runCommand co@CLIOps{..}
         (Genesis
           (NewDirectory outDir)
           startTime
           protocolParametersFile
           blockCount
           protocolMagic
           giTestBalance
           giFakeAvvmBalance
           giAvvmBalanceFactor
           giSeed) = do
  protoParamsRaw <- LB.readFile protocolParametersFile
  protocolParameters <- case canonicalDecodePretty protoParamsRaw of
    Left e  -> throwIO $ ProtocolParametersParseFailed protocolParametersFile e
    Right x -> pure x

  -- We're relying on the generator to fake AVVM and delegation.
  mGenesisDlg <- runExceptT $ mkGenesisDelegation []
  genesisDelegation <- case mGenesisDlg of
    Left e  -> throwIO $ DelegationError e
    Right x -> pure x

  seed <- case giSeed of
    Nothing -> Crypto.runSecureRandom . Crypto.randomNumber $ shiftL 1 32
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

runCommand co@CLIOps{..} (PrettySigningKeyPublic skF) =
  putStrLn =<< T.unpack
             . prettySigningKeyPub
             <$> readSigningKey co skF

runCommand co (MigrateDelegateKeyFrom
                  fromVer
                  (NewSigningKeyFile newKey)
                  oldKey) =
        LB.writeFile newKey
    =<< coSerialiseDelegateKey co
    =<< flip readSigningKey oldKey
    =<< fromCO
  where
    fromCO = decideCLIOps fromVer

runCommand co (DumpHardcodedGenesis (NewDirectory dir)) =
  dumpGenesis co dir
              (configGenesisData Dummy.dummyConfig)
              Dummy.dummyGeneratedSecrets

runCommand CLIOps{..} (PrintGenesisHash (GenesisFile genesis)) = do
  gdE <- runExceptT (readGenesisData genesis)
  case gdE of
    Left e  -> throwIO $ GenesisReadError genesis e
    Right x -> putStrLn . F.format Crypto.hashHexF
               . unGenesisHash
               $ snd x

runCommand co@CLIOps{..} (PrintSigningKeyAddress netMagic skF) =
  putStrLn . T.unpack . prettyAddress
           . CC.Common.makeVerKeyAddress netMagic
           . Crypto.toVerification
           =<< readSigningKey co skF

runCommand CLIOps{..}
           (Keygen (NewSigningKeyFile skF) disablePassword) = do

  passph <- if disablePassword
            then pure Crypto.emptyPassphrase
            else readPassword $
                 "Enter password to encrypt '" <> skF <> "': "

  (_vk, esk) <- Crypto.runSecureRandom $ Crypto.safeKeyGen passph

  ensureNewFileLBS skF
    =<< (coSerialiseDelegateKey $ SigningKey $ Crypto.eskPayload esk)

runCommand co (ToVerification
                skF (NewVerificationKeyFile vkF)) = do
  ensureNewFileText vkF
    . Builder.toLazyText . Crypto.formatFullVerificationKey . Crypto.toVerification
    =<< readSigningKey co skF

runCommand co (IssueDelegationCertificate pM epoch skF vkF cFp) = do
  sk <- readSigningKey co skF
  vk <- readVerificationKey vkF
  let signer = Crypto.noPassSafeSigner sk
  -- TODO:  we need to support password-protected secrets.
  let cert = signCertificate pM vk epoch signer
  ensureNewFileLBS (nFp cFp) =<< (coSerialiseDelegationCert co $ cert)

runCommand CLIOps{..}
           (CheckDelegation magic
            (CertificateFile certF)
            issuerVF
            delegateVF) = do
  issuerVK'   <- readVerificationKey issuerVF
  delegateVK' <- readVerificationKey delegateVF
  certBS      <- LB.readFile certF
  cert :: Certificate <- case canonicalDecodePretty certBS of
    Left e  -> throwIO $ DlgCertificateDeserialisationFailed certF e
    Right x -> pure x

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
  unless (null issues) $
    throwIO $ CertificateValidationErrors certF issues

runCommand co (SubmitTx stTopology (TxFile stTx) stCommon) = do
  withRealPBFT co mainnetConfiguration stCommon $
    \cc p@(ProtocolRealPBFT _ _ _ _ _) -> do
      txBS <- LB.readFile stTx
      case deserialiseOrFail txBS of
        Left  e  -> throwIO $ TxDeserialisationFailed stTx e
        Right tx@ByronTx{byronTxId} -> do
          putStrLn $ "transaction hash (TxId): " <> show byronTxId
          handleTxSubmission cc p stTopology tx stdoutTracer

runCommand co@CLIOps{..}
           (SpendGenesisUTxO
            (NewTxFile ctTx) ctKey ctGenRichAddr ctOuts ctCommon) = do
  withRealPBFT co mainnetConfiguration ctCommon $
    \_cc (ProtocolRealPBFT gc _ _ _ _) -> do
      sk <- readSigningKey co ctKey
      let tx@ByronTx{byronTxId} = txSpendGenesisUTxOByronPBFT gc sk ctGenRichAddr ctOuts
      putStrLn $ "genesis protocol magic:  " <> show (configProtocolMagicId gc)
      putStrLn $ "transaction hash (TxId): " <> show byronTxId
      ensureNewFileLBS ctTx (serialise tx)

runCommand co@CLIOps{..}
           (SpendUTxO
            (NewTxFile ctTx) ctKey ctIns ctOuts ctCommon) = do
  withRealPBFT co mainnetConfiguration ctCommon $
    \_cc (ProtocolRealPBFT gc _ _ _ _) -> do
      sk <- readSigningKey co ctKey
      let tx@ByronTx{byronTxId} = txSpendUTxOByronPBFT gc sk ctIns ctOuts
      putStrLn $ "genesis protocol magic:  " <> show (configProtocolMagicId gc)
      putStrLn $ "transaction hash (TxId): " <> show byronTxId
      ensureNewFileLBS ctTx (serialise tx)

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

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
    proto@ProtocolRealPBFT{} -> do
      action cc proto
    _ -> throwIO $ ProtocolNotSupported coProtocol

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
