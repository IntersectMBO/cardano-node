{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
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
{-# OPTIONS_GHC -Wno-partial-fields #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.CLI.Run (
    CliError (..)
  , ClientCommand(..)
  , runCommand
  ) where

import           Prelude (String, error, show)
import           Cardano.Prelude hiding (option, show, trace)

import           Codec.Serialise (serialise, deserialiseOrFail)
import           Control.Tracer
import           Data.Bits (shiftL)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map.Strict as Map
import           Data.Map (Map)
import           Data.Semigroup ((<>))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as V
import qualified Formatting as F
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

import           Cardano.Binary (Annotated(..), reAnnotate, serialize')
import qualified Cardano.Chain.Common as CC.Common
import           Cardano.Chain.Common
import           Cardano.Chain.Delegation hiding (Map, epoch)
import           Cardano.Chain.Genesis
import qualified Cardano.Chain.Genesis as CC.Genesis
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.Chain.UTxO
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Crypto (SigningKey (..), ProtocolMagic, ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Crypto.Signing as Crypto
import           Cardano.Node.Configuration.Presets (mainnetConfiguration)
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy

import           Cardano.CLI.Ops
import           Cardano.Common.CommonCLI
import           Cardano.Common.Protocol
import           Cardano.Node.CanonicalJSON
import           Cardano.Node.Orphans ()
import           Cardano.Node.Configuration.Topology
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
    TopologyInfo
    FilePath
    CommonCLI
  | SpendGenesisUTxO
    FilePath
    FilePath
    Address
    (NonEmpty TxOut)
    CommonCLI

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
    Right x -> putStrLn . F.format Crypto.hashHexF
               . unGenesisHash
               $ snd x

runCommand co@CLIOps{..} (PrintSigningKeyAddress netMagic secPath) =
  putStrLn . T.unpack . prettyAddress
           . CC.Common.makeVerKeyAddress netMagic
           . Crypto.toVerification
           =<< readSigningKey co secPath

runCommand CLIOps{..}
           (Keygen outFile disablePassword) = do

  passph <- if disablePassword
            then pure Crypto.emptyPassphrase
            else readPassword $
                 "Enter password to encrypt '" <> outFile <> "': "

  (_vk, esk) <- Crypto.runSecureRandom $ Crypto.safeKeyGen passph

  ensureNewFileLBS outFile
    =<< (coSerialiseDelegateKey $ SigningKey $ Crypto.eskPayload esk)

runCommand co (ToVerification
                  secretPath
                  outFile) = do
  ensureNewFileText outFile
    . Builder.toLazyText . Crypto.formatFullVerificationKey . Crypto.toVerification
    =<< readSigningKey co secretPath

runCommand co@CLIOps{..}
           (Redelegate protoMagic epoch genesisSF delegateVF outCertF) = do
  sk <- readSigningKey co genesisSF
  vk <- readVerificationKey delegateVF
  let signer = Crypto.noPassSafeSigner sk
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

runCommand CLIOps{..}
           (SubmitTx stTopology stTx stCommon) = do

  cc <- mkConfiguration mainnetConfiguration stCommon

  SomeProtocol p <- fromProtocol cc coProtocol

  case p of
    ProtocolRealPBFT{} -> do
      txBS <- LB.readFile stTx
      case deserialiseOrFail txBS of
        Left  e  -> throwIO $ TxDeserialisationFailed stTx e
        Right tx -> handleTxSubmission p stTopology tx stdoutTracer
    _ -> throwIO $ ProtocolNotSupported coProtocol

runCommand co@CLIOps{..}
           (SpendGenesisUTxO ctTx ctKey ctGenRichAddr ctOuts ctCommon) = do

  sk <- readSigningKey co ctKey

  cc <- mkConfiguration mainnetConfiguration ctCommon

  SomeProtocol p <- fromProtocol cc coProtocol

  case p of
    ProtocolRealPBFT gc _ _ _ _ -> do
      let gentx = txSpendGenesisUTxOByronPBFT gc sk ctGenRichAddr ctOuts
      putStrLn $ "genesis protocol magic: " <> show (configProtocolMagicId gc)
      ensureNewFileLBS ctTx (serialise gentx)
    _ -> throwIO $ ProtocolNotSupported coProtocol

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

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
        <> (T.unpack $ prettyAddress genRichAddr)
        <> "\n\nIt has the following, though:\n\n"
        <> Cardano.Prelude.concat (T.unpack . prettyAddress <$> Map.keys initialUtxo)

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

-- TODO:  we need to support password-protected secrets.
readSigningKey :: CLIOps IO -> FilePath -> IO SigningKey
readSigningKey co fp =
  coDeserialiseDelegateKey co fp =<< LB.readFile fp

readVerificationKey :: FilePath -> IO Crypto.VerificationKey
readVerificationKey fp = do
  vkB <- SB.readFile fp
  case Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB of
    Left e -> throwIO . VerificationKeyDeserialisationFailed fp $ T.pack $ show e
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

prettySigningKeyPub :: SigningKey -> Text
prettySigningKeyPub (Crypto.toVerification -> vk) = TL.toStrict
  $  "public key hash: " <> (F.format Crypto.hashHexF . CC.Common.addressHash $ vk) <> "\n"
  <> "     public key: " <> (Builder.toLazyText . Crypto.formatFullVerificationKey $ vk)

prettyAddress :: CC.Common.Address -> Text
prettyAddress addr = TL.toStrict
  $  F.format CC.Common.addressF         addr <> "\n"
  <> F.format CC.Common.addressDetailedF addr

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
