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

module Cardano.GenesisTool.Run (
    decideKeyMaterialOps
  , main
  , GenesisToolError (..)
  ) where

import           Prelude (String, id)

import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Exception (throw)
import           Control.Monad
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
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath ((</>))
import           System.IO (hGetLine, hSetEcho, hFlush, stdout, stdin)
import           Text.Printf (printf)
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions)
#endif

import qualified Crypto.SCRAPE as Scrape

import           Cardano.Prelude hiding (option)

import           Cardano.Binary (Annotated(..), serialize')
import qualified Cardano.Chain.Common as CC
import           Cardano.Chain.Delegation (Certificate, ACertificate (..)
                                          , mkCertificate, isValid)
import           Cardano.Crypto (SigningKey (..))
import qualified Test.Cardano.Chain.Genesis.Dummy as Dummy
import qualified Cardano.Crypto.Random as CCr
import qualified Cardano.Crypto.Hashing as CCr
import qualified Cardano.Crypto.Signing as CCr
import           Cardano.Chain.Genesis
--Cardano.Crypto.Signing.VerificationKey
import           Cardano.Node.CanonicalJSON

import qualified Cardano.Legacy.Byron as Legacy
import           Cardano.GenesisTool.CLI

main :: IO ()
main = do
  CLI{mainCommand, systemVersion} <- execParser opts
  runCommand (decideKeyMaterialOps systemVersion) mainCommand

-- | Top level parser with info.
opts :: ParserInfo CLI
opts = info (parseCLI <**> helper)
  ( fullDesc
    <> progDesc "Cardano genesis tool."
    <> header "Cardano genesis tool."
  )

data GenesisToolError
  -- Basic user errors
  = OutputMustNotAlreadyExist FilePath
  -- Serialization errors
  | ProtocolParametersParseFailed FilePath Text
  | GenesisReadError GenesisDataError
  | SigningKeyDeserializationFailed FilePath Codec.CBOR.Read.DeserialiseFailure
  | VerificationKeyDeserializationFailed Text
  | DlgCertificateDeserializationFailed Text
    -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  -- Inconsistencies
  | DelegationError GenesisDelegationError
  | GenesisSpecError Text
  | GenesisGenerationError GenesisDataGenerationError
  -- Invariants/assertions
  | NoGenesisDelegationForKey Text
  deriving Show

instance Exception GenesisToolError

-- TODO: should we convert to ExceptT?
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
  let protocolParameters = either
        (throw . ProtocolParametersParseFailed protocolParametersFile) id $
        canonicalDecPre protoParamsRaw

  -- We're relying on the generator to fake AVVM and delegation.
  mGenesisDlg <- runExceptT $ mkGenesisDelegation []
  let genesisDelegation   = either (throw . DelegationError) id mGenesisDlg
      genesisAvvmBalances = GenesisAvvmBalances mempty

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
        giSeed              -- :: !Integer
      giUseHeavyDlg =
        True                -- Not using delegate keys unsupported.

  let genesisSpec = either (throw . GenesisSpecError) id mGenesisSpec

  -- Generate (mostly)
  res <- runExceptT $ generateGenesisData startTime genesisSpec
  let (genesisData, generatedSecrets) =
        either (throw . GenesisGenerationError) id res

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

runCommand KeyMaterialOps{..} (PrintGenesisHash secretPath) =
  putStrLn . F.format CCr.hashHexF
           . unGenesisHash
           . snd . either (throw . GenesisReadError) id
           =<< runExceptT (readGenesisData secretPath)

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
  cert :: ACertificate ()
              <- either (throw . DlgCertificateDeserializationFailed) id
                 . canonicalDecPre <$> LB.readFile certF

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
  mapM_ (hPutStrLn stderr) issues

  exitWith $ if null issues then ExitSuccess else ExitFailure 1

{-------------------------------------------------------------------------------
  Supporting functions
-------------------------------------------------------------------------------}

-- TODO:  we need to support password-protected secrets.
readSigningKey :: KeyMaterialOps IO -> FilePath -> IO SigningKey
readSigningKey kmo fp =
  kmoDeserialiseDelegateKey kmo
  <$> LB.readFile fp

readVerificationKey :: FilePath -> IO CCr.VerificationKey
readVerificationKey fp =
  either (throw . VerificationKeyDeserializationFailed . show) id
  . CCr.parseFullVerificationKey . fromString . UTF8.toString
  <$> SB.readFile fp

-- TODO:  we'd be better served with a combination of a temporary file
--        with an atomic rename.
ensureNewFile' :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
ensureNewFile' writer outFile blob = do
  exists <- doesPathExist outFile
  when exists $
    throw $ OutputMustNotAlreadyExist outFile
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
    then throw $ OutputMustNotAlreadyExist outDir
    else createDirectory outDir

  let genesisJSONFile = outDir <> "/genesis.json"
  LB.writeFile genesisJSONFile =<< kmoSerialiseGenesis genesisData

  let dlgCertMap = unGenesisDelegation $ gdHeavyDelegation genesisData
      isCertForSK :: SigningKey -> Certificate -> Bool
      isCertForSK sk UnsafeACertificate{..} = delegateVK == CCr.toVerification sk
      findDelegateCert :: SigningKey -> Certificate
      findDelegateCert sk =
        fromMaybe (throw . NoGenesisDelegationForKey $ prettySigningKeyPub sk)
        . flip find (Map.elems dlgCertMap) . isCertForSK $ sk
      wOut :: String -> String -> (a -> IO LB.ByteString) -> [a] -> IO ()
      wOut = writeSecrets outDir

  wOut "genesis-keys"    "key"  kmoSerialiseGenesisKey     gsDlgIssuersSecrets
  wOut "delegate-keys"   "key"  kmoSerialiseDelegateKey    gsRichSecrets
  wOut "poor-keys"       "key"  kmoSerialisePoorKey        gsPoorSecrets
  wOut "delegation-cert" "json" kmoSerialiseDelegationCert (findDelegateCert <$> gsRichSecrets)
  wOut "avvm-seed"       "seed" (pure . LB.fromStrict)     gsFakeAvvmSeeds

prettySigningKeyPub :: SigningKey -> Text
prettySigningKeyPub (CCr.toVerification -> vk) = TL.toStrict
  $  "public key hash: " <> (F.format CCr.hashHexF . CC.addressHash $ vk) <> "\n"
  <> "     public key: " <> (Builder.toLazyText . CCr.formatFullVerificationKey $ vk)

prettyAddress :: CC.Address -> Text
prettyAddress addr = TL.toStrict
  $  F.format CC.addressF addr <> "\n"
  <> F.format CC.addressDetailedF addr

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
    , kmoDeserialiseDelegateKey       = Legacy.lrkSigningKey . snd
      . either (throw . SigningKeyDeserializationFailed "") id
      . deserialiseFromBytes Legacy.decodeLegacyDelegateKey
    }
  ByronPBFT ->
    KeyMaterialOps
    { kmoSerialiseGenesisKey          = pure . serialiseSigningKey
    , kmoSerialiseDelegateKey         = pure . serialiseSigningKey
    , kmoSerialisePoorKey             = pure . serialiseSigningKey . poorSecretToKey
    , kmoSerialiseGenesis             = pure . canonicalEncPre
    , kmoSerialiseDelegationCert      = pure . canonicalEncPre
    , kmoDeserialiseDelegateKey       = SigningKey . snd
      . either (throw . SigningKeyDeserializationFailed "") id
      . deserialiseFromBytes CCr.fromCBORXPrv
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
