{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Node.Protocol.Shelley
  ( mkSomeConsensusProtocolShelley

    -- * Errors
  , ShelleyProtocolInstantiationError(..)
  , GenesisReadError(..)
  , GenesisValidationError(..)
  , PraosLeaderCredentialsError(..)

    -- * Reusable parts
  , readGenesis
  , readGenesisAny
  , readLeaderCredentials
  , genesisHashToPraosNonce
  , validateGenesis
  , checkExpectedGenesisHash
  ) where

import           Cardano.Api hiding (FileError)
import qualified Cardano.Api as Api
import           Cardano.Api.Experimental.Certificate (OperationalCertificate (..))

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Crypto.KES.Class (algorithmNameKES)
import           Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import           Cardano.Ledger.Keys (coerceKeyRole)
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Types
import           Cardano.Node.Tracing.Era.HardFork ()
import           Cardano.Node.Tracing.Era.Shelley ()
import           Cardano.Node.Tracing.Formatting ()
import           Cardano.Node.Tracing.Tracers.ChainDB ()
import           Cardano.Node.Types
import           Cardano.Protocol.Crypto (KES)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras ()
import           Ouroboros.Consensus.Protocol.Praos.AgentClient (MonadKESAgent (..))
import           Ouroboros.Consensus.Protocol.Praos.Common (KESEnvelopeError (..),
                   KESFileLoadError (..), KESFileLoadReason (..), PraosCanBeLeader (..),
                   PraosCredentialsSource (..))
import           Ouroboros.Consensus.Shelley.Node (Nonce (..), ProtocolParamsShelleyBased (..),
                   ShelleyLeaderCredentials (..))

import           Control.Exception (IOException)
import           Control.Monad
import           Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import           Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Data.Word (Word8)
import           System.Directory (doesFileExist, getFileSize)
import           System.FilePath (takeDirectory)
import           System.FS.API (SomeHasFS (..))
import           System.FS.API.Types (MountPoint (MountPoint))
import           System.FS.IO (ioHasFS)
import qualified System.IO.MMap as MMap

import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (peekByteOff)


------------------------------------------------------------------------------
-- Shelley protocol
--

-- | Make 'SomeConsensusProtocol' using the Shelley instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
mkSomeConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolShelley NodeShelleyProtocolConfiguration {
                                  npcShelleyGenesisFile,
                                  npcShelleyGenesisFileHash
                                }
                          files = do
    (genesis, genesisHash) <- firstExceptT GenesisReadError $
                              readGenesis npcShelleyGenesisFile
                                          npcShelleyGenesisFileHash
    firstExceptT GenesisValidationError $ validateGenesis genesis
    leaderCredentials <- firstExceptT PraosLeaderCredentialsError $
                         readLeaderCredentials files

    -- Filesystem rooted at the Shelley genesis directory, used by the ledger to
    -- read initial funds/staking injected from genesis (testnets only).
    let shelleyGenesisFS = SomeHasFS $ ioHasFS $ MountPoint $ takeDirectory $ unGenesisFile npcShelleyGenesisFile

    return $ SomeConsensusProtocol Api.ShelleyBlockType $ Api.ProtocolInfoArgsShelley
      shelleyGenesisFS
      genesis
      Consensus.ProtocolParamsShelleyBased {
        shelleyBasedInitialNonce = genesisHashToPraosNonce genesisHash,
        shelleyBasedLeaderCredentials =
            leaderCredentials
      }
      (ProtVer (natVersion @2) 0)


genesisHashToPraosNonce :: GenesisHash -> Nonce
genesisHashToPraosNonce (GenesisHash h) = Nonce (Crypto.castHash h)

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (ShelleyGenesis, GenesisHash)
readGenesis = readGenesisAny

-- | Read a genesis file using a memory-safe strategy.
--
-- Genesis files are used in testing and benchmarking to create testnets with
-- large datasets. To be able to read these files in memory constrained
-- environments we use a conditional loading strategy: if file is below ~10
-- megabytes it is entirely read into memory, otherwise the 'ByteString' is
-- created using `mmap` that uses the virtual memory subsystem to do on-demand
-- loading.
-- With current usage and how benchmakring is done only Shelley and Conway are
-- affected (Shelley's `initialFunds` and Conway's "delegs" and "initialDReps").
readGenesisAny :: FromJSON genesis
               => GenesisFile
               -> Maybe GenesisHash
               -> ExceptT GenesisReadError IO (genesis, GenesisHash)
readGenesisAny (GenesisFile file) mExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadFileError file) $ do
        -- Size of the file in 8-bit bytes.
        size <- getFileSize file
        {-- Mainnet files for reference:
              -byron-genesis.json:   1056360 (1.1M)
              -shelley-genesis.json:    2486 (2.5K)
              -alonzo-genesis.json:     9459 (9.3K)
              -conway-genesis.json:     4168 (4.1K)
        --}
        if size >= 10485760 -- 10 megabytes.
        then MMap.mmapFileByteString file Nothing -- MMap version.
        else BS.readFile file                     -- Non-lazy version.
    genesisHash <- checkExpectedGenesisHash content mExpectedGenesisHash
    genesis <- firstExceptT (GenesisDecodeError file) $ hoistEither $
                 Aeson.eitherDecodeStrict' content
    return (genesis, genesisHash)

checkExpectedGenesisHash
  :: BS.ByteString -- ^ genesis bytes
  -> Maybe GenesisHash -- ^ expected hash, check for hash match, if provided
  -> ExceptT GenesisReadError IO GenesisHash
checkExpectedGenesisHash genesisBytes mExpected = do
  let actual = GenesisHash $ Crypto.hashWith id genesisBytes
  forM_ mExpected $ \expected ->
    when (actual /= expected) $
      throwError (GenesisHashMismatch actual expected)
  pure actual

validateGenesis :: ShelleyGenesis
                -> ExceptT GenesisValidationError IO ()
validateGenesis genesis =
    firstExceptT GenesisValidationErrors . hoistEither $
      Shelley.validateGenesis genesis

readLeaderCredentials
  :: Maybe ProtocolFilepaths
  -> ExceptT PraosLeaderCredentialsError IO [ShelleyLeaderCredentials StandardCrypto]
readLeaderCredentials Nothing = return []
readLeaderCredentials (Just pfp) =
  -- The set of credentials is a sum total of what comes from the CLI,
  -- as well as what's in the bulk credentials file.
  (<>) <$> readLeaderCredentialsSingleton pfp
       <*> readLeaderCredentialsBulk      pfp

readLeaderCredentialsSingleton ::
     ProtocolFilepaths ->
     ExceptT PraosLeaderCredentialsError IO
             [ShelleyLeaderCredentials StandardCrypto]
-- It's OK to supply none of the files on the CLI
readLeaderCredentialsSingleton
   ProtocolFilepaths
     { shelleyCertFile      = Nothing,
       shelleyVRFFile       = Nothing,
       shelleyKESSource     = Nothing
     } = pure []
-- Or to supply all of the files
readLeaderCredentialsSingleton
  ProtocolFilepaths { shelleyCertFile = Just opCertFile,
                      shelleyVRFFile = Just vrfFile,
                      shelleyKESSource = Just kesSource
                    } = do
    vrfSKey <-
      firstExceptT FileError (newExceptT $ readFileTextEnvelope (File vrfFile))

    (credentialsSource, vkey) <- case kesSource of
      KESKeyFilePath kesFile -> do
        OperationalCertificate opCert vkey <-
          firstExceptT FileError $ newExceptT $ readFileTextEnvelope $ File opCertFile
        -- 'updateBlockForging' treats a missing KES key file as "forging
        -- disabled by the operator" and must keep seeing exactly this error.
        exists <- liftIO $ doesFileExist kesFile
        unless exists $ left $ FileError $ Api.FileDoesNotExistError kesFile
        checkKesKeyFileType kesFile
        -- The key itself is read by consensus, straight into mlocked memory,
        -- when the credentials are instantiated; the check that it is the hot
        -- key named in the operational certificate happens there too. Only
        -- the path travels through the forging configuration.
        pure (PraosCredentialsFile opCert kesFile Nothing, vkey)

      -- TODO: minor yikes: when we're using an agent, we don't check that the
      -- opcert and the key provided by the KES agent match, like we do when
      -- the key is provided in a file on the command line
      KESAgentSocketPath socketFile -> do
        OperationalCertificate _ vkey <- firstExceptT FileError $ newExceptT $ readFileTextEnvelope $ File opCertFile
        pure (PraosCredentialsAgent socketFile, vkey)

    return [mkPraosLeaderCredentials credentialsSource vkey vrfSKey]

-- But not OK to supply some of the files without the others.
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyCertFile = Nothing} =
     left OCertNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyVRFFile = Nothing} =
     left VRFKeyNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyKESSource = Nothing} =
     left KESKeyNotSpecified

-- | Header-only check of a KES signing key file: its text envelope @type@
-- must be one the node understands, so that an obviously wrong file fails
-- here, before the node starts syncing, with a readable message.
--
-- The hex text in the file is the secret, so the file is read into mlocked
-- memory (through consensus's 'withKESKeyFileLocked') and only the type
-- string is copied out; no 'ByteString' of the file ever exists on the GHC
-- heap. The full validation of the envelope and the key is done by consensus
-- when the credentials are instantiated.
checkKesKeyFileType :: FilePath -> ExceptT PraosLeaderCredentialsError IO ()
checkKesKeyFileType kesFile = do
  mType <- handleIOExceptT (FileError . Api.FileIOError kesFile) $
    withKESKeyFileLocked kesFile $ \ptr len -> do
      mSpan <- scanEnvelopeType ptr len
      forM mSpan $ \(off, n) ->
        T.decodeUtf8With T.lenientDecode
          <$> BS.packCStringLen (castPtr (ptr `plusPtr` off), n)
  case mType of
    Nothing ->
      left $ EnvelopeParseError kesFile "no \"type\" field found in the text envelope"
    Just ty
      | ty `elem` acceptedKesKeyTypes -> pure ()
      | otherwise -> left $ UnsupportedKesKeyType kesFile ty

-- | The text envelope types accepted for @--shelley-kes-key@ files: for now
-- only the evolution-zero type written by @cardano-cli node key-gen-KES@.
acceptedKesKeyTypes :: [T.Text]
acceptedKesKeyTypes =
  [ T.pack ("KesSigningKey_" <> algorithmNameKES (Proxy @(KES StandardCrypto))) ]

-- | Find the string value of the top-level @"type"@ member of the JSON object
-- held in a buffer and return its offset and length (the content between
-- the quotes, escapes left as they are). Only the shape of a text envelope
-- is understood: an object whose members are all strings. Anything else, or
-- an object without a @"type"@ member, yields 'Nothing'. Nothing is copied
-- out of the buffer.
scanEnvelopeType :: Ptr Word8 -> Int -> IO (Maybe (Int, Int))
scanEnvelopeType ptr len = runMaybeT $ do
    i0 <- skipWs 0
    i1 <- expect 0x7b i0 -- '{'
    i2 <- skipWs i1
    member i2
  where
    byteAt :: Int -> MaybeT IO Word8
    byteAt i = do
      guard (i < len)
      lift (peekByteOff ptr i)

    skipWs :: Int -> MaybeT IO Int
    skipWs i
      | i >= len = pure i
      | otherwise = do
          b <- byteAt i
          if b == 0x20 || b == 0x09 || b == 0x0a || b == 0x0d
            then skipWs (i + 1)
            else pure i

    expect :: Word8 -> Int -> MaybeT IO Int
    expect w i = do
      b <- byteAt i
      guard (b == w)
      pure (i + 1)

    -- A JSON string whose opening quote is at @i@: content offset, content
    -- length and the position after the closing quote.
    string :: Int -> MaybeT IO (Int, Int, Int)
    string i = do
      start <- expect 0x22 i -- '"'
      end <- stringEnd start
      pure (start, end - start, end + 1)

    stringEnd :: Int -> MaybeT IO Int
    stringEnd i = do
      b <- byteAt i
      if | b == 0x22 -> pure i              -- closing '"'
         | b == 0x5c -> stringEnd (i + 2)   -- skip the escaped character
         | otherwise -> stringEnd (i + 1)

    -- One @"key": "value"@ member starting at @i@, then the next one after
    -- a comma; a closing brace before a @"type"@ member ends the search.
    member :: Int -> MaybeT IO (Int, Int)
    member i = do
      (kOff, kLen, i1) <- string i
      i2 <- skipWs i1 >>= expect 0x3a -- ':'
      i3 <- skipWs i2
      (vOff, vLen, i4) <- string i3
      isType <- lift (spanIs kOff kLen "type")
      if isType
        then pure (vOff, vLen)
        else do
          i5 <- skipWs i4 >>= expect 0x2c -- ','
          skipWs i5 >>= member

    spanIs :: Int -> Int -> String -> IO Bool
    spanIs off n str
      | n /= length str = pure False
      | otherwise =
          and <$> zipWithM (\k c -> (== fromIntegral (ord c)) <$> byteAtIO (off + k)) [0 ..] str

    byteAtIO :: Int -> IO Word8
    byteAtIO = peekByteOff ptr

data ShelleyCredentials
  = ShelleyCredentials
    { scCert :: (TextEnvelope, FilePath)
    , scVrf  :: (TextEnvelope, FilePath)
    , scKes  :: (TextEnvelope, FilePath)
    }

readLeaderCredentialsBulk
  :: ProtocolFilepaths
  -> ExceptT PraosLeaderCredentialsError IO [ShelleyLeaderCredentials StandardCrypto]
readLeaderCredentialsBulk ProtocolFilepaths { shelleyBulkCredsFile = mfp } =
  mapM parseShelleyCredentials =<< readBulkFile mfp
 where
   parseShelleyCredentials
     :: ShelleyCredentials
     -> ExceptT PraosLeaderCredentialsError IO (ShelleyLeaderCredentials StandardCrypto)
   parseShelleyCredentials ShelleyCredentials { scCert, scVrf, scKes } = do
     OperationalCertificate opCert vkey <- parseEnvelope scCert
     KesSigningKey kesKey <- parseEnvelope scKes
     let credentialsSource = PraosCredentialsUnsound opCert kesKey
     vrfSKey <- parseEnvelope scVrf
     pure $ mkPraosLeaderCredentials credentialsSource vkey vrfSKey

   readBulkFile
     :: Maybe FilePath
     -> ExceptT PraosLeaderCredentialsError IO [ShelleyCredentials]
   readBulkFile Nothing = pure []
   readBulkFile (Just fp) = do
     content <- handleIOExceptT (CredentialsReadError fp) $
                  BS.readFile fp
     envelopes <- firstExceptT (EnvelopeParseError fp) $ hoistEither $
                    Aeson.eitherDecodeStrict' content
     pure $ uncurry mkCredentials <$> zip [0..] envelopes
    where
      mkCredentials :: Int -> (TextEnvelope, TextEnvelope, TextEnvelope)
                    -> ShelleyCredentials
      mkCredentials ix (teCert, teVrf, teKes) =
       let loc ty = fp <> "." <> show ix <> ty
       in ShelleyCredentials (teCert, loc "cert")
                             (teVrf,  loc "vrf")
                             (teKes,  loc "kes")

mkPraosLeaderCredentials ::
     PraosCredentialsSource StandardCrypto
  -> VerificationKey StakePoolKey
  -> SigningKey VrfKey
  -> ShelleyLeaderCredentials StandardCrypto
mkPraosLeaderCredentials
    credentialsSource
    (StakePoolVerificationKey vkey)
    (VrfSigningKey vrfKey) =
    ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader =
        PraosCanBeLeader {
          praosCanBeLeaderCredentialsSource = credentialsSource,
          praosCanBeLeaderColdVerKey = coerceKeyRole vkey,
          praosCanBeLeaderSignKeyVRF = vrfKey
        },
      shelleyLeaderCredentialsLabel = "Shelley"
    }

parseEnvelope ::
     HasTextEnvelope a
  => (TextEnvelope, String)
  -> ExceptT PraosLeaderCredentialsError IO a
parseEnvelope (te, loc) =
  firstExceptT (FileError . Api.FileError loc) . hoistEither $
    deserialiseFromTextEnvelope te


------------------------------------------------------------------------------
-- Errors
--

data ShelleyProtocolInstantiationError =
       GenesisReadError GenesisReadError
     | GenesisValidationError GenesisValidationError
     | PraosLeaderCredentialsError PraosLeaderCredentialsError
  deriving Show

instance Error ShelleyProtocolInstantiationError where
  prettyError (GenesisReadError err) = prettyError err
  prettyError (GenesisValidationError err) = prettyError err
  prettyError (PraosLeaderCredentialsError err) = prettyError err


data GenesisReadError =
       GenesisReadFileError !FilePath !IOException
     | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
     | GenesisDecodeError !FilePath !String
  deriving Show

instance Error GenesisReadError where
  prettyError (GenesisReadFileError fp err) =
        "There was an error reading the genesis file: "
     <> pshow fp <> " Error: " <> pshow err

  prettyError (GenesisHashMismatch actual expected) =
        "Wrong genesis file: the actual hash is " <> pshow actual
     <> ", but the expected genesis hash given in the node "
     <> "configuration file is " <> pshow expected

  prettyError (GenesisDecodeError fp err) =
        "There was an error parsing the genesis file: "
     <> pshow fp <> " Error: " <> pshow err

newtype GenesisValidationError = GenesisValidationErrors [Shelley.ValidationErr]
  deriving Show

instance Error GenesisValidationError where
  prettyError (GenesisValidationErrors vErrs) =
    pshow (T.unlines (map Shelley.describeValidationErr vErrs))


data PraosLeaderCredentialsError =
       CredentialsReadError !FilePath !IOException
     | EnvelopeParseError !FilePath !String
     | FileError !(Api.FileError TextEnvelopeError)
--TODO: pick a less generic constructor than FileError

     | OCertNotSpecified
     | VRFKeyNotSpecified
     | KESKeyNotSpecified
     | UnsupportedKesKeyType
         FilePath
         -- KES signing key
         T.Text
         -- Text envelope type found in the file
     | KesKeyFileLoadError !KESFileLoadError
         -- Consensus rejected the KES signing key file when instantiating
         -- the credentials
  deriving Show

instance Error PraosLeaderCredentialsError where
  prettyError (CredentialsReadError fp err) =
        "There was an error reading a credentials file: "
     <> pshow fp <> " Error: " <> pshow err

  prettyError (EnvelopeParseError fp err) =
        "There was an error parsing a credentials envelope: "
     <> pshow fp <> " Error: " <> pshow err

  prettyError (FileError fileErr) = prettyError fileErr
  prettyError (UnsupportedKesKeyType kesFp ty) =
       "The KES key file at: " <> pshow kesFp
    <> " has the text envelope type " <> pshow ty
    <> ", which this node does not accept; expected one of: " <> pshow acceptedKesKeyTypes
  prettyError (KesKeyFileLoadError (KESFileLoadError kesFp reason)) =
       "The KES key file at: " <> pshow kesFp <> " could not be loaded: "
    <> renderKESFileLoadReason reason
  prettyError OCertNotSpecified  = pshow $ missingFlagMessage "shelley-operational-certificate"
  prettyError VRFKeyNotSpecified = pshow $ missingFlagMessage "shelley-vrf-key"
  prettyError KESKeyNotSpecified = pshow $ missingFlagMessage "shelley-kes-key"

renderKESFileLoadReason :: KESFileLoadReason -> Doc ann
renderKESFileLoadReason KESFileKeyMismatch =
  "the signing key does not match the KES verification key in the operational certificate"
renderKESFileLoadReason KESFileUnsupportedType =
  "the text envelope type is not supported by this node version"
renderKESFileLoadReason (KESFileEnvelope err) = case err of
  EnvelopeNotAnObject -> "the file is not a JSON object"
  EnvelopeMissingField field -> "the text envelope has no " <> pshow field <> " field"
  EnvelopeUnknownType -> "unknown text envelope type"
  EnvelopeBadHex hexErr -> "the cborHex field is not valid hex: " <> pshow hexErr
  EnvelopeBadCbor -> "the cborHex field does not hold a single CBOR byte string"
  EnvelopeBadLength expected actual ->
    "wrong key length: expected " <> pshow expected <> " bytes, found " <> pshow actual

missingFlagMessage :: String -> String
missingFlagMessage flag =
  "To create blocks, the --" <> flag <> " must also be specified"
