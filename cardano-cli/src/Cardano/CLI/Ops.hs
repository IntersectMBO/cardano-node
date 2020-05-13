{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Ops
  ( CardanoEra(..)
  , cardanoEraForProtocol
  , decodeCBOR
  , deserialiseSigningKey
  , ensureNewFile
  , ensureNewFileLBS
  , getGenesisHashText
  , ncCardanoEra
  , pPrintCBOR
  , readCBOR
  , readGenesis
  , readProtocolMagicId
  , serialiseDelegationCert
  , serialiseDelegateKey
  , serialiseGenesis
  , serialisePoorKey
  , serialiseSigningKey
  , validateCBOR
  , withRealPBFT
  ) where

import           Cardano.Prelude hiding (atomically, catch, option)

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, left)

import qualified Data.ByteString.Lazy as LB
import qualified Formatting as F
import           System.Directory (canonicalizePath, doesPathExist, makeAbsolute)
import qualified Text.JSON.Canonical as CanonicalJSON

import           Cardano.Binary (Decoder, fromCBOR)
import           Cardano.Chain.Block (fromCBORABlockOrBoundary)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Crypto.Signing as Crypto
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Crypto (SigningKey (..))
import qualified Cardano.Crypto.Hashing as Crypto
import           Cardano.Crypto.ProtocolMagic as Crypto
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Node.Run
                   (RunNode(..))

import           Cardano.Config.Protocol
                   (SomeConsensusProtocol(..), mkConsensusProtocol)
import           Cardano.Config.Types
import qualified Cardano.CLI.Legacy.Byron as Legacy
import           Cardano.CLI.Errors
import           Cardano.CLI.Era


decodeCBOR
  :: LByteString
  -> (forall s. Decoder s a)
  -> Either CliError (LB.ByteString, a)
decodeCBOR bs decoder =
  first CBORDecodingError $ deserialiseFromBytes decoder bs


deserialiseSigningKey :: CardanoEra -> FilePath -> LB.ByteString
                      -> Either CliError SigningKey
deserialiseSigningKey ByronEraLegacy fp delSkey =
  case deserialiseFromBytes Legacy.decodeLegacyDelegateKey delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, Legacy.LegacyDelegateKey sKey ) -> pure sKey

deserialiseSigningKey ByronEra fp delSkey =
  case deserialiseFromBytes Crypto.fromCBORXPrv delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, sKey) -> Right $ SigningKey sKey

deserialiseSigningKey ShelleyEra _ _ = Left $ CardanoEraNotSupported ShelleyEra

-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> ExceptT CliError IO ()
ensureNewFile writer outFile blob = do
  exists <- liftIO $ doesPathExist outFile
  when exists $
    left $ OutputMustNotAlreadyExist outFile
  liftIO $ writer outFile blob

ensureNewFileLBS :: FilePath -> LB.ByteString -> ExceptT CliError IO ()
ensureNewFileLBS = ensureNewFile LB.writeFile

getGenesisHashText :: GenesisFile -> ExceptT CliError IO Text
getGenesisHashText (GenesisFile genFile) = do
  canonGenFile <- liftIO $ canonicalizePath genFile
  gFile <- liftIO $ makeAbsolute canonGenFile
  (_, Genesis.GenesisHash gHash) <- readGenesis $ GenesisFile gFile
  return $ F.sformat Crypto.hashHexF gHash

readProtocolMagicId :: GenesisFile -> ExceptT CliError IO Crypto.ProtocolMagicId
readProtocolMagicId gFile = do
  (genData, _) <- readGenesis gFile
  pure $ Genesis.gdProtocolMagicId genData

-- | Read genesis from a file.
readGenesis :: GenesisFile -> ExceptT CliError IO (Genesis.GenesisData, Genesis.GenesisHash)
readGenesis (GenesisFile fp) = firstExceptT (GenesisReadError fp) $ Genesis.readGenesisData fp

validateCBOR :: CBORObject -> LByteString -> Either CliError Text
validateCBOR cborObject bs =
  case cborObject of
    CBORBlockByron epochSlots -> do
      (const () ) <$> decodeCBOR bs (fromCBORABlockOrBoundary epochSlots)
      Right "Valid Byron block."

    CBORDelegationCertificateByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s Delegation.Certificate)
      Right "Valid Byron delegation certificate."

    CBORTxByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s UTxO.Tx)
      Right "Valid Byron Tx."

    CBORUpdateProposalByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s Update.Proposal)
      Right "Valid Byron update proposal."

    CBORVoteByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s Update.Vote)
      Right "Valid Byron vote."

pPrintCBOR :: LByteString -> ExceptT CliError IO ()
pPrintCBOR bs = do
  case deserialiseFromBytes decodeTerm bs of
    Left err -> left $ CBORPrettyPrintError err
    Right (remaining, decodedVal) -> do
        liftIO . putTextLn . toS . prettyHexEnc $ encodeTerm decodedVal
        unless (LB.null remaining) $
          pPrintCBOR remaining

readCBOR :: FilePath -> ExceptT CliError IO LByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . toS . displayException)
    (LB.readFile fp)


serialiseDelegationCert :: CanonicalJSON.ToJSON Identity a
                        => CardanoEra -> a -> Either CliError LB.ByteString
serialiseDelegationCert ByronEraLegacy dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert ByronEra       dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert ShelleyEra     _       = Left $ CardanoEraNotSupported ShelleyEra


serialiseDelegateKey :: CardanoEra -> SigningKey -> Either CliError LB.ByteString
serialiseDelegateKey ByronEraLegacy sk = pure
                                       . toLazyByteString
                                       . Legacy.encodeLegacyDelegateKey
                                       $ Legacy.LegacyDelegateKey sk
serialiseDelegateKey ByronEra  sk = serialiseSigningKey ByronEra sk
serialiseDelegateKey ShelleyEra _ = Left $ CardanoEraNotSupported ShelleyEra


serialiseGenesis :: CardanoEra -> Genesis.GenesisData
                 -> Either CliError LB.ByteString
serialiseGenesis ByronEraLegacy gData = pure $ canonicalEncodePretty gData
serialiseGenesis ByronEra       gData = pure $ canonicalEncodePretty gData
serialiseGenesis ShelleyEra     _     = Left $ CardanoEraNotSupported ShelleyEra


serialisePoorKey :: CardanoEra -> Genesis.PoorSecret
                 -> Either CliError LB.ByteString
serialisePoorKey ByronEraLegacy ps = serialiseSigningKey ByronEraLegacy $
                                       Genesis.poorSecretToKey ps
serialisePoorKey ByronEra       ps = serialiseSigningKey ByronEra $
                                       Genesis.poorSecretToKey ps
serialisePoorKey ShelleyEra     _  = Left $ CardanoEraNotSupported ShelleyEra


serialiseSigningKey :: CardanoEra -> SigningKey
                    -> Either CliError LB.ByteString
serialiseSigningKey ByronEraLegacy (SigningKey k) = pure $ toLazyByteString (Crypto.toCBORXPrv k)
serialiseSigningKey ByronEra       (SigningKey k) = pure $ toLazyByteString (Crypto.toCBORXPrv k)
serialiseSigningKey ShelleyEra     _              = Left $ CardanoEraNotSupported ShelleyEra



-- | Perform an action that expects ProtocolInfo for Byron/PBFT,
--   with attendant configuration.
withRealPBFT
  :: NodeConfiguration
  -> (RunNode ByronBlock
        => Consensus.Protocol ByronBlock Consensus.ProtocolRealPBFT
        -> ExceptT RealPBFTError IO a)
  -> ExceptT RealPBFTError IO a
withRealPBFT nc action = do
  SomeConsensusProtocol p <- firstExceptT FromProtocolError $
                               mkConsensusProtocol nc Nothing
  case p of
    proto@Consensus.ProtocolRealPBFT{} -> action proto
    _ -> left $ IncorrectProtocolSpecified (ncProtocol nc)

ncCardanoEra :: NodeConfiguration -> CardanoEra
ncCardanoEra = cardanoEraForProtocol . ncProtocol

