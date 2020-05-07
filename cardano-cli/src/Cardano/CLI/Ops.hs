{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Ops
  ( CardanoEra(..)
  , cardanoEraForProtocol
  , decodeCBOR
  , deserialiseSigningKey
  , ensureNewFile
  , ensureNewFileLBS
  , getGenesisHashText
  , getAndPrintLocalTip
  , getLocalTip
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
  , withIOManagerE
  , withRealPBFT
  , CliError(..)
  , RealPBFTError(..)
  ) where

import           Prelude (show, unlines)
import           Cardano.Prelude hiding (atomically, catch, option, show, unlines)

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, left, right)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Formatting as F
import           System.Directory (canonicalizePath, doesPathExist, makeAbsolute)
import qualified Text.JSON.Canonical as CanonicalJSON

import           Cardano.Api (ApiError, LocalStateQueryError, renderLocalStateQueryError)
import           Cardano.Binary
                   (Decoder, DecoderError, fromCBOR)
import           Cardano.Chain.Block (fromCBORABlockOrBoundary)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Config.Shelley.Genesis (ShelleyGenesisError, renderShelleyGenesisError)
import qualified Cardano.Crypto.Signing as Crypto
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Crypto (SigningKey (..))
import qualified Cardano.Crypto.Hashing as Crypto
import           Cardano.Crypto.ProtocolMagic as Crypto
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
                   (MonadSTM, StrictTMVar, atomically, newEmptyTMVarM, tryPutTMVar, takeTMVar)
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (nullTracer)
import           Network.Mux (MuxError)
import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Network.NodeToClient
                   (Codecs'(..), defaultCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                   (nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Config (TopLevelConfig (..), configCodec)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo(..))
import           Ouroboros.Consensus.Node.Run
                   (RunNode(..))
import           Ouroboros.Consensus.Util.Condense (Condense(..))
import           Ouroboros.Network.Block
import           Ouroboros.Network.Mux
                   (AppType(InitiatorApp), OuroborosApplication(..),
                    MuxPeer(..), RunMiniProtocol(..))
import           Ouroboros.Network.NodeToClient
                   (IOManager, LocalConnectionId, NetworkConnectTracers(..),
                    NodeToClientProtocols(..), NodeToClientVersionData(..),
                    NodeToClientVersion, connectTo, localSnocket,
                    localStateQueryPeerNull, localTxSubmissionPeerNull,
                    versionedNodeToClientProtocols, foldMapVersions,
                    withIOManager)
import           Ouroboros.Network.Protocol.ChainSync.Client
                   (ChainSyncClient(..), ClientStIdle(..), ClientStNext(..)
                   , chainSyncClientPeer, recvMsgRollForward)
import           Ouroboros.Network.Protocol.Handshake.Version
                   (DictVersion(..), Versions)

import           Cardano.Common.LocalSocket (chooseSocketPath)
import           Cardano.Config.Protocol
                   (Protocol(..), ProtocolInstantiationError,
                    SomeConsensusProtocol(..), mkConsensusProtocol,
                    renderProtocolInstantiationError)
import           Cardano.Config.Shelley.Address (AddressError, renderAddressError)
import           Cardano.Config.Shelley.ColdKeys (KeyError, renderKeyError)
import           Cardano.Config.Shelley.KES (KESError, renderKESError)
import           Cardano.Config.Shelley.VRF (VRFError, renderVRFError)
import           Cardano.Config.Shelley.OCert (OperationalCertError)
import           Cardano.Config.Types
import qualified Cardano.CLI.Legacy.Byron as Legacy

-- | Many commands have variants or file formats that depend on the era.
--
data CardanoEra = ByronEraLegacy | ByronEra | ShelleyEra
  deriving Show

cardanoEraForProtocol :: Protocol -> CardanoEra
cardanoEraForProtocol BFT      = ShelleyEra
cardanoEraForProtocol Praos    = ShelleyEra
cardanoEraForProtocol MockPBFT = ShelleyEra
cardanoEraForProtocol RealPBFT = ByronEra
cardanoEraForProtocol TPraos   = ShelleyEra

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
    Right (_, decodedVal) -> do liftIO . putTextLn . toS . prettyHexEnc $ encodeTerm decodedVal
                                right ()

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

-- | Exception type for all errors thrown by the CLI.
--   Well, almost all, since we don't rethrow the errors from readFile & such.
data CliError
  = AddressCliError AddressError
  | ByronVoteDecodingError !DecoderError
  | ByronVoteSubmissionError !RealPBFTError
  | ByronReadUpdateProposalFileFailure !FilePath !Text
  | ByronReadVoteFileFailure !FilePath !Text
  | CardanoEraNotSupported !CardanoEra
  | CBORDecodingError !DeserialiseFailure
  | CBORPrettyPrintError !DeserialiseFailure
  | CertificateValidationErrors !FilePath ![Text]
  | DelegationError !Genesis.GenesisDelegationError
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | GenerateTxsError !RealPBFTError
  | GenesisGenerationError !Genesis.GenesisDataGenerationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenesisSpecError !Text
  | IssueUtxoError !RealPBFTError
  | KESCliError KESError
  | KeyCliError KeyError
  | NoBlocksFound !FilePath
  | NodeSubmitTxError !RealPBFTError
  | NotEnoughTxInputs
  | NotEnoughTxOutputs
  | NoGenesisDelegationForKey !Text
  | OutputMustNotAlreadyExist !FilePath
  | OperationalCertError OperationalCertError
  | ProtocolError !ProtocolInstantiationError
  | ProtocolParametersParseFailed !FilePath !Text
  | ReadCBORFileFailure !FilePath !Text
  | ReadSigningKeyFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  | TxDeserialisationFailed !FilePath !DecoderError
  -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  | SigningKeyDeserialisationFailed !FilePath !DeserialiseFailure
  | SpendGenesisUTxOError !RealPBFTError
  | UpdateProposalDecodingError !DecoderError
  | UpdateProposalSubmissionError !RealPBFTError
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | VRFCliError VRFError
  | FileNotFoundError !FilePath
  | CardanoApiError !ApiError
  | IOError !FilePath !IOException
  | AesonDecode !FilePath !Text
  | ShelleyGenesisError !ShelleyGenesisError
  | IncorrectProtocolSpecifiedError !Protocol
  | NodeLocalStateQueryError !LocalStateQueryError
  | AddressDescribeError !Text

instance Show CliError where
  show (AddressCliError e)
    = T.unpack $ renderAddressError e
  show (ByronVoteDecodingError err)
    =  "Error decoding Byron vote: " <> show err
  show (ByronVoteSubmissionError pbftErr)
    = "Error submitting Byron vote: " <> (show $ renderRealPBFTError pbftErr)
  show (ByronReadUpdateProposalFileFailure fp err)
    =  "Error reading Byron update proposal at: " <> fp <> "Error: " <> show err
  show (ByronReadVoteFileFailure fp err)
    =  "Error reading Byron vote at: " <> fp <> "Error: " <> show err
  show (CBORDecodingError e)
    = "Error with CBOR decoding: " <> show e
  show (CBORPrettyPrintError e)
    = "Error with CBOR decoding: " <> show e
  show (CertificateValidationErrors fp errs)
    = unlines $
      "Errors while validating certificate '" <> fp <> "':":
      (("  " <>) . T.unpack <$> errs)
  show (GenesisReadError fp err)
    = "Genesis file '" <> fp <> "' read failure: "<> show err
  show (DelegationError err)
    = "Error while issuing delegation: " <> show err
  show (DlgCertificateDeserialisationFailed fp err)
    = "Delegation certificate '" <> fp <> "' read failure: "<> T.unpack err
  show (FileNotFoundError fp)
    = "File '" <> fp <> "' not found!"
  show (GenerateTxsError err)
    = "Error in GenerateTxs command: " <> (T.unpack $ renderRealPBFTError err)
  show (GenesisGenerationError err)
    = "Genesis generation failed in mkGenesis: " <> show err
  show (GenesisSpecError err)
    = "Error in genesis specification: " <> T.unpack err
  show (IssueUtxoError err)
    = "Error SpendUTxO command: " <> (T.unpack $ renderRealPBFTError err)
  show (KESCliError err)
    = show $ renderKESError err
  show (KeyCliError err)
    = T.unpack $ renderKeyError err
  show (NoBlocksFound fp)
    = "Error while creating update proposal, no blocks found in: " <> fp
  show (NodeSubmitTxError err)
    = "Error in SubmitTx command: " <> (T.unpack $ renderRealPBFTError err)
  show (NoGenesisDelegationForKey key)
    = "Newly-generated genesis doesn't delegate to operational key: " <> T.unpack key
  show NotEnoughTxInputs
    = "Transactions must have at least one input."
  show NotEnoughTxOutputs
    = "Transactions must have at least one output."
  show (OutputMustNotAlreadyExist fp)
    = "Output file/directory must not already exist: " <> fp
  show (OperationalCertError err)
    = show err --TODO: renderOperationalCertError
  show (ProtocolError err)
    = "Protocol Instantiation Error " <> (T.unpack $ renderProtocolInstantiationError err)
  show (CardanoApiError apiError)
    = show apiError
  show (CardanoEraNotSupported era)
    = "Unsupported Cardano era " <> show era
  show (ProtocolParametersParseFailed fp err)
    = "Protocol parameters file '" <> fp <> "' read failure: "<> T.unpack err
  show (ReadSigningKeyFailure fp expt)
    = "Exception encountered while trying to read the signing key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadCBORFileFailure fp expt)
    = "Exception encountered while trying to read the CBOR file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (ReadVerificationKeyFailure fp expt)
    = "Exception encountered while trying to read the verification key file at: " <> fp
       <> "Exception: " <> T.unpack expt
  show (SigningKeyDeserialisationFailed fp err)
    = "Signing key '" <> fp <> "' read failure: "<> show err
  show (SpendGenesisUTxOError err)
    = "Error in SpendGenesisUTxO command: " <> (T.unpack $ renderRealPBFTError err)
  show (TxDeserialisationFailed fp err)
    = "Transaction file '" <> fp <> "' read failure: "<> show err
  show (UpdateProposalDecodingError err)
    = "Error decoding update proposal: " <> show err
  show (UpdateProposalSubmissionError err)
    = "Error submitting update proposal: " <> (T.unpack $ renderRealPBFTError err)
  show (VerificationKeyDeserialisationFailed fp err)
    = "Verification key '" <> fp <> "' read failure: "<> T.unpack err
  show (VRFCliError err) = T.unpack $ renderVRFError err
  show (IOError fp ioe)
    = "File '" <> fp <> "': " ++ show ioe
  show (AesonDecode fp txt)
    = "File '" <> fp <> "': " ++ show txt
  show (ShelleyGenesisError sge)
    = T.unpack $ renderShelleyGenesisError sge
  show (IncorrectProtocolSpecifiedError ptcl)
    = "Incorrect protocol specified: " <> (toS $ show ptcl)
  show (NodeLocalStateQueryError err)
    = T.unpack $ renderLocalStateQueryError err
  show (AddressDescribeError txt)
    = T.unpack txt

data RealPBFTError
  = IncorrectProtocolSpecified !Protocol
  | FromProtocolError !ProtocolInstantiationError
  | InvariantViolation !Text
  | TransactionTypeNotHandledYet !Text

renderRealPBFTError :: RealPBFTError -> Text
renderRealPBFTError err =
  case err of
    IncorrectProtocolSpecified ptcl -> "Incorrect protocol specified: " <> (toS $ show ptcl)
    FromProtocolError ptclInstErr -> renderProtocolInstantiationError ptclInstErr
    InvariantViolation invErr -> "Invariant violation: " <> invErr
    TransactionTypeNotHandledYet err' -> "Transaction type not handled yet: " <> err'

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

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

-- | Get the node's tip, using the local chain sync protocol, and write it to
-- the standard output device.
getAndPrintLocalTip
  :: ConfigYamlFilePath
  -> Maybe CLISocketPath
  -> IOManager
  -> IO ()
getAndPrintLocalTip configFp mSockPath iocp = do
    nc <- parseNodeConfigurationFP configFp
    sockPath <- return $ chooseSocketPath (ncSocketPath nc) mSockPath
    frmPtclRes <- runExceptT $ firstExceptT ProtocolError $
                    mkConsensusProtocol nc Nothing
    SomeConsensusProtocol p <- case frmPtclRes of
                          Right p -> pure p
                          Left err -> do putTextLn . toS $ show err
                                         exitFailure
    putTextLn =<< getTipOutput <$> getLocalTip p iocp sockPath
  where
    getTipOutput :: forall blk. Condense (HeaderHash blk) => Tip blk -> Text
    getTipOutput (TipGenesis) = "Current tip: genesis (origin)"
    getTipOutput (Tip slotNo headerHash blkNo) =
      T.pack $ unlines [ "\n"
                        , "Current tip: "
                        , "Block hash: " <> condense headerHash
                        , "Slot: " <> condense slotNo
                        , "Block number: " <> condense blkNo
                        ]

-- | Get the node's tip using the local chain sync protocol.
getLocalTip
  :: forall blk . RunNode blk
  => Consensus.Protocol blk (BlockProtocol blk)
  -> IOManager
  -> SocketPath
  -> IO (Tip blk)
getLocalTip ptcl iocp sockPath = do
  tipVar <- newEmptyTMVarM
  createNodeConnection (Proxy) ptcl iocp sockPath tipVar
  atomically $ takeTMVar tipVar

createNodeConnection
  :: forall blk . RunNode blk
  => Proxy blk
  -> Consensus.Protocol blk (BlockProtocol blk)
  -> IOManager
  -> SocketPath
  -> StrictTMVar IO (Tip blk)
  -> IO ()
createNodeConnection proxy ptcl iocp (SocketFile path) tipVar =
    let ProtocolInfo{pInfoConfig} = Consensus.protocolInfo ptcl in
    connectTo
      (localSnocket iocp path)
      (NetworkConnectTracers nullTracer nullTracer)
      (localInitiatorNetworkApplication proxy pInfoConfig tipVar)
      path
    `catch` handleMuxError

handleMuxError :: MuxError -> IO ()
handleMuxError err = print err

localInitiatorNetworkApplication
  :: forall blk m.
     ( RunNode blk
     , MonadIO m
     , MonadST    m
     , MonadTimer m
     )
  => Proxy blk
  -> TopLevelConfig blk
  -> StrictTMVar m (Tip blk)
  -> Versions NodeToClientVersion DictVersion
              (LocalConnectionId -> OuroborosApplication InitiatorApp LB.ByteString m () Void)
localInitiatorNetworkApplication proxy cfg tipVar =
    foldMapVersions
      (\v ->
        versionedNodeToClientProtocols
          (nodeToClientProtocolVersion proxy v)
          versionData
          (protocols v))
      (supportedNodeToClientVersions proxy)
 where
  versionData = NodeToClientVersionData { networkMagic = nodeNetworkMagic proxy cfg }

  protocols clientVersion =
      NodeToClientProtocols {
        localChainSyncProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              nullTracer
              cChainSyncCodec
              (chainSyncClientPeer (chainSyncClient tipVar))

      , localTxSubmissionProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              nullTracer
              cTxSubmissionCodec
              localTxSubmissionPeerNull
      , localStateQueryProtocol =
          InitiatorProtocolOnly $
            MuxPeer
              nullTracer
              cStateQueryCodec
              localStateQueryPeerNull
      }
    where
      Codecs { cChainSyncCodec
             , cTxSubmissionCodec
             , cStateQueryCodec
             }
        = defaultCodecs (configCodec cfg) clientVersion

ncCardanoEra :: NodeConfiguration -> CardanoEra
ncCardanoEra = cardanoEraForProtocol . ncProtocol

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)

chainSyncClient
  :: forall blk m . (MonadIO m, MonadSTM m)
  => StrictTMVar m (Tip blk)
  -> ChainSyncClient (Serialised blk) (Tip blk) m ()
chainSyncClient tipVar = ChainSyncClient $ pure $
  SendMsgRequestNext
    clientStNext
    (pure $ ClientStNext
              { recvMsgRollForward = \_ _ -> ChainSyncClient $ pure clientStIdle
              , recvMsgRollBackward = \_ _ -> ChainSyncClient $ pure clientStIdle
              }
    )
 where
  clientStIdle :: ClientStIdle (Serialised blk) (Tip blk) m ()
  clientStIdle =
    SendMsgRequestNext clientStNext (pure clientStNext)
  --TODO: we should be able to simply return the tip as the result with
  -- SendMsgDone and collect this as the result of the overall protocol.
  -- While currently we can have protocols return things, the current OuroborosApplication
  -- stuff gets in the way of returning an overall result, but that's being worked on,
  -- and this can be improved when that's ready.
  clientStNext :: ClientStNext (Serialised blk) (Tip blk) m ()
  clientStNext = ClientStNext
    { recvMsgRollForward = \_blk tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    , recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
        void $ atomically $ tryPutTMVar tipVar tip
        pure $ SendMsgDone ()
    }
