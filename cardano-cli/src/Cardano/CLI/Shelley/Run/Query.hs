{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLI.Shelley.Run.Query
  ( DelegationsAndRewards(..)
  , ShelleyQueryCmdError
  , ShelleyQueryCmdLocalStateQueryError (..)
  , renderOpCertIntervalInformation
  , renderShelleyQueryCmdError
  , renderLocalStateQueryError
  , runQueryCmd
  , toEpochInfo
  , determineEra
  , mergeDelegsAndRewards
  , percentage
  , executeQuery
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api
import           Cardano.Api.Byron
import           Cardano.Api.Orphans ()
import           Cardano.Api.Shelley

import           Control.Monad.Trans.Except (ExceptT (..), except, runExcept, runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   hoistMaybe, left, onLeft, onNothing)
import           Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Sharing (Interns, Share)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as T
import qualified Data.Text.IO as Text
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Time.Clock
import qualified Data.Vector as Vector
import           Formatting.Buildable (build)
import           Numeric (showEFloat)
import           Prettyprinter
import qualified System.IO as IO
import           Text.Printf (printf)

import           Cardano.Binary (DecoderError)
import           Cardano.CLI.Helpers (HelpersError (..), hushM, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Pretty
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (VerificationKeyOrHashOrFile,
                   readVerificationKeyOrHashOrFile)
import           Cardano.CLI.Shelley.Orphans ()
import qualified Cardano.CLI.Shelley.Output as O
import           Cardano.CLI.Shelley.Run.Genesis (ShelleyGenesisCmdError,
                   readAndDecodeShelleyGenesis)
import           Cardano.CLI.Types
import           Cardano.Crypto.Hash (hashToBytesAsHex)
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.VRF as Crypto
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import           Cardano.Ledger.BaseTypes (Seed, UnitInterval)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.SafeHash (HashAnnotated)
import           Cardano.Ledger.Shelley.LedgerState (PState (_fPParams, _pParams, _retiring))
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import           Cardano.Ledger.Shelley.Scripts ()
import           Cardano.Slotting.EpochInfo (EpochInfo (..), epochInfoSlotToUTCTime, hoistEpochInfo)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   toRelativeTime)
import           Ouroboros.Consensus.Cardano.Block as Consensus (EraMismatch (..))
import           Ouroboros.Consensus.Protocol.TPraos (StandardCrypto)
import           Ouroboros.Network.Block (Serialised (..))

import qualified Ouroboros.Consensus.HardFork.History as Consensus
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus

import           Control.Monad (forM, forM_, join)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.IO.Unlift (MonadIO (..))
import           Control.Monad.Trans.Class
import           Data.Bifunctor (Bifunctor (..))
import           Data.Function ((&))
import           Data.Functor ((<&>))
import qualified Data.List as List
import           Data.Map.Strict (Map)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import           GHC.Records (HasField)

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant flip" -}

data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdAcquireFailure !AcquiringFailure
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | ShelleyQueryCmdByronEra
  | ShelleyQueryCmdEraMismatch !EraMismatch
  | ShelleyQueryCmdUnsupportedMode !AnyConsensusMode
  | ShelleyQueryCmdPastHorizon !Qry.PastHorizonException
  | ShelleyQueryCmdSystemStartUnavailable
  | ShelleyQueryCmdGenesisReadError !ShelleyGenesisCmdError
  | ShelleyQueryCmdLeaderShipError !LeadershipError
  | ShelleyQueryCmdTextEnvelopeReadError !(FileError TextEnvelopeError)
  | ShelleyQueryCmdTextReadError !(FileError InputDecodeError)
  | ShelleyQueryCmdOpCertCounterReadError !(FileError TextEnvelopeError)
  | ShelleyQueryCmdProtocolStateDecodeFailure !(LBS.ByteString, DecoderError)
  | ShelleyQueryCmdPoolStateDecodeError DecoderError
  | ShelleyQueryCmdStakeSnapshotDecodeError DecoderError
  | ShelleyQueryCmdUnsupportedNtcVersion !UnsupportedNtcVersionError

  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    ShelleyQueryCmdAcquireFailure acquireFail -> Text.pack $ show acquireFail
    ShelleyQueryCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
      " Era: " <> textShow era
    ShelleyQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occurred." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra
    ShelleyQueryCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyQueryCmdPastHorizon e -> "Past horizon: " <> textShow e
    ShelleyQueryCmdSystemStartUnavailable -> "System start unavailable"
    ShelleyQueryCmdGenesisReadError err' -> Text.pack $ displayError err'
    ShelleyQueryCmdLeaderShipError e -> Text.pack $ displayError e
    ShelleyQueryCmdTextEnvelopeReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdTextReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdOpCertCounterReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdProtocolStateDecodeFailure (_, decErr) ->
      "Failed to decode the protocol state: " <> toStrict (toLazyText $ build decErr)
    ShelleyQueryCmdPoolStateDecodeError decoderError ->
      "Failed to decode PoolState.  Error: " <> Text.pack (show decoderError)
    ShelleyQueryCmdStakeSnapshotDecodeError decoderError ->
      "Failed to decode StakeSnapshot.  Error: " <> Text.pack (show decoderError)
    ShelleyQueryCmdUnsupportedNtcVersion (UnsupportedNtcVersionError minNtcVersion ntcVersion) ->
      "Unsupported feature for the node-to-client protocol version.\n" <>
      "This query requires at least " <> textShow minNtcVersion <> " but the node negotiated " <> textShow ntcVersion <> ".\n" <>
      "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryLeadershipSchedule consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule outputAs ->
      runQueryLeadershipSchedule consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule outputAs
    QueryProtocolParameters' consensusModeParams network mOutFile ->
      runQueryProtocolParameters consensusModeParams network mOutFile
    QueryTip consensusModeParams network mOutFile ->
      runQueryTip consensusModeParams network mOutFile
    QueryStakePools' consensusModeParams network mOutFile ->
      runQueryStakePools consensusModeParams network mOutFile
    QueryStakeDistribution' consensusModeParams network mOutFile ->
      runQueryStakeDistribution consensusModeParams network mOutFile
    QueryStakeAddressInfo consensusModeParams addr network mOutFile ->
      runQueryStakeAddressInfo consensusModeParams addr network mOutFile
    QueryDebugLedgerState' consensusModeParams network mOutFile ->
      runQueryLedgerState consensusModeParams network mOutFile
    QueryStakeSnapshot' consensusModeParams network allOrOnlyPoolIds mOutFile ->
      runQueryStakeSnapshot consensusModeParams network allOrOnlyPoolIds mOutFile
    QueryProtocolState' consensusModeParams network mOutFile ->
      runQueryProtocolState consensusModeParams network mOutFile
    QueryUTxO' consensusModeParams qFilter networkId mOutFile ->
      runQueryUTxO consensusModeParams qFilter networkId mOutFile
    QueryKesPeriodInfo consensusModeParams network nodeOpCert mOutFile ->
      runQueryKesPeriodInfo consensusModeParams network nodeOpCert mOutFile
    QueryPoolState' consensusModeParams network poolid ->
      runQueryPoolState consensusModeParams network poolid
    QueryTxMempool consensusModeParams network op mOutFile ->
      runQueryTxMempool consensusModeParams network op mOutFile

runQueryProtocolParameters
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  result <- liftIO $ executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
    anyE@(AnyCardanoEra era) <- lift (determineEraExpr cModeParams)
      & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

    case cardanoEraStyle era of
      LegacyByronEra -> left ShelleyQueryCmdByronEra
      ShelleyBasedEra sbe -> do
        let cMode = consensusModeOnly cModeParams

        eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

        lift (queryExpr $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters)
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdEraMismatch)

  writeProtocolParameters mOutFile =<< except (join (first ShelleyQueryCmdAcquireFailure result))

 where
  writeProtocolParameters
    :: Maybe OutputFile
    -> ProtocolParameters
    -> ExceptT ShelleyQueryCmdError IO ()
  writeProtocolParameters mOutFile' pparams =
    case mOutFile' of
      Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $
          LBS.writeFile fpath (encodePretty pparams)

-- | Calculate the percentage sync rendered as text.
percentage
  :: RelativeTime
  -- ^ 'tolerance'.  If 'b' - 'a' < 'tolerance', then 100% is reported.  This even if we are 'tolerance' seconds
  -- behind, we are still considered fully synced.
  -> RelativeTime
  -- ^ 'nowTime'.  The time of the most recently synced block.
  -> RelativeTime
  -- ^ 'tipTime'.  The time of the tip of the block chain to which we need to sync.
  -> Text
percentage tolerance a b = Text.pack (printf "%.2f" pc)
  where -- All calculations are in seconds (Integer)
        t  = relativeTimeSeconds tolerance
        -- Plus 1 to prevent division by zero.  The 's' prefix stands for strictly-positive.
        sa = relativeTimeSeconds a + 1
        sb = relativeTimeSeconds b + 1
        -- Fast forward the 'nowTime` by the tolerance, but don't let the result exceed the tip time.
        ua = min (sa + t) sb
        ub = sb
        -- Final percentage to render as text.
        pc = id @Double (fromIntegral ua / fromIntegral  ub) * 100.0

relativeTimeSeconds :: RelativeTime -> Integer
relativeTimeSeconds (RelativeTime dt) = floor (nominalDiffTimeToSeconds dt)

-- | Query the chain tip via the chain sync protocol.
--
-- This is a fallback query to support older versions of node to client protocol.
queryChainTipViaChainSync :: MonadIO m => LocalNodeConnectInfo mode -> m ChainTip
queryChainTipViaChainSync localNodeConnInfo = do
  liftIO . T.hPutStrLn IO.stderr $
    "Warning: Local header state query unavailable. Falling back to chain sync query"
  liftIO $ getLocalChainTip localNodeConnInfo

runQueryTip
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  case consensusModeOnly cModeParams of
    CardanoMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

      eLocalState <- ExceptT $ fmap sequence $
        executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT $ do
          era <- lift (queryExpr (QueryCurrentEra CardanoModeIsMultiEra)) & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          eraHistory <- lift (queryExpr (QueryEraHistory CardanoModeIsMultiEra)) & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          mChainBlockNo <- lift (queryExpr  QueryChainBlockNo) & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion) & fmap Just
          mChainPoint <- lift (queryExpr (QueryChainPoint CardanoMode)) & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion) & fmap Just
          mSystemStart <- lift (queryExpr  QuerySystemStart) & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion) & fmap Just

          return O.QueryTipLocalState
            { O.era = era
            , O.eraHistory = eraHistory
            , O.mSystemStart = mSystemStart
            , O.mChainTip = makeChainTip <$> mChainBlockNo <*> mChainPoint
            }

      mLocalState <- hushM (first ShelleyQueryCmdAcquireFailure eLocalState) $ \e ->
        liftIO . T.hPutStrLn IO.stderr $ "Warning: Local state unavailable: " <> renderShelleyQueryCmdError e

      chainTip <- pure (mLocalState >>= O.mChainTip)
        -- The chain tip is unavailable via local state query because we are connecting with an older
        -- node to client protocol so we use chain sync instead which necessitates another connection.
        -- At some point when we can stop supporting the older node to client protocols, this fallback
        -- can be removed.
        & onNothing (queryChainTipViaChainSync localNodeConnInfo)

      let tipSlotNo :: SlotNo = case chainTip of
            ChainTipAtGenesis -> 0
            ChainTip slotNo _ _ -> slotNo

      localStateOutput <- forM mLocalState $ \localState -> do
        case slotToEpoch tipSlotNo (O.eraHistory localState) of
          Left e -> do
            liftIO . T.hPutStrLn IO.stderr $
              "Warning: Epoch unavailable: " <> renderShelleyQueryCmdError (ShelleyQueryCmdPastHorizon e)
            return $ O.QueryTipLocalStateOutput
              { O.localStateChainTip = chainTip
              , O.mEra = Nothing
              , O.mEpoch = Nothing
              , O.mSyncProgress = Nothing
              , O.mSlotInEpoch = Nothing
              , O.mSlotsToEpochEnd = Nothing
              }

          Right (epochNo, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd) -> do
            syncProgressResult <- runExceptT $ do
              systemStart <- fmap getSystemStart (O.mSystemStart localState) & hoistMaybe ShelleyQueryCmdSystemStartUnavailable
              nowSeconds <- toRelativeTime (SystemStart systemStart) <$> liftIO getCurrentTime
              tipTimeResult <- getProgress tipSlotNo (O.eraHistory localState) & bimap ShelleyQueryCmdPastHorizon fst & except

              let tolerance = RelativeTime (secondsToNominalDiffTime 600)

              return $ flip (percentage tolerance) nowSeconds tipTimeResult

            mSyncProgress <- hushM syncProgressResult $ \e -> do
              liftIO . T.hPutStrLn IO.stderr $ "Warning: Sync progress unavailable: " <> renderShelleyQueryCmdError e

            return $ O.QueryTipLocalStateOutput
              { O.localStateChainTip = chainTip
              , O.mEra = Just (O.era localState)
              , O.mEpoch = Just epochNo
              , O.mSlotInEpoch = Just slotsInEpoch
              , O.mSlotsToEpochEnd = Just slotsToEpochEnd
              , O.mSyncProgress = mSyncProgress
              }

      case mOutFile of
        Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath $ encodePretty localStateOutput
        Nothing                 -> liftIO $ LBS.putStrLn        $ encodePretty localStateOutput

    mode -> left (ShelleyQueryCmdUnsupportedMode (AnyConsensusMode mode))

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--

runQueryUTxO
  :: AnyConsensusModeParams
  -> QueryUTxOFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO (AnyConsensusModeParams cModeParams)
             qfilter network mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- pure (toEraInMode era cMode)
    & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

  let query   = QueryInShelleyBasedEra sbe (QueryUTxO qfilter)
      qInMode = QueryInEra eInMode query

  result <- executeQuery era cModeParams localNodeConnInfo qInMode

  writeFilteredUTxOs sbe mOutFile result

runQueryKesPeriodInfo
  :: AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryKesPeriodInfo (AnyConsensusModeParams cModeParams) network nodeOpCertFile
                       mOutFile = do

  opCert <- lift (readFileTextEnvelope AsOperationalCertificate nodeOpCertFile)
    & onLeft (left . ShelleyQueryCmdOpCertCounterReadError)

  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era
  case cMode of
    CardanoMode -> do
      eInMode <- toEraInMode era cMode
        & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

      -- We check that the KES period specified in the operational certificate is correct
      -- based on the KES period defined in the genesis parameters and the current slot number
      let genesisQinMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryGenesisParameters
          eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra
      gParams <- executeQuery era cModeParams localNodeConnInfo genesisQinMode

      chainTip <- liftIO $ getLocalChainTip localNodeConnInfo

      let curKesPeriod = currentKesPeriod chainTip gParams
          oCertStartKesPeriod = opCertStartingKesPeriod opCert
          oCertEndKesPeriod = opCertEndKesPeriod gParams opCert
          opCertIntervalInformation = opCertIntervalInfo gParams chainTip curKesPeriod oCertStartKesPeriod oCertEndKesPeriod

      eraHistory <- lift (queryNodeLocalState localNodeConnInfo Nothing eraHistoryQuery)
        & onLeft (left . ShelleyQueryCmdAcquireFailure)

      let eInfo = toTentativeEpochInfo eraHistory


      -- We get the operational certificate counter from the protocol state and check that
      -- it is equivalent to what we have on disk.

      let ptclStateQinMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryProtocolState
      ptclState <- executeQuery era cModeParams localNodeConnInfo ptclStateQinMode

      (onDiskC, stateC) <- eligibleLeaderSlotsConstaints sbe $ opCertOnDiskAndStateCounters ptclState opCert
      let counterInformation = opCertNodeAndOnDiskCounters onDiskC stateC

      -- Always render diagnostic information
      liftIO . putStrLn $ renderOpCertIntervalInformation nodeOpCertFile opCertIntervalInformation
      liftIO . putStrLn $ renderOpCertNodeAndOnDiskCounterInformation nodeOpCertFile counterInformation

      let qKesInfoOutput = createQueryKesPeriodInfoOutput opCertIntervalInformation counterInformation eInfo gParams
          kesPeriodInfoJSON = encodePretty qKesInfoOutput

      liftIO $ LBS.putStrLn kesPeriodInfoJSON
      forM_ mOutFile (\(OutputFile oFp) ->
        handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError oFp)
          $ LBS.writeFile oFp kesPeriodInfoJSON)
    mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode
 where
   currentKesPeriod :: ChainTip -> GenesisParameters -> CurrentKesPeriod
   currentKesPeriod ChainTipAtGenesis _ = CurrentKesPeriod 0
   currentKesPeriod (ChainTip currSlot _ _) gParams =
     let slotsPerKesPeriod = fromIntegral $ protocolParamSlotsPerKESPeriod gParams
     in CurrentKesPeriod $ unSlotNo currSlot `div` slotsPerKesPeriod

   opCertStartingKesPeriod :: OperationalCertificate -> OpCertStartingKesPeriod
   opCertStartingKesPeriod = OpCertStartingKesPeriod . fromIntegral . getKesPeriod

   opCertEndKesPeriod :: GenesisParameters -> OperationalCertificate -> OpCertEndingKesPeriod
   opCertEndKesPeriod gParams oCert =
     let OpCertStartingKesPeriod start = opCertStartingKesPeriod oCert
         maxKesEvo = fromIntegral $ protocolParamMaxKESEvolutions gParams
     in OpCertEndingKesPeriod $ start + maxKesEvo

   -- See OCERT rule in Shelley Spec: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec
   opCertIntervalInfo
     :: GenesisParameters
     -> ChainTip
     -> CurrentKesPeriod
     -> OpCertStartingKesPeriod
     -> OpCertEndingKesPeriod
     -> OpCertIntervalInformation
   opCertIntervalInfo gParams currSlot' c s e@(OpCertEndingKesPeriod oCertEnd) =
       let cSlot = case currSlot' of
                       (ChainTip cSlotN _ _) -> unSlotNo cSlotN
                       ChainTipAtGenesis -> 0
           slotsTillExp = SlotsTillKesKeyExpiry . SlotNo $ (oCertEnd * fromIntegral (protocolParamSlotsPerKESPeriod gParams)) - cSlot
       in O.createOpCertIntervalInfo c s e (Just slotsTillExp)

   opCertNodeAndOnDiskCounters
     :: OpCertOnDiskCounter
     -> Maybe OpCertNodeStateCounter
     -> OpCertNodeAndOnDiskCounterInformation
   opCertNodeAndOnDiskCounters o@(OpCertOnDiskCounter odc) (Just n@(OpCertNodeStateCounter nsc))
     | odc < nsc = OpCertOnDiskCounterBehindNodeState o n
     | odc > nsc + 1 = OpCertOnDiskCounterTooFarAheadOfNodeState o n
     | odc == nsc + 1 = OpCertOnDiskCounterAheadOfNodeState o n
     | otherwise = OpCertOnDiskCounterEqualToNodeState o n
   opCertNodeAndOnDiskCounters o Nothing = OpCertNoBlocksMintedYet o

   opCertExpiryUtcTime
     :: Tentative (EpochInfo (Either Text))
     -> GenesisParameters
     -> OpCertEndingKesPeriod
     -> Maybe UTCTime
   opCertExpiryUtcTime eInfo gParams (OpCertEndingKesPeriod oCertExpiryKesPeriod) =
     let time = epochInfoSlotToUTCTime
                  (tentative eInfo)
                  (SystemStart $ protocolParamSystemStart gParams)
                  (fromIntegral $ oCertExpiryKesPeriod * fromIntegral (protocolParamSlotsPerKESPeriod gParams))
     in case time of
          Left _ -> Nothing
          Right t -> Just t

   renderOpCertNodeAndOnDiskCounterInformation :: FilePath -> OpCertNodeAndOnDiskCounterInformation -> String
   renderOpCertNodeAndOnDiskCounterInformation opCertFile opCertCounterInfo =
     case opCertCounterInfo of
      OpCertOnDiskCounterEqualToNodeState _ _ ->
        renderStringDefault $
          green "✓" <+> hang 0
              ( vsep
                [ "The operational certificate counter agrees with the node protocol state counter"
                ]
              )
      OpCertOnDiskCounterAheadOfNodeState _ _ ->
        renderStringDefault $
          green "✓" <+> hang 0
              ( vsep
                [ "The operational certificate counter ahead of the node protocol state counter by 1"
                ]
              )
      OpCertOnDiskCounterTooFarAheadOfNodeState onDiskC nodeStateC ->
        renderStringDefault $
          red "✗" <+> hang 0
            ( vsep
              [ "The operational certificate counter too far ahead of the node protocol state counter in the operational certificate at: " <> pretty opCertFile
              , "On disk operational certificate counter: " <> pretty (unOpCertOnDiskCounter onDiskC)
              , "Protocol state counter: " <> pretty (unOpCertNodeStateCounter nodeStateC)
              ]
            )
      OpCertOnDiskCounterBehindNodeState onDiskC nodeStateC ->
        renderStringDefault $
          red "✗" <+> hang 0
            ( vsep
              [ "The protocol state counter is greater than the counter in the operational certificate at: " <> pretty opCertFile
              , "On disk operational certificate counter: " <> pretty (unOpCertOnDiskCounter onDiskC)
              , "Protocol state counter: " <> pretty (unOpCertNodeStateCounter nodeStateC)
              ]
            )
      OpCertNoBlocksMintedYet (OpCertOnDiskCounter onDiskC) ->
        renderStringDefault $
          red "✗" <+> hang 0
            ( vsep
              [ "No blocks minted so far with the operational certificate at: " <> pretty opCertFile
              , "On disk operational certificate counter: " <> pretty onDiskC
              ]
            )


   createQueryKesPeriodInfoOutput
     :: OpCertIntervalInformation
     -> OpCertNodeAndOnDiskCounterInformation
     -> Tentative (EpochInfo (Either Text))
     -> GenesisParameters
     -> O.QueryKesPeriodInfoOutput
   createQueryKesPeriodInfoOutput oCertIntervalInfo oCertCounterInfo eInfo gParams  =
     let (e, mStillExp) = case oCertIntervalInfo of
                            OpCertWithinInterval _ end _ sTillExp -> (end, Just sTillExp)
                            OpCertStartingKesPeriodIsInTheFuture _ end _ -> (end, Nothing)
                            OpCertExpired _ end _ -> (end, Nothing)
                            OpCertSomeOtherError _ end _ -> (end, Nothing)
         (onDiskCounter, mNodeCounter) = case oCertCounterInfo of
                                           OpCertOnDiskCounterEqualToNodeState d n -> (d, Just n)
                                           OpCertOnDiskCounterAheadOfNodeState d n -> (d, Just n)
                                           OpCertOnDiskCounterTooFarAheadOfNodeState d n -> (d, Just n)
                                           OpCertOnDiskCounterBehindNodeState d n -> (d, Just n)
                                           OpCertNoBlocksMintedYet d -> (d, Nothing)

     in O.QueryKesPeriodInfoOutput
        { O.qKesOpCertIntervalInformation = oCertIntervalInfo
        , O.qKesInfoNodeStateOperationalCertNo = mNodeCounter
        , O.qKesInfoOnDiskOperationalCertNo = onDiskCounter
        , O.qKesInfoMaxKesKeyEvolutions = fromIntegral $ protocolParamMaxKESEvolutions gParams
        , O.qKesInfoSlotsPerKesPeriod = fromIntegral $ protocolParamSlotsPerKESPeriod gParams
        , O.qKesInfoKesKeyExpiry =
            case mStillExp of
              Just _ -> opCertExpiryUtcTime eInfo gParams e
              Nothing -> Nothing
        }

   -- We get the operational certificate counter from the protocol state and check that
   -- it is equivalent to what we have on disk.
   opCertOnDiskAndStateCounters :: forall era . ()
      => Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
      => FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
      => Crypto.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
      => ProtocolState era
      -> OperationalCertificate
      -> ExceptT ShelleyQueryCmdError IO (OpCertOnDiskCounter, Maybe OpCertNodeStateCounter)
   opCertOnDiskAndStateCounters ptclState opCert@(OperationalCertificate _ stakePoolVKey) = do
    let onDiskOpCertCount = fromIntegral $ getOpCertCount opCert

    chainDepState <- pure (decodeProtocolState ptclState)
      & onLeft (left . ShelleyQueryCmdProtocolStateDecodeFailure)

    -- We need the stake pool id to determine what the counter of our SPO
    -- should be.
    let opCertCounterMap = Consensus.getOpCertCounters (Proxy @(ConsensusProtocol era)) chainDepState
        StakePoolKeyHash blockIssuerHash = verificationKeyHash stakePoolVKey

    case Map.lookup (coerce blockIssuerHash) opCertCounterMap of
      -- Operational certificate exists in the protocol state
      -- so our ondisk op cert counter must be greater than or
      -- equal to what is in the node state
      Just ptclStateCounter -> return (OpCertOnDiskCounter onDiskOpCertCount, Just $ OpCertNodeStateCounter ptclStateCounter)
      Nothing -> return (OpCertOnDiskCounter onDiskOpCertCount, Nothing)


renderOpCertIntervalInformation :: FilePath -> OpCertIntervalInformation -> String
renderOpCertIntervalInformation opCertFile opCertInfo = case opCertInfo of
  OpCertWithinInterval _start _end _current _stillExp ->
    renderStringDefault $
      green "✓" <+> hang 0
        ( vsep
          [ "Operational certificate's KES period is within the correct KES period interval"
          ]
        )
  OpCertStartingKesPeriodIsInTheFuture (OpCertStartingKesPeriod start) (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    renderStringDefault $
      red "✗" <+> hang 0
        ( vsep
          [ "Node operational certificate at: " <> pretty opCertFile <> " has an incorrectly specified starting KES period. "
          , "Current KES period: " <> pretty current
          , "Operational certificate's starting KES period: " <> pretty start
          , "Operational certificate's expiry KES period: " <> pretty end
          ]
        )
  OpCertExpired _ (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    renderStringDefault $
      red "✗" <+> hang 0
        ( vsep
          [ "Node operational certificate at: " <> pretty opCertFile <> " has expired. "
          , "Current KES period: " <> pretty current
          , "Operational certificate's expiry KES period: " <> pretty end
          ]
        )

  OpCertSomeOtherError (OpCertStartingKesPeriod start) (OpCertEndingKesPeriod end) (CurrentKesPeriod current) ->
    renderStringDefault $
      red "✗" <+> hang 0
        ( vsep
          [ "An unknown error occurred with operational certificate at: " <> pretty opCertFile
          , "Current KES period: " <> pretty current
          , "Operational certificate's starting KES period: " <> pretty start
          , "Operational certificate's expiry KES period: " <> pretty end
          ]
        )

-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--
runQueryPoolState
  :: AnyConsensusModeParams
  -> NetworkId
  -> [Hash StakePoolKey]
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryPoolState (AnyConsensusModeParams cModeParams) network poolIds = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryPoolState $ Just $ Set.fromList poolIds
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe writePoolState result

-- | Query the local mempool state
runQueryTxMempool
  :: AnyConsensusModeParams
  -> NetworkId
  -> TxMempoolQuery
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTxMempool (AnyConsensusModeParams cModeParams) network query mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  localQuery <- case query of
      TxMempoolQueryTxExists tx -> do
        anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
          & onLeft (left . ShelleyQueryCmdAcquireFailure)
        let cMode = consensusModeOnly cModeParams
        eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)
        pure $ LocalTxMonitoringQueryTx $ TxIdInMode tx eInMode
      TxMempoolQueryNextTx -> pure LocalTxMonitoringSendNextTx
      TxMempoolQueryInfo -> pure LocalTxMonitoringMempoolInformation

  result <- liftIO $ queryTxMonitoringLocal localNodeConnInfo localQuery
  let renderedResult = encodePretty result
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn renderedResult
    Just (OutputFile oFp) -> handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError oFp)
        $ LBS.writeFile oFp renderedResult


-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshot
  :: AnyConsensusModeParams
  -> NetworkId
  -> AllOrOnly [Hash StakePoolKey]
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeSnapshot (AnyConsensusModeParams cModeParams) network allOrOnlyPoolIds mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakeSnapshot $ case allOrOnlyPoolIds of
        All -> Nothing
        Only poolIds -> Just $ Set.fromList poolIds

  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writeStakeSnapshots mOutFile) result


runQueryLedgerState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState (AnyConsensusModeParams cModeParams)
                    network mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- pure (toEraInMode era cMode)
    & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryDebugLedgerState

  result <- executeQuery era cModeParams localNodeConnInfo qInMode

  obtainLedgerEraClassConstraints sbe (writeLedgerState mOutFile) result

runQueryProtocolState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolState (AnyConsensusModeParams cModeParams)
                      network mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- pure (toEraInMode era cMode)
    & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryProtocolState

  result <- executeQuery era cModeParams localNodeConnInfo qInMode

  case cMode of
    CardanoMode -> eligibleWriteProtocolStateConstaints sbe $ writeProtocolState mOutFile result
    mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runQueryStakeAddressInfo
  :: AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo (AnyConsensusModeParams cModeParams)
                         (StakeAddress _ addr) network mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- pure (toEraInMode era cMode)
    & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

  let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr
      query = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakeAddresses stakeAddr network

  result <- executeQuery era cModeParams localNodeConnInfo query

  writeStakeAddressInfo mOutFile $ DelegationsAndRewards result

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
newtype ShelleyQueryCmdLocalStateQueryError
  = EraMismatchError EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  deriving (Eq, Show)

renderLocalStateQueryError :: ShelleyQueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> textShow err

writeStakeAddressInfo
  :: Maybe OutputFile
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile delegsAndRewards =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty delegsAndRewards)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty delegsAndRewards)

writeLedgerState :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => ToJSON (DebugLedgerState era)
                 => FromCBOR (DebugLedgerState era)
                 => Maybe OutputFile
                 -> SerialisedDebugLedgerState era
                 -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile qState@(SerialisedDebugLedgerState serLedgerState) =
  case mOutFile of
    Nothing ->
      case decodeDebugLedgerState qState of
        Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
        Right ledgerState -> liftIO . LBS.putStrLn $ Aeson.encode ledgerState
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState

writeStakeSnapshots :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Era.Crypto ledgerera ~ StandardCrypto
  => Maybe OutputFile
  -> SerialisedStakeSnapshots era
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeSnapshots mOutFile qState = do
  StakeSnapshot snapshot <- pure (decodeStakeSnapshot qState)
    & onLeft (left . ShelleyQueryCmdStakeSnapshotDecodeError)

  -- Calculate the three pool and active stake values for the given pool
  liftIO . maybe LBS.putStrLn (LBS.writeFile . unOutputFile) mOutFile $ encodePretty snapshot

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState._delegationState._pstate._pParams.<pool_id>
writePoolState :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Era.Crypto ledgerera ~ StandardCrypto
  => Ledger.Era ledgerera
  => SerialisedPoolState era
  -> ExceptT ShelleyQueryCmdError IO ()
writePoolState serialisedCurrentEpochState = do
  PoolState poolState <- pure (decodePoolState serialisedCurrentEpochState)
    & onLeft (left . ShelleyQueryCmdPoolStateDecodeError)

  let hks = Set.toList $ Set.fromList $ Map.keys (_pParams poolState) <> Map.keys (_fPParams poolState) <> Map.keys (_retiring poolState)

  let poolStates :: Map (KeyHash 'StakePool StandardCrypto) (Params StandardCrypto)
      poolStates = Map.fromList $ hks <&>
        ( \hk ->
          ( hk
          , Params
            { poolParameters        = Map.lookup hk (SL._pParams  poolState)
            , futurePoolParameters  = Map.lookup hk (SL._fPParams poolState)
            , retiringEpoch         = Map.lookup hk (SL._retiring poolState)
            }
          )
        )

  liftIO . LBS.putStrLn $ encodePretty poolStates

writeProtocolState ::
  ( FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  )
  => Maybe OutputFile
  -> ProtocolState era
  -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile ps@(ProtocolState pstate) =
  case mOutFile of
    Nothing -> case decodeProtocolState ps of
      Left (bs, _) -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
      Right chainDepstate -> liftIO . LBS.putStrLn $ encodePretty chainDepstate
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        . LBS.writeFile fpath $ unSerialised pstate

writeFilteredUTxOs :: Api.ShelleyBasedEra era
                   -> Maybe OutputFile
                   -> UTxO era
                   -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs shelleyBasedEra' mOutFile utxo =
  case mOutFile of
    Nothing -> liftIO $ printFilteredUTxOs shelleyBasedEra' utxo
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath
        $ getIsCardanoEraConstraint (shelleyBasedToCardanoEra shelleyBasedEra')
        $ encodePretty utxo

printFilteredUTxOs :: Api.ShelleyBasedEra era -> UTxO era -> IO ()
printFilteredUTxOs shelleyBasedEra' (UTxO utxo) = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraAllegra ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraMary    ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraAlonzo ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraBabbage ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraConway ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo

 where
   title :: Text
   title =
     "                           TxHash                                 TxIx        Amount"

printUtxo
  :: Api.ShelleyBasedEra era
  -> (TxIn, TxOut CtxUTxO era)
  -> IO ()
printUtxo shelleyBasedEra' txInOutTuple =
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]

    ShelleyBasedEraAllegra ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraMary ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _ _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraAlonzo ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
             ]
    ShelleyBasedEraBabbage ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
             ]
    ShelleyBasedEraConway ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
             ]
 where
  textShowN :: Show a => Int -> a -> Text
  textShowN len x =
    let str = show x
        slen = length str
    in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

  printableValue :: TxOutValue era -> Text
  printableValue (TxOutValue _ val) = renderValue val
  printableValue (TxOutAdaOnly _ (Lovelace i)) = Text.pack $ show i

runQueryStakePools
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakePools (AnyConsensusModeParams cModeParams)
                          network mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  poolIds <-
    ( lift $ executeLocalStateQueryExpr localNodeConnInfo Nothing $ runExceptT @ShelleyQueryCmdError $ do
        anyE@(AnyCardanoEra era) <- case consensusModeOnly cModeParams of
          ByronMode -> return $ AnyCardanoEra ByronEra
          ShelleyMode -> return $ AnyCardanoEra ShelleyEra
          CardanoMode ->
            lift (queryExpr $ QueryCurrentEra CardanoModeIsMultiEra)
              & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)

        let cMode = consensusModeOnly cModeParams

        eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

        sbe <- getSbe $ cardanoEraStyle era

        lift (queryExpr (QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryStakePools))
          & onLeft (left . ShelleyQueryCmdUnsupportedNtcVersion)
          & onLeft (left . ShelleyQueryCmdEraMismatch)
    ) & onLeft (left . ShelleyQueryCmdAcquireFailure)
      & onLeft left

  writeStakePools mOutFile poolIds

writeStakePools
  :: Maybe OutputFile
  -> Set PoolId
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakePools (Just (OutputFile outFile)) stakePools =
  handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakePools)

writeStakePools Nothing stakePools =
  forM_ (Set.toList stakePools) $ \poolId ->
    liftIO . putStrLn $ Text.unpack (serialiseToBech32 poolId)

runQueryStakeDistribution
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution (AnyConsensusModeParams cModeParams)
                          network mOutFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- pure (toEraInMode era cMode)
    & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE))

  let query = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakeDistribution

  result <- executeQuery era cModeParams localNodeConnInfo query

  writeStakeDistribution mOutFile result

writeStakeDistribution
  :: Maybe OutputFile
  -> Map PoolId Rational
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) stakeDistrib =
  handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakeDistrib)

writeStakeDistribution Nothing stakeDistrib =
  liftIO $ printStakeDistribution stakeDistrib


printStakeDistribution :: Map PoolId Rational -> IO ()
printStakeDistribution stakeDistrib = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  sequence_
    [ putStrLn $ showStakeDistr poolId stakeFraction
    | (poolId, stakeFraction) <- Map.toList stakeDistrib ]
 where
   title :: Text
   title =
     "                           PoolId                                 Stake frac"

   showStakeDistr :: PoolId
                  -> Rational
                  -- ^ Stake fraction
                  -> String
   showStakeDistr poolId stakeFraction =
     concat
       [ Text.unpack (serialiseToBech32 poolId)
       , "   "
       , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
       ]

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
-- TODO: Move to cardano-api
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress Lovelace, Map StakeAddress PoolId)
    deriving (Eq, Show)


mergeDelegsAndRewards :: DelegationsAndRewards -> [(StakeAddress, Maybe Lovelace, Maybe PoolId)]
mergeDelegsAndRewards (DelegationsAndRewards (rewardsMap, delegMap)) =
 [ (stakeAddr, Map.lookup stakeAddr rewardsMap, Map.lookup stakeAddr delegMap)
 | stakeAddr <- nub $ Map.keys rewardsMap ++ Map.keys delegMap
 ]


instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ mergeDelegsAndRewards delegsAndRwds
    where
      delegAndRwdToJson :: (StakeAddress, Maybe Lovelace, Maybe PoolId) -> Aeson.Value
      delegAndRwdToJson (addr, mRewards, mPoolId) =
        Aeson.object
          [ "address" .= addr
          , "delegation" .= mPoolId
          , "rewardAccountBalance" .= mRewards
          ]

instance FromJSON DelegationsAndRewards where
  parseJSON = withArray "DelegationsAndRewards" $ \arr -> do
    let vals = Vector.toList arr
    decoded <- mapM decodeObject vals
    pure $ zipper decoded
   where
     zipper :: [(StakeAddress, Maybe Lovelace, Maybe PoolId)]
             -> DelegationsAndRewards
     zipper l = do
       let maps = [ ( maybe mempty (Map.singleton sa) delegAmt
                    , maybe mempty (Map.singleton sa) mPool
                    )
                  | (sa, delegAmt, mPool) <- l
                  ]
       DelegationsAndRewards
         $ foldl
             (\(amtA, delegA) (amtB, delegB) -> (amtA <> amtB, delegA <> delegB))
             (mempty, mempty)
             maps

     decodeObject :: Aeson.Value
                  -> Aeson.Parser (StakeAddress, Maybe Lovelace, Maybe PoolId)
     decodeObject  = withObject "DelegationsAndRewards" $ \o -> do
       address <- o .: "address"
       delegation <- o .:? "delegation"
       rewardAccountBalance <- o .:? "rewardAccountBalance"
       pure (address, rewardAccountBalance, delegation)

runQueryLeadershipSchedule
  :: AnyConsensusModeParams
  -> NetworkId
  -> GenesisFile -- ^ Shelley genesis
  -> VerificationKeyOrHashOrFile StakePoolKey
  -> SigningKeyFile -- ^ VRF signing key
  -> EpochLeadershipSchedule
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLeadershipSchedule (AnyConsensusModeParams cModeParams) network
                           (GenesisFile genFile) coldVerKeyFile (SigningKeyFile vrfSkeyFp)
                           whichSchedule mJsonOutputFile = do
  SocketPath sockPath <- lift readEnvSocketPath & onLeft (left . ShelleyQueryCmdEnvVarSocketErr)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- lift (determineEra cModeParams localNodeConnInfo)
    & onLeft (left . ShelleyQueryCmdAcquireFailure)

  sbe <- getSbe (cardanoEraStyle era)

  let cMode = consensusModeOnly cModeParams

  poolid <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey coldVerKeyFile)
    & onLeft (left . ShelleyQueryCmdTextReadError)

  vrkSkey <- lift (readFileTextEnvelope (AsSigningKey AsVrfKey) vrfSkeyFp)
    & onLeft (left . ShelleyQueryCmdTextEnvelopeReadError)

  shelleyGenesis <- lift (readAndDecodeShelleyGenesis genFile)
    & onLeft (left . ShelleyQueryCmdGenesisReadError)

  case cMode of
    CardanoMode -> do
      eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

      let pparamsQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
          ptclStateQuery = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryProtocolState
          eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra

      pparams <- executeQuery era cModeParams localNodeConnInfo pparamsQuery
      ptclState <- executeQuery era cModeParams localNodeConnInfo ptclStateQuery
      eraHistory <- lift (queryNodeLocalState localNodeConnInfo Nothing eraHistoryQuery)
        & onLeft (left . ShelleyQueryCmdAcquireFailure)

      let eInfo = toEpochInfo eraHistory
      let currentEpochQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryEpoch
      curentEpoch <- executeQuery era cModeParams localNodeConnInfo currentEpochQuery

      let bpp = bundleProtocolParams era pparams

      schedule <- case whichSchedule of
        CurrentEpoch -> do
          serCurrentEpochState <- executeQuery era cModeParams localNodeConnInfo $
            QueryInEra eInMode $ QueryInShelleyBasedEra sbe (QueryPoolDistribution (Just (Set.singleton poolid)))
          firstExceptT ShelleyQueryCmdLeaderShipError $ hoistEither
            $ eligibleLeaderSlotsConstaints sbe
            $ currentEpochEligibleLeadershipSlots
              sbe
              shelleyGenesis
              eInfo
              bpp
              ptclState
              poolid
              vrkSkey
              serCurrentEpochState
              curentEpoch

        NextEpoch -> do
          let currentEpochStateQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryCurrentEpochState

          tip <- liftIO $ getLocalChainTip localNodeConnInfo
          serCurrentEpochState <- executeQuery era cModeParams localNodeConnInfo currentEpochStateQuery

          firstExceptT ShelleyQueryCmdLeaderShipError $ hoistEither
            $ eligibleLeaderSlotsConstaints sbe
            $ nextEpochEligibleLeadershipSlots sbe shelleyGenesis
              serCurrentEpochState ptclState poolid vrkSkey bpp
              eInfo (tip, curentEpoch)

      case mJsonOutputFile of
        Nothing -> liftIO $ printLeadershipScheduleAsText schedule eInfo (SystemStart $ sgSystemStart shelleyGenesis)
        Just (OutputFile jsonOutputFile) ->
          liftIO $ LBS.writeFile jsonOutputFile $
            printLeadershipScheduleAsJson schedule eInfo (SystemStart $ sgSystemStart shelleyGenesis)
    mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode
 where
  printLeadershipScheduleAsText
    :: Set SlotNo
    -> EpochInfo (Either Text)
    -> SystemStart
    -> IO ()
  printLeadershipScheduleAsText leadershipSlots eInfo sStart = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    sequence_
      [ putStrLn $ showLeadershipSlot slot eInfo sStart
      | slot <- Set.toList leadershipSlots ]
   where
     title :: Text
     title =
       "     SlotNo                          UTC Time              "

     showLeadershipSlot
       :: SlotNo
       -> EpochInfo (Either Text)
       -> SystemStart
       -> String
     showLeadershipSlot lSlot@(SlotNo sn) eInfo' sStart' =
       case epochInfoSlotToUTCTime eInfo' sStart' lSlot of
         Right slotTime ->
          concat
           [ "     "
           , show sn
           , "                   "
           , show slotTime
           ]
         Left err ->
          concat
           [ "     "
           , show sn
           , "                   "
           , Text.unpack err
           ]
  printLeadershipScheduleAsJson
    :: Set SlotNo
    -> EpochInfo (Either Text)
    -> SystemStart
    -> LBS.ByteString
  printLeadershipScheduleAsJson leadershipSlots eInfo sStart =
    encodePretty $ showLeadershipSlot <$> List.sort (Set.toList leadershipSlots)
    where
      showLeadershipSlot :: SlotNo -> Aeson.Value
      showLeadershipSlot lSlot@(SlotNo sn) =
        case epochInfoSlotToUTCTime eInfo sStart lSlot of
          Right slotTime ->
            Aeson.object
              [ "slotNumber" Aeson..= sn
              , "slotTime" Aeson..= slotTime
              ]
          Left err ->
            Aeson.object
              [ "slotNumber" Aeson..= sn
              , "error" Aeson..= Text.unpack err
              ]


-- Helpers

calcEraInMode
  :: CardanoEra era
  -> ConsensusMode mode
  -> ExceptT ShelleyQueryCmdError IO (EraInMode era mode)
calcEraInMode era mode =
  pure (toEraInMode era mode)
    & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyCardanoEra era)))

executeQuery
  :: forall result era mode. CardanoEra era
  -> ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> ExceptT ShelleyQueryCmdError IO result
executeQuery era cModeP localNodeConnInfo q = do
  eraInMode <- calcEraInMode era $ consensusModeOnly cModeP
  case eraInMode of
    ByronEraInByronMode -> left ShelleyQueryCmdByronEra
    _ -> liftIO execQuery >>= queryResult
 where
   execQuery :: IO (Either AcquiringFailure (Either EraMismatch result))
   execQuery = queryNodeLocalState localNodeConnInfo Nothing q

getSbe :: Monad m => CardanoEraStyle era -> ExceptT ShelleyQueryCmdError m (Api.ShelleyBasedEra era)
getSbe LegacyByronEra = left ShelleyQueryCmdByronEra
getSbe (Api.ShelleyBasedEra sbe) = return sbe

queryResult
  :: Either AcquiringFailure (Either EraMismatch a)
  -> ExceptT ShelleyQueryCmdError IO a
queryResult eAcq = pure eAcq
  & onLeft (left . ShelleyQueryCmdAcquireFailure)
  & onLeft (left . ShelleyQueryCmdLocalStateQueryError . EraMismatchError)

toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Either Text)
toEpochInfo (EraHistory _ interpreter) =
  hoistEpochInfo (first (Text.pack . show) . runExcept)
    $ Consensus.interpreterToEpochInfo interpreter

-- | A value that is tentative or produces a tentative value if used.  These values
-- are considered accurate only if some future event such as a hard fork does not
-- render them invalid.
newtype Tentative a = Tentative { tentative :: a } deriving (Eq, Show)

-- | Get an Epoch Info that computes tentative values.  The values computed are
-- tentative because it uses an interpreter that is extended past the horizon.
-- This interpreter will compute accurate values into the future as long as a
-- a hard fork does not happen in the intervening time.  Those values are thus
-- "tentative" because they can change in the event of a hard fork.
toTentativeEpochInfo :: EraHistory CardanoMode -> Tentative (EpochInfo (Either Text))
toTentativeEpochInfo (EraHistory _ interpreter) =
  Tentative
    $ hoistEpochInfo (first (Text.pack . show) . runExcept)
    $ Consensus.interpreterToEpochInfo (Consensus.unsafeExtendSafeZone interpreter)

obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => Api.ShelleyBasedEra era
  -> (( ToJSON (DebugLedgerState era)
      , FromCBOR (DebugLedgerState era)
      , Era.Crypto ledgerera ~ StandardCrypto
      , Ledger.Era (ShelleyLedgerEra era)
      ) => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAlonzo  f = f
obtainLedgerEraClassConstraints ShelleyBasedEraBabbage f = f
obtainLedgerEraClassConstraints ShelleyBasedEraConway  f = f


eligibleLeaderSlotsConstaints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (( ShelleyLedgerEra era ~ ledgerera
      , Ledger.Crypto ledgerera ~ StandardCrypto
      , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
      , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
      , Era.Era ledgerera
      , HasField "_d" (Core.PParams (ShelleyLedgerEra era)) UnitInterval
      , Crypto.Signable (Crypto.VRF (Ledger.Crypto ledgerera)) Seed
      , Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Ledger.Credential 'Staking StandardCrypto)
      , Crypto.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
      , HashAnnotated
          (Core.TxBody (ShelleyLedgerEra era))
          Core.EraIndependentTxBody
          StandardCrypto
      ) => a
     )
  -> a
eligibleLeaderSlotsConstaints ShelleyBasedEraShelley f = f
eligibleLeaderSlotsConstaints ShelleyBasedEraAllegra f = f
eligibleLeaderSlotsConstaints ShelleyBasedEraMary    f = f
eligibleLeaderSlotsConstaints ShelleyBasedEraAlonzo  f = f
eligibleLeaderSlotsConstaints ShelleyBasedEraBabbage f = f
eligibleLeaderSlotsConstaints ShelleyBasedEraConway  f = f

eligibleWriteProtocolStateConstaints
  :: ShelleyBasedEra era
  -> (( FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
      , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
      ) => a
     )
  -> a
eligibleWriteProtocolStateConstaints ShelleyBasedEraShelley f = f
eligibleWriteProtocolStateConstaints ShelleyBasedEraAllegra f = f
eligibleWriteProtocolStateConstaints ShelleyBasedEraMary    f = f
eligibleWriteProtocolStateConstaints ShelleyBasedEraAlonzo  f = f
eligibleWriteProtocolStateConstaints ShelleyBasedEraBabbage f = f
eligibleWriteProtocolStateConstaints ShelleyBasedEraConway  f = f

-- Required instances
-- instance FromCBOR (TPraosState StandardCrypto) where
-- instance FromCBOR (Praos.PraosState StandardCrypto) where
