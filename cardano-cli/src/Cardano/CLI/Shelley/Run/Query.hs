{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLI.Shelley.Run.Query
  ( DelegationsAndRewards(..)
  , ShelleyQueryCmdError
  , ShelleyQueryCmdLocalStateQueryError (..)
  , renderShelleyQueryCmdError
  , renderLocalStateQueryError
  , runQueryCmd
  , mergeDelegsAndRewards
  , percentage
  , executeQuery
  ) where

import           Cardano.Prelude
import           Prelude (String, id)

import           Cardano.Api
import qualified Cardano.Api as Api
import           Cardano.Api.Byron
import           Cardano.Api.Shelley
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError (..), hushM, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key
import           Cardano.CLI.Shelley.Orphans ()
import qualified Cardano.CLI.Shelley.Output as O
import           Cardano.CLI.Shelley.Run.Genesis (ShelleyGenesisCmdError,
                   readAndDecodeShelleyGenesis)
import           Cardano.CLI.Types
import           Cardano.Crypto.Hash (hashToBytesAsHex)
import qualified Cardano.Crypto.VRF as Crypto
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import           Cardano.Ledger.BaseTypes (Seed, UnitInterval)
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.Shelley.Constraints
import           Cardano.Ledger.Shelley.EpochBoundary
import           Cardano.Ledger.Shelley.LedgerState (DPState (_pstate),
                   EpochState (esLState, esSnapshots), LedgerState (_delegationState),
                   NewEpochState (nesEs), PState (_fPParams, _pParams, _retiring))
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import           Cardano.Ledger.Shelley.Scripts ()
import qualified Cardano.Protocol.TPraos.API as Ledger
import           Cardano.Slotting.EpochInfo (EpochInfo (..), epochInfoSlotToUTCTime, hoistEpochInfo)
import           Control.Monad.Trans.Except (except)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   hoistMaybe, left, newExceptT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types as Aeson
import           Data.Coerce (coerce)
import           Data.List (nub)
import           Data.Sharing (Interns, Share)
import           Data.Time.Clock
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   SystemStart (..), toRelativeTime)
import           Ouroboros.Consensus.Cardano.Block as Consensus (EraMismatch (..))
import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import           Text.Printf (printf)

import           Cardano.Protocol.TPraos.Rules.Prtcl
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Compact.VMap as VMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as T
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import           Numeric (showEFloat)
import qualified Ouroboros.Consensus.HardFork.History as Consensus

import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified System.IO as IO


{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdAcquireFailure !AcquireFailure
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | ShelleyQueryCmdByronEra
  | ShelleyQueryCmdPoolIdError (Hash StakePoolKey)
  | ShelleyQueryCmdEraMismatch !EraMismatch
  | ShelleyQueryCmdUnsupportedMode !AnyConsensusMode
  | ShelleyQueryCmdPastHorizon !Qry.PastHorizonException
  | ShelleyQueryCmdSystemStartUnavailable
  | ShelleyQueryCmdGenesisReadError !ShelleyGenesisCmdError
  | ShelleyQueryCmdLeaderShipError !LeadershipError
  | ShelleyQueryCmdTextEnvelopeReadError !(FileError TextEnvelopeError)
  | ShelleyQueryCmdTextReadError !(FileError InputDecodeError )
  | ShelleyQueryCmdColdKeyReadFileError !(FileError InputDecodeError)
  | ShelleyQueryCmdOpCertCounterReadError !(FileError TextEnvelopeError)
  | ShelleyQueryCmdProtocolStateDecodeFailure
  | ShelleyQueryCmdSlotToUtcError Text
  | ShelleyQueryCmdNodeUnknownStakePool
      FilePath
      -- ^ Operational certificate of the unknown stake pool.

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
    ShelleyQueryCmdPoolIdError poolId -> "The pool id does not exist: " <> show poolId
    ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cMode <>
      " Era: " <> show era
    ShelleyQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occurred." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra
    ShelleyQueryCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyQueryCmdPastHorizon e -> "Past horizon: " <> show e
    ShelleyQueryCmdSystemStartUnavailable -> "System start unavailable"
    ShelleyQueryCmdGenesisReadError err' -> Text.pack $ displayError err'
    ShelleyQueryCmdLeaderShipError e -> Text.pack $ displayError e
    ShelleyQueryCmdTextEnvelopeReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdSlotToUtcError e -> "Failed to convert slot to UTC time: " <> e
    ShelleyQueryCmdTextReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdColdKeyReadFileError e -> Text.pack $ displayError e
    ShelleyQueryCmdOpCertCounterReadError e -> Text.pack $ displayError e
    ShelleyQueryCmdProtocolStateDecodeFailure -> "Failed to decode the protocol state."
    ShelleyQueryCmdNodeUnknownStakePool nodeOpCert ->
      Text.pack $ "The stake pool associated with: " <> nodeOpCert <> " was not found. Ensure the correct KES key has been " <>
                  "specified and that the stake pool is registered. If you have submitted a stake pool registration certificate " <>
                  "in the current epoch, you must wait until the following epoch for the registration to take place."


runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryLeadershipSchedule consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule ->
      runQueryLeadershipSchedule consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule
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
    QueryStakeSnapshot' consensusModeParams network poolid ->
      runQueryStakeSnapshot consensusModeParams network poolid
    QueryPoolParams' consensusModeParams network poolid ->
      runQueryPoolParams consensusModeParams network poolid
    QueryProtocolState' consensusModeParams network mOutFile ->
      runQueryProtocolState consensusModeParams network mOutFile
    QueryUTxO' consensusModeParams qFilter networkId mOutFile ->
      runQueryUTxO consensusModeParams qFilter networkId mOutFile
    QueryKesPeriodInfo consensusModeParams network nodeOpCert mOutFile ->
      runQueryKesPeriodInfo consensusModeParams network nodeOpCert mOutFile

runQueryProtocolParameters
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr
                           readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  result <- liftIO $ executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
    anyE@(AnyCardanoEra era) <- lift $ determineEraExpr cModeParams

    case cardanoEraStyle era of
      LegacyByronEra -> left ShelleyQueryCmdByronEra
      ShelleyBasedEra sbe -> do
        let cMode = consensusModeOnly cModeParams

        eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

        ppResult <- lift . queryExpr $ QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

        except ppResult & firstExceptT ShelleyQueryCmdEraMismatch

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
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath

  case consensusModeOnly cModeParams of
    CardanoMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

      eLocalState <- liftIO $ executeLocalStateQueryExpr localNodeConnInfo Nothing $ \ntcVersion -> do
        era <- queryExpr (QueryCurrentEra CardanoModeIsMultiEra)
        eraHistory <- queryExpr (QueryEraHistory CardanoModeIsMultiEra)
        mChainBlockNo <- if ntcVersion >= NodeToClientV_10
          then Just <$> queryExpr QueryChainBlockNo
          else return Nothing
        mChainPoint <- if ntcVersion >= NodeToClientV_10
          then Just <$> queryExpr (QueryChainPoint CardanoMode)
          else return Nothing
        mSystemStart <- if ntcVersion >= NodeToClientV_9
          then Just <$> queryExpr QuerySystemStart
          else return Nothing

        return O.QueryTipLocalState
          { O.era = era
          , O.eraHistory = eraHistory
          , O.mSystemStart = mSystemStart
          , O.mChainTip = makeChainTip <$> mChainBlockNo <*> mChainPoint
          }

      mLocalState <- hushM (first ShelleyQueryCmdAcquireFailure eLocalState) $ \e ->
        liftIO . T.hPutStrLn IO.stderr $ "Warning: Local state unavailable: " <> renderShelleyQueryCmdError e

      chainTip <- case mLocalState >>= O.mChainTip of
        Just chainTip -> return chainTip

        -- The chain tip is unavailable via local state query because we are connecting with an older
        -- node to client protocol so we use chain sync instead which necessitates another connection.
        -- At some point when we can stop supporting the older node to client protocols, this fallback
        -- can be removed.
        Nothing -> queryChainTipViaChainSync localNodeConnInfo

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
              }

          Right (epochNo, _, _) -> do
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
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query   = QueryInShelleyBasedEra sbe (QueryUTxO qfilter)
          qInMode = QueryInEra eInMode query
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      writeFilteredUTxOs sbe mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


runQueryKesPeriodInfo
  :: AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryKesPeriodInfo (AnyConsensusModeParams cModeParams) network nodeOpCertFile
                       mOutFile = do

  opCert <- firstExceptT ShelleyQueryCmdOpCertCounterReadError
              . newExceptT $ readFileTextEnvelope AsOperationalCertificate nodeOpCertFile

  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
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
      gParams@GenesisParameters{protocolParamMaxKESEvolutions, protocolParamSystemStart, protocolParamSlotsPerKESPeriod}
        <- executeQuery era cModeParams localNodeConnInfo genesisQinMode

      chainTip <- liftIO $ getLocalChainTip localNodeConnInfo

      (slotsTillNewKesKey, currentKesPeriod, kesIntervalStart, kesIntervalEnd, periodCheckDiag)
        <- opCertKesPeriodCheck opCert chainTip gParams
      eraHistory <- firstExceptT ShelleyQueryCmdAcquireFailure . newExceptT $ queryNodeLocalState localNodeConnInfo Nothing eraHistoryQuery

      let eInfo = toEpochInfo eraHistory

      kesKeyExpirySlotUtc <- firstExceptT ShelleyQueryCmdSlotToUtcError . hoistEither
                               $ epochInfoSlotToUTCTime
                                 eInfo
                                 (SystemStart protocolParamSystemStart)
                                 (fromIntegral $ kesIntervalEnd * fromIntegral protocolParamSlotsPerKESPeriod)
      -- We get the operational certificate counter from the protocol state and check that
      -- it is equivalent to what we have on disk.

      let ptclStateQinMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryProtocolState
      ptclState <- executeQuery era cModeParams localNodeConnInfo ptclStateQinMode
      (opCertCounter, ptclStateCounter, opCertCounterStateDiag) <- opCertCounterStateCheck ptclState opCert

      let diagnoses = opCertCounterStateDiag ++ [periodCheckDiag]

      if any anyFailureDiagnostic diagnoses
      then
        case mOutFile of
          Just (OutputFile _) -> liftIO $ mapM_ kesOpCertDiagnosticsRender diagnoses
          Nothing -> liftIO $ mapM_ kesOpCertDiagnosticsRender diagnoses
      else do
        let kesPeriodInfo = O.QueryKesPeriodInfoOutput
                              { O.qKesInfoCurrentKESPeriod = currentKesPeriod
                              , O.qKesInfoStartKesInterval = kesIntervalStart
                              , O.qKesInfoStartEndInterval = kesIntervalEnd
                              , O.qKesInfoRemainingSlotsInPeriod = slotsTillNewKesKey
                              , O.qKesInfoNodeStateOperationalCertNo = ptclStateCounter
                              , O.qKesInfoOnDiskOperationalCertNo = opCertCounter
                              , O.qKesInfoMaxKesKeyEvolutions = fromIntegral protocolParamMaxKESEvolutions
                              , O.qKesInfoSlotsPerKesPeriod = fromIntegral protocolParamSlotsPerKESPeriod
                              , O.qKesInfoKesKeyExpiry = kesKeyExpirySlotUtc
                              }
            kesPeriodInfoJSON = encodePretty kesPeriodInfo

        liftIO $ mapM_ kesOpCertDiagnosticsRender diagnoses

        case mOutFile of
          Just (OutputFile oFp) ->
            handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError oFp)
              $ LBS.writeFile oFp kesPeriodInfoJSON
          Nothing -> liftIO $ LBS.putStrLn kesPeriodInfoJSON

    mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode
 where
   -- Returns the number of slots left until KES key rotation and the KES period.
   -- We check the calculated current KES period based on the current slot and
   -- protocolParamSlotsPerKESPeriod matches what is specified in the 'OperationalCertificate'.
   opCertKesPeriodCheck
     :: OperationalCertificate
     -> ChainTip
     -> GenesisParameters
     -> ExceptT ShelleyQueryCmdError IO ( Word64 -- Slots until we need to generate a new KES key
                                        , Word64 -- Current Kes period
                                        , Word64 -- Kes period interval start
                                        , Word64 -- Kes period interval end
                                        , KesOpCertDiagnostic)
   opCertKesPeriodCheck opCert ChainTipAtGenesis
                          GenesisParameters{protocolParamSlotsPerKESPeriod, protocolParamMaxKESEvolutions} = do
     let opCertKesPeriod = fromIntegral $ getKesPeriod opCert
         slotsPerKesPeriod = fromIntegral protocolParamSlotsPerKESPeriod
         maxKesEvolutions  = fromIntegral protocolParamMaxKESEvolutions
         currentKesPeriod = 0 -- We are at genesis
         kesPeriodIntervalStart = 0 -- We are at genesis
         kesPeriodIntervalEnd = maxKesEvolutions
         slotsTillNewKesKey = kesPeriodIntervalEnd * slotsPerKesPeriod

     if fromIntegral protocolParamSlotsPerKESPeriod == opCertKesPeriod
     then
       return ( slotsTillNewKesKey
              , currentKesPeriod
              , kesPeriodIntervalStart
              , kesPeriodIntervalEnd
              , SuccessDiagnostic OpCertCurrentKesPeriodWithinInterval
              )
     else return ( slotsPerKesPeriod
                 , currentKesPeriod
                 , kesPeriodIntervalStart
                 , kesPeriodIntervalEnd
                 , FailureDiagnostic
                     $ OpCertKesPeriodOutsideOfCurrentInterval
                         (nodeOpCertFile, opCertKesPeriod)
                         kesPeriodIntervalStart
                         kesPeriodIntervalEnd
                 )
   opCertKesPeriodCheck opCert (ChainTip currSlot _ _)
                        GenesisParameters{ protocolParamSlotsPerKESPeriod
                                         , protocolParamMaxKESEvolutions} = do
     -- We need to check that the KES period of the operational certificate falls
     -- within the current KES period interval. We can calculate the start of
     -- current KES period interval we are in by dividing the current KES period
     -- by protocolParamMaxKESEvolutions. Once we determine the interval we must check
     -- our KES starting period is within that interval.

     let slotsPerKesPeriod = fromIntegral protocolParamSlotsPerKESPeriod
         maxKesEvolutions  = fromIntegral protocolParamMaxKESEvolutions
         currentKesPeriod = unSlotNo currSlot `div` slotsPerKesPeriod

         kesPeriodIntervalStart = (currentKesPeriod `div` maxKesEvolutions) * maxKesEvolutions
         kesPeriodIntervalEnd = kesPeriodIntervalStart + maxKesEvolutions

         opCertKesPeriod = fromIntegral $ getKesPeriod opCert

         kesPeriodIntervalEndInSlots = kesPeriodIntervalEnd * slotsPerKesPeriod

         slotsTillNewKesKey = kesPeriodIntervalEndInSlots - unSlotNo currSlot

        -- See OCERT rule in ledger specs
     if kesPeriodIntervalStart <= opCertKesPeriod &&
        -- Checks if op cert KES period is less than the interval start
        opCertKesPeriod < kesPeriodIntervalEnd
        -- Checks if op cert KES period is less than the interval end
     then
       return ( slotsTillNewKesKey
              , currentKesPeriod
              , kesPeriodIntervalStart
              , kesPeriodIntervalEnd
              , SuccessDiagnostic OpCertCurrentKesPeriodWithinInterval
              )
     else do
       let fd = FailureDiagnostic $ OpCertKesPeriodOutsideOfCurrentInterval
                                      (nodeOpCertFile, opCertKesPeriod)
                                      kesPeriodIntervalStart
                                      kesPeriodIntervalEnd
       return ( 0
              , currentKesPeriod
              , kesPeriodIntervalStart
              , kesPeriodIntervalEnd
              , fd
              )

   -- We get the operational certificate counter from the protocol state and check that
   -- it is equivalent to what we have on disk.
   opCertCounterStateCheck
     :: ProtocolState era
     -> OperationalCertificate
     -> ExceptT ShelleyQueryCmdError IO ( Word64 -- Operational certificate count
                                        , Word64 -- Ptcl state counter
                                        , [KesOpCertDiagnostic])
   opCertCounterStateCheck ptclState opCert@(OperationalCertificate _ stakePoolVKey) = do
    let onDiskOpCertCount = fromIntegral $ getOpCertCount opCert
    case decodeProtocolState ptclState of
      Left _ -> left ShelleyQueryCmdProtocolStateDecodeFailure
      Right chainDepState -> do
        -- We need the stake pool id to determine what the counter of our SPO
        -- should be.
        let PrtclState opCertCounterMap _ _ = Ledger.csProtocol chainDepState
            StakePoolKeyHash blockIssuerHash = verificationKeyHash stakePoolVKey

        case Map.lookup (coerce blockIssuerHash) opCertCounterMap of
          -- Operational certificate exists in the protocol state
          -- so our ondisk op cert counter must be greater than or
          -- equal to what is in the node state
          Just ptclStateCounter ->
            if onDiskOpCertCount <= ptclStateCounter
            then
              let fd = FailureDiagnostic $ OpCertCounterLessThanPtclCounter
                                             ptclStateCounter
                                             onDiskOpCertCount
                                             nodeOpCertFile
              in return (onDiskOpCertCount, ptclStateCounter, [fd])
            else
              let sd = SuccessDiagnostic OpCertCounterMoreThanOrEqualToNodeState
              in return (onDiskOpCertCount, ptclStateCounter, [sd])
          Nothing ->
            if onDiskOpCertCount >= 0
            then
              let sd = SuccessDiagnostic OpCertCounterMoreThanOrEqualToZero
              in return (onDiskOpCertCount, 0, [sd])
            else
              -- Should't be possible
              let fd = FailureDiagnostic OpCertCounterNegative
              in return (onDiskOpCertCount, 0, [fd])

   kesOpCertDiagnosticsRender :: KesOpCertDiagnostic -> IO ()
   kesOpCertDiagnosticsRender diag =
     case diag of
       SuccessDiagnostic sd ->
          let successString :: String = case sd of
                OpCertCounterMoreThanOrEqualToNodeState ->
                  "The operational certificate counter agrees with the node protocol state counter"
                OpCertCurrentKesPeriodWithinInterval -> "Operational certificate's kes period is within the correct KES period interval"
                OpCertCountersAreEqual -> "The counters in the operational certificate and operational certificate issue counter file are the same"
                OpCertCounterMoreThanOrEqualToZero -> "No blocks minted so far with the operational certificate but the counter is more than or equal to 0"
          in putStrLn $ "✓ " <> successString

       FailureDiagnostic fd ->
          let failureString :: String = case fd of
                OpCertCounterNegative -> "Operational certificate counter is negative"
                OpCertCounterLessThanPtclCounter ptclStateCounter onDiskOpCertCount nodeOpCertFile' ->
                 "The protocol state counter is greater than the counter in the operational certificate at: " <> nodeOpCertFile' <> "\n" <>
                 "On disk operational certificate count: " <> show onDiskOpCertCount <> "\n" <>
                 "Protocol state count: " <> show ptclStateCounter

                OpCertKesPeriodOutsideOfCurrentInterval (nodeOpCertFile', opCertKesPeriod) intervalStart intervalEnd ->
                  "Node operational certificate at: " <> nodeOpCertFile' <> " has an incorrectly specified KES period. " <> "\n" <>
                  "The operational certificate's specified KES period is outside of the current KES period interval." <> "\n" <>
                  "KES period interval start: " <> show intervalStart <> "\n" <>
                  "KES period interval end: " <> show intervalEnd <> "\n" <>
                  "Operational certificate's specified KES period: " <> show opCertKesPeriod

          in putStrLn $ "✗ " <> failureString

data KesOpCertDiagnostic = SuccessDiagnostic SuccessDiagnostic
                         | FailureDiagnostic FailureDiagnostic


anyFailureDiagnostic :: KesOpCertDiagnostic -> Bool
anyFailureDiagnostic FailureDiagnostic{} = True
anyFailureDiagnostic SuccessDiagnostic{} = False

data SuccessDiagnostic
  = OpCertCounterMoreThanOrEqualToNodeState
  | OpCertCounterMoreThanOrEqualToZero
  | OpCertCurrentKesPeriodWithinInterval
  | OpCertCountersAreEqual

data FailureDiagnostic
  = OpCertCounterLessThanPtclCounter
      Word64
      -- ^ Operational certificate counter in the protocol state
      Word64
      -- ^ Operational certificate counter on disk
      FilePath
      -- ^ Operational certificate
  | OpCertKesPeriodOutsideOfCurrentInterval
      !(FilePath, Word64)
      -- ^ Operational certificate filepath with its KES period
      !Word64
      -- ^ Kes Period interval start
      !Word64
      -- ^ Kes Period interval end
  | OpCertCounterNegative

-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--
runQueryPoolParams
  :: AnyConsensusModeParams
  -> NetworkId
  -> Hash StakePoolKey
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryPoolParams (AnyConsensusModeParams cModeParams) network poolid = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryDebugLedgerState
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writePoolParams poolid) result


-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshot
  :: AnyConsensusModeParams
  -> NetworkId
  -> Hash StakePoolKey
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeSnapshot (AnyConsensusModeParams cModeParams) network poolid = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryDebugLedgerState
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writeStakeSnapshot poolid) result


runQueryLedgerState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState (AnyConsensusModeParams cModeParams)
                    network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let qInMode = QueryInEra eInMode
                      . QueryInShelleyBasedEra sbe
                      $ QueryDebugLedgerState
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      obtainLedgerEraClassConstraints sbe (writeLedgerState mOutFile) result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


runQueryProtocolState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolState (AnyConsensusModeParams cModeParams)
                      network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let qInMode = QueryInEra eInMode
                      . QueryInShelleyBasedEra sbe
                      $ QueryProtocolState
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      writeProtocolState mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

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
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr
          query = QueryInEra eInMode
                    . QueryInShelleyBasedEra sbe
                    $ QueryStakeAddresses stakeAddr network

      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeStakeAddressInfo mOutFile $ DelegationsAndRewards result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  | ShelleyProtocolEraMismatch
  -- ^ The Shelley protocol only supports the Shelley era.
  deriving (Eq, Show)

renderLocalStateQueryError :: ShelleyQueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    ByronProtocolNotSupportedError ->
      "The attempted local state query does not support the Byron protocol."
    ShelleyProtocolEraMismatch ->
        "The Shelley protocol mode can only be used with the Shelley era, "
     <> "i.e. with --shelley-mode use --shelley-era flag"

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
    Nothing -> case decodeDebugLedgerState qState of
                 Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
                 Right ledgerState -> liftIO . LBS.putStrLn $ encodePretty ledgerState
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState

writeStakeSnapshot :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Era.Crypto ledgerera ~ StandardCrypto
  => FromCBOR (DebugLedgerState era)
  => PoolId
  -> SerialisedDebugLedgerState era
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeSnapshot (StakePoolKeyHash hk) qState =
  case decodeDebugLedgerState qState of
    -- In the event of decode failure print the CBOR instead
    Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs

    Right ledgerState -> do
      -- Ledger State
      let (DebugLedgerState snapshot) = ledgerState

      -- The three stake snapshots, obtained from the ledger state
      let (SnapShots markS setS goS _) = esSnapshots $ nesEs snapshot

      -- Calculate the three pool and active stake values for the given pool
      liftIO . LBS.putStrLn $ encodePretty $ Stakes
        { markPool = getPoolStake hk markS
        , setPool = getPoolStake hk setS
        , goPool = getPoolStake hk goS
        , markTotal = getAllStake markS
        , setTotal = getAllStake setS
        , goTotal = getAllStake goS
        }

-- | Sum all the stake that is held by the pool
getPoolStake :: KeyHash Cardano.Ledger.Keys.StakePool crypto -> SnapShot crypto -> Integer
getPoolStake hash ss = pStake
  where
    Coin pStake = fold (Map.map fromCompact $ VMap.toMap s)
    Stake s = poolStake hash (_delegations ss) (_stake ss)

-- | Sum the active stake from a snapshot
getAllStake :: SnapShot crypto -> Integer
getAllStake (SnapShot stake _ _) = activeStake
  where
    Coin activeStake = fold (fmap fromCompact (VMap.toMap (unStake stake)))

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState._delegationState._pstate._pParams.<pool_id>
writePoolParams :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => FromCBOR (DebugLedgerState era)
  => Crypto.Crypto (Era.Crypto ledgerera)
  => Era.Crypto ledgerera ~ StandardCrypto
  => PoolId
  -> SerialisedDebugLedgerState era
  -> ExceptT ShelleyQueryCmdError IO ()
writePoolParams (StakePoolKeyHash hk) qState =
  case decodeDebugLedgerState qState of
    -- In the event of decode failure print the CBOR instead
    Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs

    Right ledgerState -> do
      let DebugLedgerState snapshot = ledgerState
      let poolState = _pstate $ _delegationState $ esLState $ nesEs snapshot

      -- Pool parameters
      let poolParams = Map.lookup hk $ _pParams poolState
      let fPoolParams = Map.lookup hk $ _fPParams poolState
      let retiring = Map.lookup hk $ _retiring poolState

      liftIO . LBS.putStrLn $ encodePretty $ Params poolParams fPoolParams retiring

writeProtocolState :: Maybe OutputFile
                   -> ProtocolState era
                   -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile ps@(ProtocolState pstate) =
  case mOutFile of
    Nothing -> case decodeProtocolState ps of
                 Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
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
        case shelleyBasedEra' of
          ShelleyBasedEraShelley -> writeUTxo fpath utxo
          ShelleyBasedEraAllegra -> writeUTxo fpath utxo
          ShelleyBasedEraMary -> writeUTxo fpath utxo
          ShelleyBasedEraAlonzo -> writeUTxo fpath utxo
 where
   writeUTxo fpath utxo' =
     handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
       $ LBS.writeFile fpath (encodePretty utxo')

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
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]

    ShelleyBasedEraAllegra ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraMary ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraAlonzo ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum) = txInOutTuple
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
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  result <- ExceptT . fmap (join . first ShelleyQueryCmdAcquireFailure) $
    executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT @ShelleyQueryCmdError $ do
      anyE@(AnyCardanoEra era) <- case consensusModeOnly cModeParams of
        ByronMode -> return $ AnyCardanoEra ByronEra
        ShelleyMode -> return $ AnyCardanoEra ShelleyEra
        CardanoMode -> lift . queryExpr $ QueryCurrentEra CardanoModeIsMultiEra

      let cMode = consensusModeOnly cModeParams

      case toEraInMode era cMode of
        Just eInMode -> do
          sbe <- getSbe $ cardanoEraStyle era

          firstExceptT ShelleyQueryCmdEraMismatch . ExceptT $
            queryExpr . QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

        Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

  writeStakePools mOutFile result

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
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query = QueryInEra eInMode
                    . QueryInShelleyBasedEra sbe
                    $ QueryStakeDistribution
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeStakeDistribution mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


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
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLeadershipSchedule (AnyConsensusModeParams cModeParams) network
                           (GenesisFile genFile) coldVerKeyFile (SigningKeyFile vrfSkeyFp)
                           whichSchedule = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  sbe <- getSbe $ cardanoEraStyle era
  let cMode = consensusModeOnly cModeParams

  poolid <- firstExceptT ShelleyQueryCmdTextReadError
              . newExceptT $ readVerificationKeyOrHashOrFile AsStakePoolKey coldVerKeyFile

  vrkSkey <- firstExceptT ShelleyQueryCmdTextEnvelopeReadError . newExceptT
               $ readFileTextEnvelope (AsSigningKey AsVrfKey) vrfSkeyFp
  shelleyGenesis <- firstExceptT ShelleyQueryCmdGenesisReadError $
                          newExceptT $ readAndDecodeShelleyGenesis genFile
  case cMode of
    CardanoMode -> do
      eInMode <- toEraInMode era cMode
          & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

      let pparamsQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
          ptclStateQuery = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryProtocolState
          eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra

      pparams <- executeQuery era cModeParams localNodeConnInfo pparamsQuery
      ptclState <- executeQuery era cModeParams localNodeConnInfo ptclStateQuery
      eraHistory <- firstExceptT ShelleyQueryCmdAcquireFailure . newExceptT $ queryNodeLocalState localNodeConnInfo Nothing eraHistoryQuery
      let eInfo = toEpochInfo eraHistory

      schedule :: Set SlotNo
        <- case whichSchedule of
             CurrentEpoch -> do
               let currentEpochStateQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryCurrentEpochState
                   currentEpochQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryEpoch
               serCurrentEpochState <- executeQuery era cModeParams localNodeConnInfo currentEpochStateQuery
               curentEpoch <- executeQuery era cModeParams localNodeConnInfo currentEpochQuery

               firstExceptT ShelleyQueryCmdLeaderShipError $ hoistEither
                 $ eligibleLeaderSlotsConstaints sbe
                 $ currentEpochEligibleLeadershipSlots
                     sbe
                     shelleyGenesis
                     eInfo
                     pparams
                     ptclState
                     poolid
                     vrkSkey
                     serCurrentEpochState
                     curentEpoch

             NextEpoch -> do
               let currentEpochStateQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryCurrentEpochState
                   currentEpochQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryEpoch
               tip <- liftIO $ getLocalChainTip localNodeConnInfo

               curentEpoch <- executeQuery era cModeParams localNodeConnInfo currentEpochQuery
               serCurrentEpochState <- executeQuery era cModeParams localNodeConnInfo currentEpochStateQuery

               firstExceptT ShelleyQueryCmdLeaderShipError $ hoistEither
                 $ eligibleLeaderSlotsConstaints sbe
                 $ nextEpochEligibleLeadershipSlots sbe shelleyGenesis
                     serCurrentEpochState ptclState poolid vrkSkey pparams
                     eInfo (tip, curentEpoch)

      liftIO $ printLeadershipSchedule schedule eInfo (SystemStart $ sgSystemStart shelleyGenesis)
    mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode
 where
  printLeadershipSchedule
    :: Set SlotNo
    -> EpochInfo (Either Text)
    -> SystemStart
    -> IO ()
  printLeadershipSchedule leadershipSlots eInfo sStart = do
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



-- Helpers

calcEraInMode
  :: CardanoEra era
  -> ConsensusMode mode
  -> ExceptT ShelleyQueryCmdError IO (EraInMode era mode)
calcEraInMode era mode=
  hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyCardanoEra era))
                   $ toEraInMode era mode

determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> ExceptT ShelleyQueryCmdError IO AnyCardanoEra
determineEra cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> do
      eraQ <- liftIO . queryNodeLocalState localNodeConnInfo Nothing
                     $ QueryCurrentEra CardanoModeIsMultiEra
      case eraQ of
        Left acqFail -> left $ ShelleyQueryCmdAcquireFailure acqFail
        Right anyCarEra -> return anyCarEra

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
   execQuery :: IO (Either AcquireFailure (Either EraMismatch result))
   execQuery = queryNodeLocalState localNodeConnInfo Nothing q

getSbe :: Monad m => CardanoEraStyle era -> ExceptT ShelleyQueryCmdError m (Api.ShelleyBasedEra era)
getSbe LegacyByronEra = left ShelleyQueryCmdByronEra
getSbe (Api.ShelleyBasedEra sbe) = return sbe

queryResult
  :: Either AcquireFailure (Either EraMismatch a)
  -> ExceptT ShelleyQueryCmdError IO a
queryResult eAcq =
  case eAcq of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right eResult ->
      case eResult of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right result -> return result

toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Either Text)
toEpochInfo (EraHistory _ interpreter) =
  hoistEpochInfo (first (Text.pack . show ) . runExcept)
    $ Consensus.interpreterToEpochInfo interpreter

obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => Api.ShelleyBasedEra era
  -> (( UsesValue ledgerera
      , ToJSON (DebugLedgerState era)
      , FromCBOR (DebugLedgerState era)
      , Era.Crypto ledgerera ~ StandardCrypto
      ) => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAlonzo  f = f


eligibleLeaderSlotsConstaints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (( ShelleyLedgerEra era ~ ledgerera
      , Ledger.Crypto ledgerera ~ StandardCrypto
      , FromCBOR (DebugLedgerState era)
      , Era.Era ledgerera
      , HasField "_d" (Core.PParams (ShelleyLedgerEra era)) UnitInterval
      , Crypto.Signable (Crypto.VRF (Ledger.Crypto ledgerera)) Seed
      , Share (Core.TxOut (ShelleyLedgerEra era)) ~ Interns (Ledger.Credential 'Staking StandardCrypto)
      ) => a
     )
  -> a
eligibleLeaderSlotsConstaints ShelleyBasedEraShelley f = f
eligibleLeaderSlotsConstaints ShelleyBasedEraAllegra f = f
eligibleLeaderSlotsConstaints ShelleyBasedEraMary    f = f
eligibleLeaderSlotsConstaints ShelleyBasedEraAlonzo  f = f
