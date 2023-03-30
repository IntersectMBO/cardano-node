{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLI.Shelley.Run.Query
  ( DelegationsAndRewards(..)
  , ShelleyQueryCmdError
  , renderOpCertIntervalInformation
  , renderShelleyQueryCmdError
  , runQueryCmd
  , toEpochInfo
  , utcTimeToSlotNo
  , mergeDelegsAndRewards
  , percentage
  ) where

import           Cardano.Api
import qualified Cardano.Api as Api
import           Cardano.Api.Byron
import           Cardano.Api.Orphans ()
import           Cardano.Api.Shelley

import           Control.Monad (forM, forM_)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.IO.Unlift (MonadIO (..))
import           Control.Monad.Oops (CouldBe, Variant, runOopsInExceptT)
import qualified Control.Monad.Oops as OO
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT (..), except, runExcept, runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   hoistMaybe, left, newExceptT, onLeft, onNothing)
import           Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.List (nub)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as T
import qualified Data.Text.IO as Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Time.Clock
import qualified Data.Vector as Vector
import           Data.Word
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
import qualified Cardano.CLI.Shelley.Output as O
import           Cardano.CLI.Shelley.Run.Genesis (ShelleyGenesisCmdError,
                   readAndDecodeShelleyGenesis)
import           Cardano.CLI.Types
import           Cardano.Crypto.Hash (hashToBytesAsHex)
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.VRF as Crypto
import           Cardano.Ledger.BaseTypes (Seed)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Crypto
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.SafeHash (HashAnnotated)
import           Cardano.Ledger.Shelley.LedgerState
                   (PState (psFutureStakePoolParams, psRetiring, psStakePoolParams))
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import           Cardano.Slotting.EpochInfo (EpochInfo (..), epochInfoSlotToUTCTime, hoistEpochInfo)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   toRelativeTime)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import           Ouroboros.Consensus.Protocol.TPraos (StandardCrypto)

import qualified Ouroboros.Consensus.HardFork.History as Consensus
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus

{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Redundant flip" -}

data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdSimpleQueryError !SimpleQueryError
  | ShelleyQueryCmdAnyQueryError !AllQueryErrors
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
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
  -- | ShelleyQueryCmdUnsupportedNtcVersion !UnsupportedNtcVersionError
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdAnyQueryError allQErrors -> Text.pack $ show allQErrors
    ShelleyQueryCmdSimpleQueryError simpleQErr -> Text.pack $ show simpleQErr
    ShelleyQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
      " Era: " <> textShow era
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

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryLeadershipSchedule mNodeSocketPath consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule outputAs ->
      runQueryLeadershipSchedule mNodeSocketPath consensusModeParams network shelleyGenFp poolid vrkSkeyFp whichSchedule outputAs
    QueryProtocolParameters' mNodeSocketPath consensusModeParams network mOutFile ->
      runQueryProtocolParameters mNodeSocketPath consensusModeParams network mOutFile
    QueryTip mNodeSocketPath consensusModeParams network mOutFile ->
      runQueryTip mNodeSocketPath consensusModeParams network mOutFile
    QueryStakePools' mNodeSocketPath consensusModeParams network mOutFile ->
      runQueryStakePools mNodeSocketPath consensusModeParams network mOutFile
    QueryStakeDistribution' mNodeSocketPath consensusModeParams network mOutFile ->
      runQueryStakeDistribution mNodeSocketPath consensusModeParams network mOutFile
    QueryStakeAddressInfo mNodeSocketPath consensusModeParams addr network mOutFile ->
      runQueryStakeAddressInfo mNodeSocketPath consensusModeParams addr network mOutFile
    QueryDebugLedgerState' mNodeSocketPath consensusModeParams network mOutFile ->
      runQueryLedgerState mNodeSocketPath consensusModeParams network mOutFile
    QueryStakeSnapshot' mNodeSocketPath consensusModeParams network allOrOnlyPoolIds mOutFile ->
      runQueryStakeSnapshot mNodeSocketPath consensusModeParams network allOrOnlyPoolIds mOutFile
    QueryProtocolState' mNodeSocketPath consensusModeParams network mOutFile ->
      runQueryProtocolState mNodeSocketPath consensusModeParams network mOutFile
    QueryUTxO' mNodeSocketPath consensusModeParams qFilter networkId mOutFile ->
      runQueryUTxO mNodeSocketPath consensusModeParams qFilter networkId mOutFile
    QueryKesPeriodInfo mNodeSocketPath consensusModeParams network nodeOpCert mOutFile ->
      runQueryKesPeriodInfo mNodeSocketPath consensusModeParams network nodeOpCert mOutFile
    QueryPoolState' mNodeSocketPath consensusModeParams network poolid ->
      runQueryPoolState mNodeSocketPath consensusModeParams network poolid
    QueryTxMempool mNodeSocketPath consensusModeParams network op mOutFile ->
      runQueryTxMempool mNodeSocketPath consensusModeParams network op mOutFile

useOops :: Bool
useOops = True

runQueryProtocolParameters
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  if useOops
    then do
      pparams <- runOopsInExceptT @ShelleyQueryCmdError $ do
        executeLocalStateQueryExprAnyQuery_ localNodeConnInfo Nothing
          (do
              AnyCardanoEra era <- determineEraExprAnyQuery_ cModeParams

              case cardanoEraStyle era of
                LegacyByronEra -> OO.throw $ ShelleyQueryCmdAnyQueryError AllQueryEraExpectedSbe
                ShelleyBasedEra sbe -> do
                  eInMode <- determineEraInModeAnyQuery_ era cModeParams
                    & OO.catch @InvalidEraInMode (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . invalidEraInModeToSbqeEraInMode)

                  Api.queryExprAnyQueryE_ (AnyQuerySbe eInMode sbe QueryProtocolParameters)
                    & OO.onLeft (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeQueryEraMismatch)
          ) & OO.catch (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeSimpleQueryError . SimpleQueryErrorAcquiringFail)
            & OO.catch (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeSimpleQueryError . SimpleQueryErrorUnsupportedVer)

      writeProtocolParameters mOutFile pparams
    else do
      pparams
        <- firstExceptT ShelleyQueryCmdAnyQueryError . newExceptT
            $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
                AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
                case cardanoEraStyle era of
                  LegacyByronEra -> left AllQueryEraExpectedSbe
                  ShelleyBasedEra sbe -> do
                    eInMode <- determineEraInModeAnyQuery era cModeParams
                    let q = AnyQuerySbe eInMode sbe QueryProtocolParameters
                    queryExprAnyQueryE q

      writeProtocolParameters mOutFile pparams
 where
  writeProtocolParameters
    :: Maybe (File () Out)
    -> ProtocolParameters
    -> ExceptT ShelleyQueryCmdError IO ()
  writeProtocolParameters mOutFile' pparams =
    case mOutFile' of
      Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
      Just (File fpath) ->
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
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network mOutFile = do
  case consensusModeOnly cModeParams of
    CardanoMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

      mLocalState <- if useOops
        then do
          eLocalState <- liftIO $ runExceptT $ runOopsInExceptT @ShelleyQueryCmdError $ do
            executeLocalStateQueryExpr_ localNodeConnInfo Nothing
              (do
                  era <- queryExpr_ (QueryCurrentEra CardanoModeIsMultiEra)
                  eraHistory <- queryExpr_ (QueryEraHistory CardanoModeIsMultiEra)
                  mChainBlockNo <- queryExpr_ QueryChainBlockNo & OO.catchAsNothing @UnsupportedNtcVersionError
                  mChainPoint <- queryExpr_ (QueryChainPoint CardanoMode) & OO.catchAsNothing @UnsupportedNtcVersionError
                  mSystemStart <- queryExpr_ QuerySystemStart & OO.catchAsNothing @UnsupportedNtcVersionError

                  return O.QueryTipLocalState
                    { O.era = era
                    , O.eraHistory = eraHistory
                    , O.mSystemStart = mSystemStart
                    , O.mChainTip = makeChainTip <$> mChainBlockNo <*> mChainPoint
                    }
              ) & OO.catch (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeSimpleQueryError . SimpleQueryErrorAcquiringFail)
                & OO.catch (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeSimpleQueryError . SimpleQueryErrorUnsupportedVer)

          hushM eLocalState $ \e ->
            liftIO . T.hPutStrLn IO.stderr $ "Warning: Local state unavailable: " <> renderShelleyQueryCmdError e
        else do
          eLocalState <-
            liftIO $ executeLocalStateQueryExprSimple localNodeConnInfo Nothing $ do
              era <- queryExprSimple $ QueryCurrentEra CardanoModeIsMultiEra
              eraHistory <- queryExprSimple $ QueryEraHistory CardanoModeIsMultiEra
              mChainBlockNo <- queryExprSimple QueryChainBlockNo
              mChainPoint <- queryExprSimple $ QueryChainPoint CardanoMode
              mSystemStart <- queryExprSimple QuerySystemStart

              return O.QueryTipLocalState
                { O.era = era
                , O.eraHistory = eraHistory
                , O.mSystemStart = Just mSystemStart
                , O.mChainTip = Just $ makeChainTip mChainBlockNo mChainPoint
                }

          hushM (first ShelleyQueryCmdSimpleQueryError eLocalState) $ \e ->
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

            mSyncProgress <- hushM syncProgressResult $ \e ->
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
        Just (File fpath) -> liftIO . LBS.writeFile fpath $ encodePretty localStateOutput
        Nothing -> liftIO . LBS.putStrLn $ encodePretty localStateOutput

    mode -> left (ShelleyQueryCmdUnsupportedMode (AnyConsensusMode mode))

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--
runQueryUTxO
  :: SocketPath
  -> AnyConsensusModeParams
  -> QueryUTxOFilter
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO (SocketPath sockPath) (AnyConsensusModeParams cModeParams)
             qfilter network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyUTxO <- firstExceptT ShelleyQueryCmdAnyQueryError
              $ newExceptT $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
                  AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
                  eInMode <- determineEraInModeAnyQuery era cModeParams
                  case cardanoEraStyle era of
                    LegacyByronEra -> left AllQueryEraExpectedSbe
                    ShelleyBasedEra sbe -> do
                      let q = AnyQuerySbe eInMode sbe $ QueryUTxO qfilter
                      AnyUTxO (shelleyBasedToCardanoEra sbe) <$> queryExprAnyQueryE q

  writeFilteredUTxOs mOutFile anyUTxO

runQueryKesPeriodInfo
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> File () In
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryKesPeriodInfo (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network nodeOpCertFile mOutFile = do
  opCert <- lift (readFileTextEnvelope AsOperationalCertificate nodeOpCertFile)
    & onLeft (left . ShelleyQueryCmdOpCertCounterReadError)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
      cMode = consensusModeOnly cModeParams
  case cMode of
    CardanoMode -> do
      (chainTip, ptclState, eraHistory, gParams)
        <- firstExceptT ShelleyQueryCmdAnyQueryError
             $ newExceptT $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
                 AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
                 let eInMode = toEraInCardanoMode era
                 -- We check that the KES period specified in the operational certificate is correct
                 -- based on the KES period defined in the genesis parameters and the current slot number
                 case cardanoEraStyle era of
                   LegacyByronEra -> left AllQueryEraExpectedSbe
                   ShelleyBasedEra sbe -> do
                     let genesisQinMode = AnyQuerySbe eInMode sbe QueryGenesisParameters
                         eraHistoryQuery = AnyQueryAnyEra $ QueryEraHistory CardanoModeIsMultiEra
                     gParams <- queryExprAnyQueryE genesisQinMode
                     eraHistory <- queryExprAnyQuery eraHistoryQuery
                     chainTip <- liftIO $ getLocalChainTip localNodeConnInfo

                     -- We get the operational certificate counter from the protocol state and check that
                     -- it is equivalent to what we have on disk.

                     let ptclStateQinMode = AnyQuerySbe eInMode sbe QueryProtocolState
                     ptclState <- queryExprAnyQueryE ptclStateQinMode
                     let anyPtclState = AnyProtocolState sbe $ eligibleWriteProtocolStateConstaints sbe
                                                              $ decodeProtocolState ptclState
                     return (chainTip, anyPtclState, eraHistory, gParams)



      let curKesPeriod = currentKesPeriod chainTip gParams
          oCertStartKesPeriod = opCertStartingKesPeriod opCert
          oCertEndKesPeriod = opCertEndKesPeriod gParams opCert
          opCertIntervalInformation = opCertIntervalInfo gParams chainTip curKesPeriod oCertStartKesPeriod oCertEndKesPeriod

      let eInfo = toTentativeEpochInfo eraHistory
      (onDiskC, stateC) <- opCertOnDiskAndStateCounters ptclState opCert
      let counterInformation = opCertNodeAndOnDiskCounters onDiskC stateC

      -- Always render diagnostic information
      liftIO . putStrLn $ renderOpCertIntervalInformation (unFile nodeOpCertFile) opCertIntervalInformation
      liftIO . putStrLn $ renderOpCertNodeAndOnDiskCounterInformation (unFile nodeOpCertFile) counterInformation

      let qKesInfoOutput = createQueryKesPeriodInfoOutput opCertIntervalInformation counterInformation eInfo gParams
          kesPeriodInfoJSON = encodePretty qKesInfoOutput

      liftIO $ LBS.putStrLn kesPeriodInfoJSON
      forM_ mOutFile (\(File oFp) ->
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


consensusGetOpCertCounters
  :: forall era.
     Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  => ShelleyBasedEra era
  -> Consensus.ChainDepState (ConsensusProtocol era)
  -> Map (KeyHash 'BlockIssuer (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era))) Word64
consensusGetOpCertCounters _  = Consensus.getOpCertCounters (Proxy @(ConsensusProtocol era))

lookUpOpCertCounterMap
  :: Crypto.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  => ShelleyBasedEra era
  -> KeyHash 'StakePool StandardCrypto
  -> Map (KeyHash 'BlockIssuer (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era))) Word64
  -> Maybe Word64
lookUpOpCertCounterMap sbe k = Map.lookup (coerceKeyRole sbe k)

coerceKeyRole
  :: Crypto.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  => ShelleyBasedEra era
  -> KeyHash 'StakePool StandardCrypto
  -> KeyHash 'BlockIssuer (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era))
coerceKeyRole _ = coerce

opCertOnDiskAndStateCounters
   :: AnyProtocolState
   -> OperationalCertificate
   -> ExceptT ShelleyQueryCmdError IO (OpCertOnDiskCounter, Maybe OpCertNodeStateCounter)
opCertOnDiskAndStateCounters (AnyProtocolState sbe eChainDepState) opCert@(OperationalCertificate _ stakePoolVKey) = do
    let onDiskOpCertCount = fromIntegral $ getOpCertCount opCert

    chainDepState <- firstExceptT ShelleyQueryCmdProtocolStateDecodeFailure $ hoistEither eChainDepState
    -- We need the stake pool id to determine what the counter of our SPO
    -- should be.
    let opCertCounterMap = getPraosSuppsNodeConstraint sbe $ consensusGetOpCertCounters sbe chainDepState
        StakePoolKeyHash blockIssuerHash = verificationKeyHash stakePoolVKey
    case getADDRHashIsBlakeConstraint sbe $ lookUpOpCertCounterMap sbe blockIssuerHash opCertCounterMap of
      -- Operational certificate exists in the protocol state
      -- so our ondisk op cert counter must be greater than or
      -- equal to what is in the node state
      Just ptclStateCounter -> return (OpCertOnDiskCounter onDiskOpCertCount, Just $ OpCertNodeStateCounter ptclStateCounter)
      Nothing -> return (OpCertOnDiskCounter onDiskOpCertCount, Nothing)

getPraosSuppsNodeConstraint
  :: ShelleyBasedEra era
  -> (Consensus.PraosProtocolSupportsNode (ConsensusProtocol era) => a)
  -> a
getPraosSuppsNodeConstraint ShelleyBasedEraShelley f = f
getPraosSuppsNodeConstraint ShelleyBasedEraAllegra f = f
getPraosSuppsNodeConstraint ShelleyBasedEraMary    f = f
getPraosSuppsNodeConstraint ShelleyBasedEraAlonzo  f = f
getPraosSuppsNodeConstraint ShelleyBasedEraBabbage f = f
getPraosSuppsNodeConstraint ShelleyBasedEraConway  f = f

getADDRHashIsBlakeConstraint
  :: ShelleyBasedEra era
  -> (Crypto.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224 => a)
  -> a
getADDRHashIsBlakeConstraint ShelleyBasedEraShelley f = f
getADDRHashIsBlakeConstraint ShelleyBasedEraAllegra f = f
getADDRHashIsBlakeConstraint ShelleyBasedEraMary    f = f
getADDRHashIsBlakeConstraint ShelleyBasedEraAlonzo  f = f
getADDRHashIsBlakeConstraint ShelleyBasedEraBabbage f = f
getADDRHashIsBlakeConstraint ShelleyBasedEraConway  f = f

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
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> [Hash StakePoolKey]
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryPoolState (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network poolIds = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  poolState
    <- firstExceptT ShelleyQueryCmdAnyQueryError
         $ newExceptT $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing  $ do
             AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
             eInMode <- determineEraInModeAnyQuery era cModeParams
             case cardanoEraStyle era of
               LegacyByronEra -> left AllQueryEraExpectedSbe
               ShelleyBasedEra sbe -> do
                 let qInMode = AnyQuerySbe eInMode sbe
                                 . QueryPoolState . Just $ Set.fromList poolIds
                 ps <- queryExprAnyQueryE qInMode
                 return $ obtainLedgerEraClassConstraints sbe $ createPoolState ps

  pState <- firstExceptT ShelleyQueryCmdPoolStateDecodeError $ hoistEither poolState

  writePoolState pState

-- | Query the local mempool state
runQueryTxMempool
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> TxMempoolQuery
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTxMempool (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network query mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  localQuery <- case query of
      TxMempoolQueryTxExists tx -> do
        case consensusModeOnly cModeParams of
          CardanoMode -> do
            anyE@(AnyCardanoEra era)
              <- firstExceptT ShelleyQueryCmdSimpleQueryError $ newExceptT
                   . executeLocalStateQueryExprSimple localNodeConnInfo Nothing
                   . queryExprSimple $ QueryCurrentEra CardanoModeIsMultiEra
            let cMode = consensusModeOnly cModeParams
            eInMode <- toEraInMode era cMode
                & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)
            pure $ LocalTxMonitoringQueryTx $ TxIdInMode tx eInMode
          mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode

      TxMempoolQueryNextTx -> pure LocalTxMonitoringSendNextTx
      TxMempoolQueryInfo -> pure LocalTxMonitoringMempoolInformation

  result <- liftIO $ queryTxMonitoringLocal localNodeConnInfo localQuery
  let renderedResult = encodePretty result
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn renderedResult
    Just (File oFp) -> handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError oFp)
        $ LBS.writeFile oFp renderedResult


-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshot
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> AllOrOnly [Hash StakePoolKey]
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeSnapshot (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network allOrOnlyPoolIds mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath


  eStakeSnapshot  <- firstExceptT ShelleyQueryCmdAnyQueryError $ newExceptT
                      $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
                           AnyCardanoEra era  <- determineEraExprAnyQuery cModeParams
                           eInMode <- determineEraInModeAnyQuery era cModeParams
                           case cardanoEraStyle era of
                            LegacyByronEra -> left AllQueryEraExpectedSbe
                            ShelleyBasedEra sbe -> do
                              let q = AnyQuerySbe eInMode sbe . QueryStakeSnapshot
                                        $ case allOrOnlyPoolIds of
                                            All -> Nothing
                                            Only poolIds -> Just $ Set.fromList poolIds
                              sSnapshot <- queryExprAnyQueryE q
                              return $ decodeSomeStakeSnapshot sbe sSnapshot

  pState <- firstExceptT ShelleyQueryCmdStakeSnapshotDecodeError . hoistEither $ eStakeSnapshot

  writeStakeSnapshots mOutFile pState


runQueryLedgerState
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
  someLedgerState <- firstExceptT ShelleyQueryCmdAnyQueryError $ newExceptT
                    $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
                        AnyCardanoEra era  <- determineEraExprAnyQuery cModeParams
                        eInMode <- determineEraInModeAnyQuery era cModeParams
                        case cardanoEraStyle era of
                         LegacyByronEra -> left AllQueryEraExpectedSbe
                         ShelleyBasedEra sbe -> do
                           let q = AnyQuerySbe eInMode sbe QueryDebugLedgerState
                           res <- queryExprAnyQueryE q
                           return $ decodeSomeSerialisedDebugLedgerState sbe res

  writeLedgerState mOutFile someLedgerState


runQueryProtocolState
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolState (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
  someProtocolState <- firstExceptT ShelleyQueryCmdAnyQueryError $ newExceptT
                       $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
                           AnyCardanoEra era  <- determineEraExprAnyQuery cModeParams
                           eInMode <- determineEraInModeAnyQuery era cModeParams
                           case cardanoEraStyle era of
                            LegacyByronEra -> left AllQueryEraExpectedSbe
                            ShelleyBasedEra sbe -> do
                              let q = AnyQuerySbe eInMode sbe QueryProtocolState
                              res <- queryExprAnyQueryE q
                              return $ eligibleWriteProtocolStateConstaints sbe
                                     $ AnyProtocolState sbe
                                     $ decodeProtocolState res

  writeProtocolState mOutFile someProtocolState

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.
runQueryStakeAddressInfo
  :: SocketPath
  -> AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo (SocketPath sockPath) (AnyConsensusModeParams cModeParams) (StakeAddress _ addr) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
  delegAndRewards <- firstExceptT ShelleyQueryCmdAnyQueryError $ newExceptT
                       $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
                           AnyCardanoEra era  <- determineEraExprAnyQuery cModeParams
                           eInMode <- determineEraInModeAnyQuery era cModeParams
                           case cardanoEraStyle era of
                            LegacyByronEra -> left AllQueryEraExpectedSbe
                            ShelleyBasedEra sbe -> do
                              let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr
                                  query = AnyQuerySbe eInMode sbe $ QueryStakeAddresses stakeAddr network
                              queryExprAnyQueryE query

  writeStakeAddressInfo mOutFile $ DelegationsAndRewards delegAndRewards

-- -------------------------------------------------------------------------------------------------

writeStakeAddressInfo
  :: Maybe (File () Out)
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile delegsAndRewards =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty delegsAndRewards)
    Just (File fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty delegsAndRewards)

writeLedgerState :: Maybe (File () Out)
                 -> SomeSerialisedDebugLedgerState
                 -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile (SomeSerialisedDebugLedgerState sbe serLedgerState) =
  case mOutFile of
    Nothing ->
      case serLedgerState of
        Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
        Right ledgerState ->
          liftIO . LBS.putStrLn $ obtainLedgerEraClassConstraints sbe $ Aeson.encode ledgerState
    Just (File fpath) ->
      let bs = case serLedgerState of
                 Left bs' -> bs'
                 Right ledgerState -> obtainLedgerEraClassConstraints sbe $ Aeson.encode ledgerState
      in handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $ LBS.writeFile fpath bs


writeStakeSnapshots
  :: Maybe (File () Out)
  -> SomeStakeSnapshot
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeSnapshots mOutFile (SomeStakeSnapshot sbe (StakeSnapshot snapshot)) = do
  -- Calculate the three pool and active stake values for the given pool
  liftIO . maybe LBS.putStrLn (LBS.writeFile . unFile) mOutFile
         $ obtainLedgerEraClassConstraints sbe $ encodePretty snapshot



-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState.lsDPState.dpsPState.psStakePoolParams.<pool_id>
createPoolState :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Core.EraCrypto ledgerera ~ StandardCrypto
  => Core.Era ledgerera
  => SerialisedPoolState era
  -> Either DecoderError (Map (KeyHash 'StakePool StandardCrypto) (Params StandardCrypto))
createPoolState serialisedCurrentEpochState = do
  PoolState poolState <- decodePoolState serialisedCurrentEpochState

  let hks = Set.toList $ Set.fromList $ Map.keys (psStakePoolParams poolState)
            <> Map.keys (psFutureStakePoolParams poolState) <> Map.keys (psRetiring poolState)

  Right $ Map.fromList $ hks <&>
        ( \hk ->
          ( hk
          , Params
            { poolParameters        = Map.lookup hk (SL.psStakePoolParams  poolState)
            , futurePoolParameters  = Map.lookup hk (SL.psFutureStakePoolParams poolState)
            , retiringEpoch         = Map.lookup hk (SL.psRetiring poolState)
            }
          )
        )


writePoolState
  :: Map (KeyHash 'StakePool StandardCrypto) (Params StandardCrypto)
  -> ExceptT ShelleyQueryCmdError IO ()
writePoolState poolStates = liftIO . LBS.putStrLn $ encodePretty poolStates

writeProtocolState
  :: Maybe (File () Out)
  -> AnyProtocolState
  -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile (AnyProtocolState sbe ePstate) =
  case mOutFile of
    Nothing -> case ePstate  of
      Left (bs, _) -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
      Right chainDepstate ->
        liftIO . LBS.putStrLn
          $ eligibleWriteProtocolStateConstaints sbe
          $ encodePretty chainDepstate
    Just (File fpath) -> case ePstate of
      Left (bs, _) ->
        handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
          $ LBS.writeFile fpath bs
      Right chainDepstate ->
        handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
          $ LBS.writeFile fpath
          $ eligibleWriteProtocolStateConstaints sbe
          $ encodePretty chainDepstate


writeFilteredUTxOs :: Maybe (File () Out)
                   -> AnyUTxO
                   -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile (AnyUTxO era utxo)  =
  case cardanoEraStyle era of
    LegacyByronEra -> return () -- TODO: Not possible
    ShelleyBasedEra sbe -> do
      case mOutFile of
        Nothing -> liftIO $ printFilteredUTxOs sbe utxo
        Just (File fpath) ->
          case sbe of
            ShelleyBasedEraShelley -> writeUTxo fpath utxo
            ShelleyBasedEraAllegra -> writeUTxo fpath utxo
            ShelleyBasedEraMary -> writeUTxo fpath utxo
            ShelleyBasedEraAlonzo -> writeUTxo fpath utxo
            ShelleyBasedEraBabbage -> writeUTxo fpath utxo
            ShelleyBasedEraConway -> writeUTxo fpath utxo
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
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakePools (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  if useOops
    then runOopsInExceptT @ShelleyQueryCmdError
        -- $ OO.catch @InvalidEraInMode (OO.throw . ShelleyQueryCmdAnyQueryError)
        $ OO.catch @AllQueryErrors (OO.throw . ShelleyQueryCmdAnyQueryError)
        $ OO.catch @InvalidEraInMode (OO.throw . AllQueryErrorSbe . invalidEraInModeToSbqeEraInMode)
        $ do
      result <- executeLocalStateQueryExprAnyQuery_ localNodeConnInfo Nothing
        ( do
            AnyCardanoEra era <- determineEraExprAnyQuery_ cModeParams
            eInMode <- determineEraInModeAnyQuery_ era cModeParams
            case cardanoEraStyle era of
              LegacyByronEra -> OO.throw AllQueryEraExpectedSbe
              ShelleyBasedEra sbe -> do
                (queryExpr_ $ AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode $ QueryInShelleyBasedEra sbe QueryStakePools)
                  & OO.onLeft (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeQueryEraMismatch)
        )  & OO.catch (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeSimpleQueryError . SimpleQueryErrorAcquiringFail)
           & OO.catch (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeSimpleQueryError . SimpleQueryErrorUnsupportedVer)

      writeStakePools_ mOutFile result
    else do
      execState <-
        liftIO $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
          AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
          eInMode <- determineEraInModeAnyQuery era cModeParams
          case cardanoEraStyle era of
            LegacyByronEra -> left AllQueryEraExpectedSbe
            ShelleyBasedEra sbe -> do
              let q = AnyQuerySbe eInMode sbe QueryStakePools
              queryExprAnyQueryE q
      poolIds <- firstExceptT ShelleyQueryCmdAnyQueryError $ hoistEither execState
      OO.runOopsInExceptT @ShelleyQueryCmdError $ writeStakePools_ mOutFile poolIds

writeStakePools_ :: ()
  => e `CouldBe` ShelleyQueryCmdError
  => Maybe (File () Out)
  -> Set PoolId
  -> ExceptT (Variant e) IO ()
writeStakePools_ (Just outFile) stakePools =
  liftIO (LBS.writeFile (unFile outFile) (encodePretty stakePools))
    & OO.onException (OO.throw . ShelleyQueryCmdWriteFileError . FileIOError (unFile outFile))

writeStakePools_ Nothing stakePools =
  forM_ (Set.toList stakePools) $ \poolId ->
    liftIO . putStrLn $ Text.unpack (serialiseToBech32 poolId)

runQueryStakeDistribution
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network mOutFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
  result <- firstExceptT ShelleyQueryCmdAnyQueryError . newExceptT
              $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
                  (AnyCardanoEra era) <- determineEraExprAnyQuery cModeParams
                  eInMode <- determineEraInModeAnyQuery era cModeParams
                  case cardanoEraStyle era of
                    LegacyByronEra -> left AllQueryEraExpectedSbe
                    ShelleyBasedEra sbe ->
                      let query = AnyQuerySbe eInMode sbe QueryStakeDistribution
                      in queryExprAnyQueryE query

  writeStakeDistribution mOutFile result

writeStakeDistribution
  :: Maybe (File () Out)
  -> Map PoolId Rational
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (File outFile)) stakeDistrib =
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
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> GenesisFile -- ^ Shelley genesis
  -> VerificationKeyOrHashOrFile StakePoolKey
  -> SigningKeyFile In -- ^ VRF signing key
  -> EpochLeadershipSchedule
  -> Maybe (File () Out)
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLeadershipSchedule
    (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network
    (GenesisFile genFile) coldVerKeyFile vrfSkeyFp
    whichSchedule mJsonOutputFile = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
      cMode = consensusModeOnly cModeParams

  poolid <- lift (readVerificationKeyOrHashOrFile AsStakePoolKey coldVerKeyFile)
    & onLeft (left . ShelleyQueryCmdTextReadError)

  vrkSkey <- lift (readFileTextEnvelope (AsSigningKey AsVrfKey) vrfSkeyFp)
    & onLeft (left . ShelleyQueryCmdTextEnvelopeReadError)

  shelleyGenesis <- lift (readAndDecodeShelleyGenesis genFile)
    & onLeft (left . ShelleyQueryCmdGenesisReadError)

  case cMode of
    CardanoMode -> do

-- data AllQueryErrors
--   = AllQueryErrorSimple SimpleQueryError
--   | AllQueryErrorSbe ShelleyBasedQueryError
--   | AllQueryEraExpectedSbe
--   deriving Show

      (eSchedule, eInfo) <- do
        if useOops
          then OO.runOopsInExceptT
              $ OO.catch @AllQueryErrors (OO.throw . ShelleyQueryCmdAnyQueryError)
              $ OO.catch @ShelleyBasedQueryError (OO.throw . AllQueryErrorSbe)
              $ OO.catch @SimpleQueryError (OO.throw . SbqeSimpleQueryError)
              $ OO.catch @AcquiringFailure (OO.throw . SimpleQueryErrorAcquiringFail)
              $ OO.catch @UnsupportedNtcVersionError (OO.throw . SimpleQueryErrorUnsupportedVer)
              $ OO.catch @InvalidEraInMode (OO.throw . invalidEraInModeToSbqeEraInMode)
              $ do
            executeLocalStateQueryExprAnyQuery_ localNodeConnInfo Nothing $ do
              AnyCardanoEra era <- determineEraExprAnyQuery_ cModeParams
              eInMode <- determineEraInModeAnyQuery_ era cModeParams
              case cardanoEraStyle era of
                LegacyByronEra -> OO.throw AllQueryEraExpectedSbe
                ShelleyBasedEra sbe' -> do
                  let pparamsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode $ QueryInShelleyBasedEra sbe' QueryProtocolParameters
                      ptclStateQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode $ QueryInShelleyBasedEra sbe' QueryProtocolState
                      eraHistoryQuery = AnyQueryAnyEra $ QueryEraHistory CardanoModeIsMultiEra
                  pparams <- queryExprAnyQueryE_ pparamsQuery
                    & OO.onLeft @EraMismatch (OO.throw . SbqeQueryEraMismatch)
                  ptclState <- queryExprAnyQueryE_ ptclStateQuery
                    & OO.onLeft @EraMismatch (OO.throw . SbqeQueryEraMismatch)
                  eraHistory <- queryExprAnyQueryE_ eraHistoryQuery
                    & OO.catch (OO.throw . ShelleyQueryCmdAnyQueryError . AllQueryErrorSbe . SbqeSimpleQueryError . SimpleQueryErrorAcquiringFail)

                  -- Current Schedule
                  let poolDistribQuery = AnyQuerySbe eInMode sbe' $ QueryPoolDistribution (Just (Set.singleton poolid))
                  serializedPoolDistribution <- queryExprAnyQueryE_ poolDistribQuery
                    & OO.onLeft @EraMismatch (OO.throw . SbqeQueryEraMismatch)

                  -- Next schedule
                  let currentEpochStateQuery = AnyQuerySbe eInMode sbe' QueryCurrentEpochState
                  serCurrentEpochState <- queryExprAnyQueryE_ currentEpochStateQuery
                    & OO.onLeft @EraMismatch (OO.throw . SbqeQueryEraMismatch)

                  let currentEpochQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode $ QueryInShelleyBasedEra sbe' QueryEpoch
                  currentEpoch <- queryExprAnyQueryE_ currentEpochQuery
                    & OO.onLeft @EraMismatch (OO.throw . SbqeQueryEraMismatch)

                  tip <- liftIO $ getLocalChainTip localNodeConnInfo

                  let eInfo = toEpochInfo eraHistory
                      bpp = bundleProtocolParams era pparams

                  let schedule =
                        case whichSchedule of
                          CurrentEpoch ->
                            eligibleLeaderSlotsConstaints sbe'
                              $ currentEpochEligibleLeadershipSlots
                                  sbe' shelleyGenesis eInfo bpp ptclState
                                  poolid vrkSkey serializedPoolDistribution currentEpoch
                          NextEpoch ->
                            eligibleLeaderSlotsConstaints sbe'
                              $ nextEpochEligibleLeadershipSlots sbe' shelleyGenesis
                                serCurrentEpochState ptclState poolid vrkSkey bpp
                                eInfo (tip, currentEpoch)
                  return (schedule, eInfo)
          else firstExceptT ShelleyQueryCmdAnyQueryError $ newExceptT $ do
            executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
              AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
              eInMode <- determineEraInModeAnyQuery era cModeParams
              case cardanoEraStyle era of
                LegacyByronEra -> left AllQueryEraExpectedSbe
                ShelleyBasedEra sbe' -> do
                  let pparamsQuery = AnyQuerySbe eInMode sbe' QueryProtocolParameters
                      ptclStateQuery = AnyQuerySbe eInMode sbe' QueryProtocolState
                      eraHistoryQuery = AnyQueryAnyEra $ QueryEraHistory CardanoModeIsMultiEra
                      currentEpochQuery = AnyQuerySbe eInMode sbe' QueryEpoch
                  pparams <- queryExprAnyQueryE pparamsQuery
                  ptclState <- queryExprAnyQueryE ptclStateQuery
                  eraHistory <- queryExprAnyQuery eraHistoryQuery
                  currentEpoch <- queryExprAnyQueryE currentEpochQuery

                  -- Current Schedule
                  let poolDistribQuery = AnyQuerySbe eInMode sbe' $ QueryPoolDistribution (Just (Set.singleton poolid))
                  serializedPoolDistribution <- queryExprAnyQueryE poolDistribQuery

                  -- Next schedule
                  let currentEpochStateQuery = AnyQuerySbe eInMode sbe' QueryCurrentEpochState
                  serCurrentEpochState <- queryExprAnyQueryE currentEpochStateQuery

                  tip <- liftIO $ getLocalChainTip localNodeConnInfo

                  let eInfo = toEpochInfo eraHistory
                      bpp = bundleProtocolParams era pparams

                  let schedule =
                        case whichSchedule of
                          CurrentEpoch ->
                            eligibleLeaderSlotsConstaints sbe'
                              $ currentEpochEligibleLeadershipSlots
                                  sbe' shelleyGenesis eInfo bpp ptclState
                                  poolid vrkSkey serializedPoolDistribution currentEpoch
                          NextEpoch ->
                            eligibleLeaderSlotsConstaints sbe'
                              $ nextEpochEligibleLeadershipSlots sbe' shelleyGenesis
                                serCurrentEpochState ptclState poolid vrkSkey bpp
                                eInfo (tip, currentEpoch)
                  return (schedule, eInfo)

      schedule <- firstExceptT ShelleyQueryCmdLeaderShipError $ hoistEither eSchedule

      case mJsonOutputFile of
        Nothing -> liftIO $ printLeadershipScheduleAsText schedule eInfo (SystemStart $ sgSystemStart shelleyGenesis)
        Just (File jsonOutputFile) ->
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

-- calcEraInMode
--   :: CardanoEra era
--   -> ConsensusMode mode
--   -> ExceptT ShelleyQueryCmdError IO (EraInMode era mode)
-- calcEraInMode era mode =
--   pure (toEraInMode era mode)
--     & onNothing (left (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyCardanoEra era)))

-- getSbe :: Monad m => CardanoEraStyle era -> ExceptT ShelleyQueryCmdError m (Api.ShelleyBasedEra era)
-- getSbe LegacyByronEra = left ShelleyQueryCmdByronEra
-- getSbe (Api.ShelleyBasedEra sbe) = return sbe

-- getSbeInQuery_ :: ()
--   => Monad m
--   => e `CouldBe` ShelleyQueryCmdError
--   => CardanoEraStyle era
--   -> ExceptT (Variant e) m (ShelleyBasedEra era)
-- getSbeInQuery_ LegacyByronEra = OO.throw ShelleyQueryCmdByronEra
-- getSbeInQuery_ (ShelleyBasedEra sbe) = return sbe

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


-- | Get slot number for timestamp, or an error if the UTC timestamp is before 'SystemStart' or after N+1 era
utcTimeToSlotNo
  :: SocketPath
  -> AnyConsensusModeParams
  -> NetworkId
  -> UTCTime
  -> ExceptT ShelleyQueryCmdError IO SlotNo
utcTimeToSlotNo (SocketPath sockPath) (AnyConsensusModeParams cModeParams) network utcTime = do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
  case consensusModeOnly cModeParams of
    CardanoMode -> do
      (systemStart, eraHistory)
        <- firstExceptT ShelleyQueryCmdSimpleQueryError $ newExceptT
             $ executeLocalStateQueryExprSimple localNodeConnInfo Nothing $
                 (,) <$> queryExprSimple QuerySystemStart
                     <*> queryExprSimple (QueryEraHistory CardanoModeIsMultiEra)
      let relTime = toRelativeTime systemStart utcTime
      hoistEither $ Api.getSlotForRelativeTime relTime eraHistory & first ShelleyQueryCmdPastHorizon
    mode -> left . ShelleyQueryCmdUnsupportedMode $ AnyConsensusMode mode



obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => Api.ShelleyBasedEra era
  -> (( ToJSON (DebugLedgerState era)
      , FromCBOR (DebugLedgerState era)
      , Core.EraCrypto ledgerera ~ StandardCrypto
      , Core.Era (ShelleyLedgerEra era)
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
      , Core.EraCrypto ledgerera ~ StandardCrypto
      , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
      , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
      , Core.Era ledgerera
      , Crypto.Signable (Crypto.VRF (Core.EraCrypto ledgerera)) Seed
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
