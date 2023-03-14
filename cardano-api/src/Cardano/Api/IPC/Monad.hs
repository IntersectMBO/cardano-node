{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryExpr
  , LocalStateQueryFailure
  , determineEraExpr
  , determineEraInMode
  , determineShelleyBasedEra
  , executeLocalStateQueryExpr
  , queryExpr
  , queryExprE
  ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.State.Strict
import qualified Data.Text as Text

import           Cardano.Ledger.Shelley.Scripts ()
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Mux as Mux
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Version
import           Cardano.Api.Modes
import           Cardano.Api.Utils


{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

-- | Monadic type for constructing local state query expressions.
--
-- Use 'queryExpr' in a do block to construct queries of this type and convert
-- the expression to a 'Net.Query.LocalStateQueryClient' with 'setupLocalStateQueryExpr'.
--
-- Some consideration was made to use Applicative instead of Monad as the abstraction in
-- order to support pipelining, but we actually have a fair amount of code where the next
-- query depends on the result of the former and therefore actually need Monad.
--
-- In order to make pipelining still possible we can explore the use of Selective Functors
-- which would allow us to straddle both worlds.
newtype LocalStateQueryExpr block point query r m a = LocalStateQueryExpr
  { runLocalStateQueryExpr :: StateT QueryState (ContT (Net.Query.ClientStAcquired block point query m r) m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

-- | 'QueryState' tracks the following:
--   - If a query has resulted in an 'EraMismatch'
--   - The node to client version of the node
--   - the node to client versions of both the node and the query in question
-- When constructing a 'LocalStateQueryExpr' we can now query the 'State'
-- before we populate the TMVar with the query result. This helps make the 'LocalStateQueryExpr'
-- monad be more composable as all the errors can now be handled in one place, 'setupLocalStateQueryExpr'.

data QueryState
  = NodeVersion NodeToClientVersion
  | NodeAndQueryVersions
      NodeToClientVersion -- ^ Node version
      NodeToClientVersion -- ^ Query version
  | QSMismatch EraMismatch
  deriving Show


-- | Execute a local state query expression.
executeLocalStateQueryExpr
  :: LocalNodeConnectInfo mode
  -> Maybe ChainPoint
  -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> IO (Either LocalStateQueryFailure a)
executeLocalStateQueryExpr connectInfo mpoint f = do
  tmvResultLocalState <- newEmptyTMVarIO
  let waitResult = readTMVar tmvResultLocalState

  r <- try $ connectToLocalNodeWithVersion
         connectInfo
         (\nodeVer ->
           LocalNodeClientProtocols
           { localChainSyncClient    = NoLocalChainSyncClient
           , localStateQueryClient   = Just $ setupLocalStateQueryExpr waitResult mpoint tmvResultLocalState nodeVer f
           , localTxSubmissionClient = Nothing
           , localTxMonitoringClient = Nothing
           }
         )
  case r of
    Left ex -> return . Left $ LocalStateQueryMuxError ex
    Right () -> atomically waitResult

data LocalStateQueryFailure
  = LocalStateQueryAcqFail !AcquiringFailure
  | LocalStateQueryNtcErrror !UnsupportedNtcVersionError
  | LocalStateQueryMismatch !EraMismatch
  | LocalStateQueryMuxError !Mux.MuxError -- The MuxError is an exception that we catch
  deriving Show

instance Error LocalStateQueryFailure where
  displayError (LocalStateQueryAcqFail acqFail) = show acqFail
  displayError (LocalStateQueryNtcErrror (UnsupportedNtcVersionError queryVersion ntcVersion)) =
    "Unsupported feature for the node-to-client protocol version.\n" <>
    "This query requires at least " <> show queryVersion <> " but the node negotiated " <> show ntcVersion <> ".\n" <>
    "Later node qState support later protocol qState (but development protocol qState are not enabled in the node by default)."
  displayError (LocalStateQueryMismatch eraMismatch) =
    "A query from a certain era was applied to a ledger from a different era: " <> show eraMismatch
  displayError (LocalStateQueryMuxError (Mux.MuxError Mux.MuxBearerClosed _e)) =
    "Connection closed unexpectedly. Check that you specified the correct mode."
  displayError (LocalStateQueryMuxError muxError) = show muxError

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
setupLocalStateQueryExpr ::
     STM x
     -- ^ An STM expression that only returns when all protocols are complete.
     -- Protocols must wait until 'waitDone' returns because premature exit will
     -- cause other incomplete protocols to abort which may lead to deadlock.
  -> Maybe ChainPoint
  -> TMVar (Either LocalStateQueryFailure a)
  -> NodeToClientVersion
  -> LocalStateQueryExpr (BlockInMode mode) ChainPoint (QueryInMode mode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) IO ()
setupLocalStateQueryExpr waitDone mPointVar' resultVar' nodeVer qInMode = do
  LocalStateQueryClient . pure . Net.Query.SendMsgAcquire mPointVar' $
    Net.Query.ClientStAcquiring
    { Net.Query.recvMsgAcquired = do
      let s = execStateT (runLocalStateQueryExpr qInMode) (NodeVersion nodeVer)
      runContT s $ \case
          QSMismatch mismatch -> do
            atomically $ putTMVar resultVar' $ Left (LocalStateQueryMismatch mismatch)
            pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

          NodeAndQueryVersions nodeVer' qVer -> do
            -- Perform version check here
            if nodeVer >= qVer
            then runContT (runStateT (runLocalStateQueryExpr qInMode) (NodeAndQueryVersions nodeVer' qVer))
                   $ \(result, _) -> do
                      atomically $ putTMVar resultVar' (Right result)
                      void $ atomically waitDone -- Wait for all protocols to complete before exiting.
                      pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()
            else do
              atomically $ putTMVar resultVar' $ Left (LocalStateQueryNtcErrror $ UnsupportedNtcVersionError nodeVer' qVer)
              pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

          NodeVersion _ -> error "setupLocalStateQueryExpr: NodeVersion only should not be possible"
    , Net.Query.recvMsgFailure = \failure -> do
        atomically $ putTMVar resultVar' (Left . LocalStateQueryAcqFail $ toAcquiringFailure failure)
        void $ atomically waitDone -- Wait for all protocols to complete before exiting.
        pure $ Net.Query.SendMsgDone ()
    }

getQueryState :: LocalStateQueryExpr block point (QueryInMode mode) r IO QueryState
getQueryState = LocalStateQueryExpr get

-- | Use 'queryExpr' in a do block to construct monadic local state queries.
queryExpr :: QueryInMode mode a -> LocalStateQueryExpr block point (QueryInMode mode) r IO a
queryExpr q = do
  let queryVersion = nodeToClientVersionOf q
  currentVersions <- getQueryState
  case currentVersions of
    NodeVersion v ->
      let qState = NodeAndQueryVersions v queryVersion
      in LocalStateQueryExpr $ do
           put qState
           StateT $ \_ ->
              ContT $ \f ->
                let t = flip $ curry f
                in do pure $ Net.Query.SendMsgQuery q
                           $ Net.Query.ClientStQuerying
                               { Net.Query.recvMsgResult = t qState }

    QSMismatch{} ->
      error $ "queryExpr: QSMismatch Shouldn't be possible as we handle this state in setupLocalStateQueryExpr " <>
              "and we close the connection."

    NodeAndQueryVersions consensusV _previousQueryVersion ->
      let qState = NodeAndQueryVersions consensusV queryVersion
      in LocalStateQueryExpr $ do
           put qState
           StateT $ \_ ->
              ContT $ \f ->
                let t = flip $ curry f
                in do pure $ Net.Query.SendMsgQuery q
                           $ Net.Query.ClientStQuerying
                               { Net.Query.recvMsgResult = t qState }

-- | Handles queries that return `Either EraMismatch a`
queryExprE :: QueryInMode mode (Either EraMismatch a) -> LocalStateQueryExpr block point (QueryInMode mode) r IO a
queryExprE q = do
  let queryVersion = nodeToClientVersionOf q
  currentVersions <- getQueryState
  case currentVersions of
    NodeVersion v -> do
      let qState = NodeAndQueryVersions v queryVersion
      r <- LocalStateQueryExpr $ do
             put qState
             StateT $ \_ ->
               ContT $ \f ->
                 let t = flip $ curry f
                 in do pure $ Net.Query.SendMsgQuery q
                              $ Net.Query.ClientStQuerying
                                  { Net.Query.recvMsgResult = t qState }
      case r of
        Left err ->
          LocalStateQueryExpr $ do
            put $ QSMismatch err
            StateT $ \_ -> error $ "queryExprE: This should not get called as the era " <>
                                   "mismatch is handled in setupLocalStateQueryExpr before " <>
                                   "the continuation is run."

        Right res -> return res
    NodeAndQueryVersions consensusVersion _previousQueryVersion -> do
      let qState = NodeAndQueryVersions consensusVersion queryVersion
      r <- LocalStateQueryExpr $ do
             put qState
             StateT $ \_ ->
               ContT $ \f ->
                 let t = flip $ curry f
                 in do pure $ Net.Query.SendMsgQuery q
                              $ Net.Query.ClientStQuerying
                                  { Net.Query.recvMsgResult = t qState }
      case r of
        Left err ->
          LocalStateQueryExpr $ do
            put $ QSMismatch err
            StateT $ \_ -> error $ "queryExprE: This should not get called as the era " <>
                                   "mismatch is handled in setupLocalStateQueryExpr before " <>
                                   "the continuation is run."

        Right res -> return res
    QSMismatch{} ->
      error $ "queryExpr: QSMismatch Shouldn't be possible as we handle this state in setupLocalStateQueryExpr " <>
              "and we close the connection."

-- | A monad expression that determines what era the node is in.
determineEraExpr
  :: ConsensusModeParams mode
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO AnyCardanoEra
determineEraExpr cModeParams =
  case consensusModeOnly cModeParams of
    ByronMode   -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> queryExpr (QueryCurrentEra CardanoModeIsMultiEra)

determineEraInMode
  :: CardanoEra era
  -> ConsensusModeParams mode
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (EraInMode era mode)
determineEraInMode era cModeParams = do
  case toEraInMode era $ consensusModeOnly cModeParams of
    Nothing ->
      fail $ "determineEraInMode: " <> Text.unpack (renderEra $ anyCardanoEra era) <>
             " is not supported in " <> show (consensusModeOnly cModeParams)
    Just eInMode -> return eInMode

determineShelleyBasedEra
  :: CardanoEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (ShelleyBasedEra era)
determineShelleyBasedEra era =
  case cardanoEraStyle era of
    LegacyByronEra -> fail "determineShelleyBasedEra: The current era is Byron"
    ShelleyBasedEra sbe -> return sbe

