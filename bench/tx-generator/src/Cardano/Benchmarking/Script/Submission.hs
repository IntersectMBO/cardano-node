{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Cardano.Benchmarking.Script.Submission
Description : Backend-agnostic transaction submission transport.

A 'SubmitTransport' is the minimal interface for submitting transactions to
some endpoint, one at a time. Concrete backends (e.g.
"Cardano.Benchmarking.Script.Ogmios") construct one; 'submitLoop' drives a
transaction stream through it, and 'runSubmitTransport' wires that into the
script monad — all independent of how submission actually happens on the wire.

The backend's rejection type is kept abstract (the @e@ parameter): the loop
only ever needs to render a rejection for tracing, expressed as a 'Pretty'
constraint, so no transport-specific detail leaks into this module.
-}
module Cardano.Benchmarking.Script.Submission
  ( SubmitTransport (..)
  , OnRejection (..)
  , onRejectionFor
  , submitLoop
  , runSubmitTransport
  ) where

import           Cardano.Api (Tx)

import           Cardano.Benchmarking.LogTypes (BenchTracers (..), TraceBenchTxSubmit (..))
import           Cardano.Benchmarking.Script.Env (ActionM, getBenchTracers, liftTxGenError,
                   traceDebug)
import           Cardano.Benchmarking.Script.Types (Generator (..))
import           Cardano.Benchmarking.Wallet (TxStream)
import           Cardano.Logging (traceWith)
import           Cardano.TxGenerator.Types (TxGenError (..))

import           Prelude

import           Control.Monad (when)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text (renderStrict)

import           Streaming

-- | A backend that can submit a single transaction and report, synchronously,
-- whether the endpoint accepted it or rejected it. The rejection type @e@ is
-- the backend's own; this module never inspects it, only renders it.
newtype SubmitTransport era e = SubmitTransport
  { submitOne :: Tx era -> IO (Either e ()) }

-- | How to proceed when the endpoint rejects a transaction.
data OnRejection
  = AbortOnRejection    -- ^ stop the stream at the first rejection
  | ContinueOnRejection -- ^ submit the whole stream, then fail if anything was rejected
  deriving (Eq, Show)

-- | Setup-phase generators (genesis import, splitting) emit chains of
-- interdependent transactions: once one is rejected, everything after it is
-- doomed, so abort right away. Only a plain 'NtoM' stream (the benchmarking
-- phase) is known to consist of mutually independent transactions, so it is
-- submitted to the end and the final tally decides.
--
-- 'Sequence' may mix chained and independent sub-generators, so it aborts
-- unconditionally. Either policy fails the run on any rejection (see
-- 'runSubmitTransport'); the policy only decides whether to keep submitting
-- after the first one, and aborting is always safe — merely conservative
-- for an all-independent sequence.
onRejectionFor :: Generator -> OnRejection
onRejectionFor generator = case generator of
  NtoM {}  -> ContinueOnRejection
  Take _ g -> onRejectionFor g
  Cycle g  -> onRejectionFor g
  _        -> AbortOnRejection

-- | Drive a transaction stream through a transport, returning a @(sent, failed)@
-- tally. Rejections are traced (rendered via 'Pretty') and counted; how the loop
-- reacts depends on the 'OnRejection' policy.
submitLoop
  :: forall era e. Pretty e
  => BenchTracers
  -> OnRejection
  -> SubmitTransport era e
  -> TxStream IO era
  -> IO (Either TxGenError (Int, Int))
submitLoop tracers onRejection transport = go 0 0
 where
  go :: Int -> Int -> TxStream IO era -> IO (Either TxGenError (Int, Int))
  go sent failed stream = do
    step <- Streaming.inspect stream
    case step of
      Left () -> return $ Right (sent, failed)
      Right (Left err :> _rest) -> return $ Left err
      Right (Right tx :> rest) -> do
        outcome <- submitOne transport tx
        case outcome of
          Right () -> go (sent + 1) failed rest
          Left e -> do
            let rendered = renderRejection e
            traceWith (btTxSubmit_ tracers) $ TraceBenchTxSubError rendered
            case onRejection of
              AbortOnRejection ->
                return $ Left $ TxGenError $ "transaction rejected: " ++ Text.unpack rendered
              ContinueOnRejection -> go sent (failed + 1) rest

renderRejection :: Pretty e => e -> Text
renderRejection = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- | Run a transaction stream through a transport within the script monad: open
-- the transport, drive the loop, and turn the outcome into the run's result —
-- a rejected transaction is a functional failure, so the action fails (and the
-- process exits non-zero) if anything was rejected.
runSubmitTransport
  :: Pretty e
  => OnRejection
  -> ((SubmitTransport era e -> IO (Either TxGenError (Int, Int)))
        -> IO (Either TxGenError (Int, Int)))
  -> TxStream IO era
  -> ActionM ()
runSubmitTransport onRejection withTransport txStream = do
  tracers <- getBenchTracers
  result <- liftIO $ withTransport $ \transport ->
              submitLoop tracers onRejection transport txStream
  case result of
    Left err -> liftTxGenError err
    Right (sent, failed) -> do
      traceDebug $ "submission done, " ++ show sent ++ " sent, " ++ show failed ++ " failed"
      when (failed > 0) $ liftTxGenError $ TxGenError $
        show failed ++ " of " ++ show (sent + failed)
          ++ " transactions were rejected"
