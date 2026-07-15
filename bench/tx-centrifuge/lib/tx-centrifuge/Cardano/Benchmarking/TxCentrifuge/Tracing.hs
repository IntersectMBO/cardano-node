{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

-- | Tracer setup for the tx-centrifuge. Creates configured contra-tracers
-- backed by trace-dispatcher and reads optional @TraceOptions@ from the
-- generator config file.
module Cardano.Benchmarking.TxCentrifuge.Tracing
  ( -- * Configure.
    Tracers (..)
  , setupTracers, nullTracers
    -- * Traces.
  , BuilderTrace (..)
  , PipeTrace (..)
  , RecyclerTrace (..)
  , ObserverTrace (..)
  , TxSubmission (..)
    -- * Re-exports.
  , traceWith
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Exception (SomeException, try)
import Numeric.Natural (Natural)
-----------
-- aeson --
-----------
import Data.Aeson (Value (String), (.=), object)
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
----------------
-- containers --
----------------
import Data.Map.Strict qualified as Map
-------------------
-- contra-tracer --
-------------------
import "contra-tracer" Control.Tracer (Tracer (..), traceWith)
---------------------------------
-- ouroboros-consensus:cardano --
---------------------------------
import Ouroboros.Consensus.Cardano qualified as Consensus (CardanoBlock)
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
---------------------------------------------
-- ouroboros-consensus:ouroboros-consensus --
---------------------------------------------
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Mempool
---------------------------------
-- ouroboros-network:framework --
---------------------------------
import Ouroboros.Network.Driver.Simple qualified as Simple
-----------------------------------------
-- ouroboros-network:framework-tracing --
-----------------------------------------
-- For the MetaTrace and LogFormatting instances of:
-- - Simple.TraceSendRecv
-- - Stateful.TraceSendRecv
import Ouroboros.Network.Tracing ()
---------------------------------
-- ouroboros-network:protocols --
---------------------------------
import Ouroboros.Network.Protocol.KeepAlive.Type qualified as KA
import Ouroboros.Network.Protocol.TxSubmission2.Type qualified as STX
----------
-- text --
----------
import Data.Text qualified as Text
----------------------
-- trace-dispatcher --
----------------------
import Cardano.Logging qualified as Logging
-------------------
-- tx-centrifuge --
-------------------
import Cardano.Benchmarking.TxCentrifuge.Fund qualified as Fund
-- Imported for its orphan LogFormatting / MetaTrace instances.
import Cardano.Benchmarking.TxCentrifuge.Tracing.Orphans ()

--------------------------------------------------------------------------------
-- Tracers
--------------------------------------------------------------------------------

type CardanoBlock = Consensus.CardanoBlock Eras.StandardCrypto

data Tracers = Tracers
  { -- tx-centrifuge traces.
    ------------------------
    -- | Builder trace: new-transaction construction events.
    trBuilder :: !(Tracer IO BuilderTrace)
    -- | Pipe trace: payload/input queue add and remove events, with the
    -- relevant queue depth. Purely about queue mechanics.
  , trPipe :: !(Tracer IO PipeTrace)
    -- | Recycler trace: payloads entering the pending backlog ('Pending') and
    -- recycled inputs added back onto a pipe's input queue ('AddToPipe'). Both
    -- events always emit the pending backlog size ('pending'), add a "count" at
    -- DDetailed and above, and the full inputs at DMaximum.
  , trRecycler :: !(Tracer IO RecyclerTrace)
    -- | Observer trace: on-chain transaction confirmation/rollback events.
  , trObserver :: !(Tracer IO ObserverTrace)
    -- | Clean, structured TxSubmission2 trace emitted by TxSubmission.hs.
  , trTxSubmission :: !(Tracer IO TxSubmission)
    -- ouroboros-network traces.
    ----------------------------
    -- | Low-level protocol trace from ouroboros-network's Driver.runPeer.
  , trTxSubmission2
      :: !( Tracer
              IO
              ( Simple.TraceSendRecv
                  ( STX.TxSubmission2
                      (Mempool.GenTxId CardanoBlock)
                      (Mempool.GenTx CardanoBlock)
                  )
              )
          )
    -- | Low-level protocol trace from ouroboros-network.
  , trKeepAlive
      :: !( Tracer
              IO
              (Simple.TraceSendRecv KA.KeepAlive)
          )

  }

-- | All-silent tracers.
nullTracers :: Tracers
nullTracers = Tracers
  { trBuilder       = Tracer (\_ -> pure ())
  , trPipe          = Tracer (\_ -> pure ())
  , trRecycler      = Tracer (\_ -> pure ())
  , trObserver      = Tracer (\_ -> pure ())
  , trTxSubmission  = Tracer (\_ -> pure ())
  , trTxSubmission2 = Tracer (\_ -> pure ())
  , trKeepAlive     = Tracer (\_ -> pure ())
  }

--------------------------------------------------------------------------------
-- Tracer setup
--------------------------------------------------------------------------------

-- | Create configured tracers from the tx-centrifuge config file. If the file
-- contains a @TraceOptions@ section, those settings are used. Otherwise falls
-- back to a sensible default (stdout, machine format, severity Debug).
setupTracers :: FilePath -> IO Tracers
setupTracers configFile = do
  trConfig <-
    either
      (\(_ :: SomeException) -> defaultTraceConfig)
      id
      <$> try (Logging.readConfiguration configFile)
  configReflection <- Logging.emptyConfigReflection
  stdoutTrace      <- Logging.standardTracer
  let trForward = mempty
      mbTrEkg   = Nothing
  -- tx-centrifuge traces.
  ------------------------
  -- Builder (TxCentrifuge.Builder.NewTx).
  !builderTr <-    Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "Builder"]
  Logging.configureTracers configReflection trConfig [builderTr]
  -- Pipe (TxCentrifuge.Pipe.*).
  !pipeTr <-       Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "Pipe"]
  Logging.configureTracers configReflection trConfig [pipeTr]
  -- Recycler (TxCentrifuge.Recycler.*).
  !recyclerTr <-   Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "Recycler"]
  Logging.configureTracers configReflection trConfig [recyclerTr]
  -- Observer (TxCentrifuge.Observer.Announce).
  !observerTr <-   Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "Observer"]
  Logging.configureTracers configReflection trConfig [observerTr]
  -- TxSubmission (TxCentrifuge.TxSubmission.*).
  !txSubTraceTr <- Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxCentrifuge", "TxSubmission"]
  Logging.configureTracers configReflection trConfig [txSubTraceTr]
  -- ouroboros-network traces.
  ----------------------------
  -- TxSubmission2 (low-level protocol trace).
  !txSub2Trace <-  Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["TxSubmission2"]
  Logging.configureTracers configReflection trConfig [txSub2Trace]
  -- KeepAlive.
  !keepAliveTr <-  Logging.mkCardanoTracer stdoutTrace trForward mbTrEkg
                     ["KeepAlive"]
  Logging.configureTracers configReflection trConfig [keepAliveTr]
  pure Tracers
    { trBuilder       = Tracer $ Logging.traceWith builderTr
    , trPipe          = Tracer $ Logging.traceWith pipeTr
    , trRecycler      = Tracer $ Logging.traceWith recyclerTr
    , trObserver      = Tracer $ Logging.traceWith observerTr
    , trTxSubmission  = Tracer $ Logging.traceWith txSubTraceTr
    , trTxSubmission2 = Tracer $ Logging.traceWith txSub2Trace
    , trKeepAlive     = Tracer $ Logging.traceWith keepAliveTr
    }

-- | Default config: stdout machine format, severity Debug for all namespaces.
defaultTraceConfig :: Logging.TraceConfig
defaultTraceConfig = Logging.emptyTraceConfig
  { Logging.tcOptions = Map.fromList
      [ ( []
        , [ Logging.ConfSeverity
              (Logging.SeverityF (Just Logging.Debug))
          , Logging.ConfBackend
              [Logging.Stdout Logging.MachineFormat]
          ]
        )
      ]
  }

--------------------------------------------------------------------------------
-- Builder trace messages
--------------------------------------------------------------------------------

-- | Trace messages emitted by the payload builder.
--
-- == Builder pipeline
--
-- The builder consumes input UTxOs (unspent funds) from the input queue, builds
-- and signs a transaction, and results are enqueued for workers to submit. Each
-- transaction produces new output UTxOs. After submission, these outputs can be
-- recycled at different points back to the input queue, forming a closed loop:
--
-- @
-- inputs --> [builder: build & sign tx] --> (tx, outputs) --> [do something]
--    ^                                                               |
--    +---------------------maybe recycle outputs --------------------+
-- @
--
-- == Cardano identifiers
--
-- The Cardano ledger uses a UTxO (Unspent Transaction Output) model. Every
-- transaction consumes existing UTxOs as /inputs/ and produces new UTxOs as
-- /outputs/. Three types from @cardano-api@ identify these objects:
--
-- === 'Api.TxId' — transaction identifier
--
-- A Blake2b-256 hash of the serialised transaction body ('Api.TxBody').
-- Uniquely identifies a transaction on the blockchain. Rendered as a
-- 64-character hex string via 'Api.serialiseToRawBytesHexText'.
--
-- === 'Api.TxIx' — output index
--
-- A zero-based index selecting one output within a transaction.
--
-- === 'Api.TxIn' — UTxO reference
--
-- A @('Api.TxId', 'Api.TxIx')@ pair that uniquely identifies a single UTxO on
-- the ledger. The standard display format is @\"\<hex-txid\>#\<ix\>\"@,
-- produced by 'Api.renderTxIn'.
--
-- A transaction's /input/ 'Api.TxIn's reference existing UTxOs being spent. Its
-- /output/ 'Api.TxIn's are derived from the new 'Api.TxId' paired with
-- sequential indices (0, 1, 2, ...).
--
-- In the tx-centrifuge, each 'Fund' record wraps a 'Api.TxIn' (the UTxO
-- reference), its Lovelace value, and the signing key needed to spend it.
data BuilderTrace
  = -- | A new transaction was built. This is purely about transaction
    -- construction and nothing about the pipe or the queues (see 'PipeTrace').
    --
    -- * 'String': builder name (the workload name, see 'Runtime.builderName').
    -- * 'Api.TxId': Blake2b-256 hash identifying the new transaction.
    --   Obtain via @'Api.getTxId' ('Api.getTxBody' signedTx)@.
    -- * 'Api.AddressInEra': the destination address this transaction pays to
    --   (the builder's own address, from 'destination_signing_key'). Rendered
    --   as bech32 only at 'Logging.DMaximum'.
    -- * @['Fund.Fund']@ (inputs): funds consumed by this transaction. Each
    --   fund's 'Fund.fundTxIn' is a 'Api.TxIn' pointing to an existing UTxO
    --   on the ledger.
    -- * @['Fund.Fund']@ (outputs): funds produced by this transaction. Each
    --   fund's 'Fund.fundTxIn' is derived from the new 'Api.TxId' and a
    --   sequential 'Api.TxIx' index (0, 1, 2, ...).
    BuilderNewTx
      !String !Api.TxId !(Api.AddressInEra Api.ConwayEra) [Fund.Fund] [Fund.Fund]

-- | Namespace: @TxCentrifuge.Builder.NewTx@. The outer prefix
-- @[\"TxCentrifuge\", \"Builder\"]@ is set when creating the tracer via
-- 'Logging.mkCardanoTracer' in 'setupTracers'.
instance Logging.MetaTrace BuilderTrace where
  namespaceFor BuilderNewTx{} = Logging.Namespace [] ["NewTx"]
  severityFor (Logging.Namespace _ ["NewTx"]) _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["NewTx"]) = Just
    "A new transaction was built from input UTxOs, producing output UTxOs."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["NewTx"]
    ]

-- | Machine-readable ('forMachine') and human-readable ('forHuman') rendering
-- of 'BuilderTrace' messages.
--
-- Machine format ('Logging.DNormal'):
--
-- @
-- { \"builder\": \"workload-name\"
-- , \"txId\": \"\<64-char hex\>\"
-- }
-- @
--
-- Machine format ('Logging.DDetailed'): adds the consumed and produced UTxO
-- references as strings.
--
-- @
-- { \"builder\": \"workload-name\"
-- , \"txId\": \"\<64-char hex\>\"
-- , \"inputs\": [\"\<txid\>#\<ix\>\", ...]
-- , \"outputs\": [\"\<txid\>#\<ix\>\", ...]
-- }
-- @
--
-- Machine format ('Logging.DMaximum'): renders each fund in full (a JSON
-- object with its @\"utxo\"@ reference and @\"lovelace\"@ value) and adds the
-- @\"destination\"@ address the transaction pays to.
--
-- @
-- { \"builder\": \"workload-name\"
-- , \"txId\": \"\<64-char hex\>\"
-- , \"destination\": \"addr...\"
-- , \"inputs\": [{\"utxo\": \"\<txid\>#\<ix\>\", \"lovelace\": 1000000}, ...]
-- , \"outputs\": [{\"utxo\": \"\<txid\>#\<ix\>\", \"lovelace\": 500000}, ...]
-- }
-- @
--
-- Human format:
--
-- @
-- NewTx [workload-name] \<txid\> inputs=[...] outputs=[...]
-- @
instance Logging.LogFormatting BuilderTrace where
  forMachine dtal (BuilderNewTx name txId dest inputs outputs) = mconcat $
       [ "builder" .= name
       , "txId"    .= String (Api.serialiseToRawBytesHexText txId)
       ]
    ++ [ "destination" .= Api.serialiseAddress dest
       | dtal >= Logging.DMaximum
       ]
    ++ [ "inputs"  .= map (renderFund dtal) inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "outputs" .= map (renderFund dtal) outputs
       | dtal >= Logging.DDetailed
       ]
  forHuman (BuilderNewTx name txId _dest inputs outputs) =
       "NewTx [" <> Text.pack name <> "] "
    <> Api.serialiseToRawBytesHexText txId
    <> " inputs=["  <> renderFundTxIns inputs  <> "]"
    <> " outputs=[" <> renderFundTxIns outputs <> "]"

-- | Render a single fund for 'forMachine' output.
--
-- * Below 'Logging.DMaximum': just the UTxO reference as a string
--   (@\"\<txid\>#\<ix\>\"@).
-- * 'Logging.DMaximum': a JSON object with @\"utxo\"@ and @\"lovelace\"@ fields.
renderFund :: Logging.DetailLevel -> Fund.Fund -> Value
renderFund dtal fund
  | dtal >= Logging.DMaximum =
      object [ "utxo"     .= Api.renderTxIn (Fund.fundTxIn fund)
             , "lovelace" .= Fund.fundValue fund
             ]
  | otherwise =
      String (Api.renderTxIn (Fund.fundTxIn fund))

-- | Render a list of funds as comma-separated @\"\<txid\>#\<ix\>\"@ references.
renderFundTxIns :: [Fund.Fund] -> Text.Text
renderFundTxIns = Text.intercalate "," . map (Api.renderTxIn . Fund.fundTxIn)

--------------------------------------------------------------------------------
-- Pipe trace messages
--------------------------------------------------------------------------------

-- | Pipe queue events. The pipe reports depth changes on its two queues as
-- items are added or removed but nothing about how a transaction was built or
-- confirmed. The 'String' is the pipe name (the JSON key is @\"pipe\"@ that is
-- currently one pipe per workload, so it is 'Runtime.builderName', but that is
-- not guaranteed to stay one-to-one).
data PipeTrace
  = -- | Inputs were added to the input queue. 'Natural' is the input-queue
    -- depth right after. @[Fund.Fund]@ is the inputs added (their count is
    -- emitted at 'Logging.DDetailed' and above, their full data at
    -- 'Logging.DMaximum').
    PipeInputsEnqueued  !String !Natural [Fund.Fund]
    -- | Inputs were removed from the input queue (taken by the builder).
    -- 'Natural' is the input-queue depth right after. @[Fund.Fund]@ is the
    -- inputs removed (count at 'Logging.DDetailed' and above, full data at
    -- 'Logging.DMaximum').
  | PipeInputsDequeued  !String !Natural [Fund.Fund]
    -- | A payload was added to the payload queue. 'Natural' is the
    -- payload-queue depth right after the write. 'Api.TxId' is the payload's
    -- tx id (emitted only at 'Logging.DMaximum').
  | PipePayloadEnqueued !String !Natural !Api.TxId
    -- | A payload was removed from the payload queue (pulled by a worker).
    -- 'Natural' is the payload-queue depth observed shortly after the pull
    -- (this is read outside the pull transaction, so under concurrency it is a
    -- close approximation, not an exact post-pull snapshot). 'Api.TxId' is the
    -- payload's tx id (emitted only at 'Logging.DMaximum').
  | PipePayloadDequeued !String !Natural !Api.TxId

-- | Namespaces: @TxCentrifuge.Pipe.{InputsEnqueued, InputsDequeued,
-- PayloadEnqueued, PayloadDequeued}@. Outer prefix
-- @[\"TxCentrifuge\", \"Pipe\"]@ is set in 'setupTracers'.
instance Logging.MetaTrace PipeTrace where
  namespaceFor PipeInputsEnqueued{}  = Logging.Namespace [] ["InputsEnqueued"]
  namespaceFor PipeInputsDequeued{}  = Logging.Namespace [] ["InputsDequeued"]
  namespaceFor PipePayloadEnqueued{} = Logging.Namespace [] ["PayloadEnqueued"]
  namespaceFor PipePayloadDequeued{} = Logging.Namespace [] ["PayloadDequeued"]
  severityFor (Logging.Namespace _ ["InputsEnqueued"])  _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["InputsDequeued"])  _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["PayloadEnqueued"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["PayloadDequeued"]) _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["InputsEnqueued"]) = Just
    "Inputs were added to the input queue (count at DDetailed, data at DMaximum)."
  documentFor (Logging.Namespace _ ["InputsDequeued"]) = Just
    "Inputs were removed from the input queue for the builder (count at DDetailed, data at DMaximum)."
  documentFor (Logging.Namespace _ ["PayloadEnqueued"]) = Just
    "A payload was added to the payload queue (its txId is included at DMaximum)."
  documentFor (Logging.Namespace _ ["PayloadDequeued"]) = Just
    "A payload was removed from the payload queue for a worker (txId at DMaximum)."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["InputsEnqueued"]
    , Logging.Namespace [] ["InputsDequeued"]
    , Logging.Namespace [] ["PayloadEnqueued"]
    , Logging.Namespace [] ["PayloadDequeued"]
    ]

-- | Machine format: every Pipe event always carries @pipe@ + @depth@ (the
-- namespace already says which queue). Input events add @count@ at
-- 'Logging.DDetailed' and above and an @inputs@ array of rendered funds at
-- 'Logging.DMaximum' (see 'renderFund'). Payload events add a @txId@ at
-- 'Logging.DMaximum'. Human format shows just the depth (and, for inputs, the
-- count).
instance Logging.LogFormatting PipeTrace where
  forMachine dtal (PipeInputsEnqueued name depth inputs) = mconcat $
       [ "pipe"  .= name
       , "depth" .= depth
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (PipeInputsDequeued name depth inputs) = mconcat $
       [ "pipe"  .= name
       , "depth" .= depth
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (PipePayloadEnqueued name depth txId) = mconcat $
       [ "pipe"  .= name
       , "depth" .= depth
       ]
    ++ [ "txId" .= String (Api.serialiseToRawBytesHexText txId)
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (PipePayloadDequeued name depth txId) = mconcat $
       [ "pipe"  .= name
       , "depth" .= depth
       ]
    ++ [ "txId" .= String (Api.serialiseToRawBytesHexText txId)
       | dtal >= Logging.DMaximum
       ]
  forHuman (PipeInputsEnqueued name depth inputs) =
       "InputsEnqueued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
    <> " count=" <> Text.pack (show (length inputs))
  forHuman (PipeInputsDequeued name depth inputs) =
       "InputsDequeued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
    <> " count=" <> Text.pack (show (length inputs))
  forHuman (PipePayloadEnqueued name depth _txId) =
       "PayloadEnqueued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)
  forHuman (PipePayloadDequeued name depth _txId) =
       "PayloadDequeued [" <> Text.pack name <> "]"
    <> " depth=" <> Text.pack (show depth)

--------------------------------------------------------------------------------
-- Recycler trace messages
--------------------------------------------------------------------------------

-- | Recycler events. The recycler holds a payload's recyclable input UTxOs
-- pending its confirm/dequeue/orphan match, then adds them back onto a pipe's
-- input queue (closing the loop). It reports both when a payload enters the
-- pending backlog ('RecyclerPending') and each add of inputs to a pipe
-- ('RecyclerAddToPipe': which inputs were added, by which recycler, into which
-- pipe). The 'String' names are the recycler and (on 'RecyclerAddToPipe') the
-- pipe (currently both the workload name, but recorded separately as that
-- mapping is not guaranteed to stay one-to-one). The 'Natural' is the resulting
-- pending backlog size.
data RecyclerTrace
  = -- | A payload entered the pending backlog (built or released first, waiting
    -- for its confirm/dequeue/orphan match). 'String' is the recycler name,
    -- 'Natural' the pending backlog size, and @[Fund.Fund]@ the inputs now held
    -- (empty when none are held yet). Count at 'Logging.DDetailed' and above,
    -- full inputs at 'Logging.DMaximum'.
    RecyclerPending   !String !Natural [Fund.Fund]
    -- | Recycled inputs were added back onto a pipe's input queue. First
    -- 'String' is the recycler name, second 'String' the pipe name, 'Natural'
    -- the pending backlog size, @[Fund.Fund]@ the added inputs (count at
    -- 'Logging.DDetailed' and above, full data at 'Logging.DMaximum').
  | RecyclerAddToPipe !String !String !Natural [Fund.Fund]

-- | Namespaces: @TxCentrifuge.Recycler.{Pending, AddToPipe}@. Outer prefix
-- @[\"TxCentrifuge\", \"Recycler\"]@ is set in 'setupTracers'.
instance Logging.MetaTrace RecyclerTrace where
  namespaceFor RecyclerPending{}   = Logging.Namespace [] ["Pending"]
  namespaceFor RecyclerAddToPipe{} = Logging.Namespace [] ["AddToPipe"]
  severityFor (Logging.Namespace _ ["Pending"])   _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["AddToPipe"]) _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["Pending"]) = Just $
    "A payload entered the recycler's pending backlog: one of its two "
    <> "lifecycle signals (build, or a dequeue/confirm/orphan release) "
    <> "has arrived and it awaits the other. The pending count is the "
    <> "current backlog size, not a pipe queue depth."
  documentFor (Logging.Namespace _ ["AddToPipe"]) = Just
    "The recycler added a payload's recycled inputs back onto a pipe's input queue (count at DDetailed, full inputs at DMaximum)."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["Pending"]
    , Logging.Namespace [] ["AddToPipe"]
    ]

-- | Machine format: both events always emit @recycler@ + @pending@ (the pending
-- backlog size, not a queue depth), and 'RecyclerAddToPipe' also emits @pipe@.
-- Both add @count@ at 'Logging.DDetailed' and above and an @inputs@ array of
-- rendered funds at 'Logging.DMaximum' (see 'renderFund'). Human format shows
-- the pending backlog size and the count.
instance Logging.LogFormatting RecyclerTrace where
  forMachine dtal (RecyclerPending recyclerName pending inputs) = mconcat $
       [ "recycler" .= recyclerName
       , "pending"  .= pending
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DMaximum
       ]
  forMachine dtal (RecyclerAddToPipe recyclerName pipeName pending inputs) = mconcat $
       [ "recycler" .= recyclerName
       , "pipe"     .= pipeName
       , "pending"  .= pending
       ]
    ++ [ "count"  .= length inputs
       | dtal >= Logging.DDetailed
       ]
    ++ [ "inputs" .= map (renderFund dtal) inputs
       | dtal >= Logging.DMaximum
       ]
  forHuman (RecyclerPending recyclerName pending inputs) =
       "Pending [" <> Text.pack recyclerName <> "]"
    <> " pending=" <> Text.pack (show pending)
    <> " count=" <> Text.pack (show (length inputs))
  forHuman (RecyclerAddToPipe recyclerName pipeName pending inputs) =
       "AddToPipe [" <> Text.pack recyclerName <> "]"
    <> " pipe=" <> Text.pack pipeName
    <> " pending=" <> Text.pack (show pending)
    <> " count=" <> Text.pack (show (length inputs))

--------------------------------------------------------------------------------
-- Observer trace messages
--------------------------------------------------------------------------------

-- | Observer events. Logged entirely in @Main.hs@ from the observer's on-chain
-- confirmation stream, decoupled from the pipe and from recycling.
data ObserverTrace
  = -- | The observer saw a transaction confirmed or orphaned (rolled back).
    --
    -- * 'String': observer name (from the config's @\"observers\"@ object).
    -- * 'Api.TxId': the confirmed/orphaned transaction's id.
    -- * 'Bool': @True@ if orphaned (rolled back), @False@ if confirmed.
    ObserverAnnounce !String !Api.TxId !Bool

-- | Namespace: @TxCentrifuge.Observer.Announce@. Outer prefix
-- @[\"TxCentrifuge\", \"Observer\"]@ is set in 'setupTracers'.
instance Logging.MetaTrace ObserverTrace where
  namespaceFor ObserverAnnounce{} = Logging.Namespace [] ["Announce"]
  severityFor (Logging.Namespace _ ["Announce"]) _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["Announce"]) = Just
    "The observer saw a transaction confirmed or orphaned (rolled back)."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["Announce"]
    ]

instance Logging.LogFormatting ObserverTrace where
  forMachine _ (ObserverAnnounce observer txId isOrphan) = mconcat
    [ "observer" .= observer
    , "txId"     .= String (Api.serialiseToRawBytesHexText txId)
    , "isOrphan" .= isOrphan
    ]
  forHuman (ObserverAnnounce observer txId isOrphan) =
       "Announce [" <> Text.pack observer <> "]"
    <> " txId=" <> Api.serialiseToRawBytesHexText txId
    <> (if isOrphan then " (orphan)" else " (confirmed)")

--------------------------------------------------------------------------------
-- TxSubmission trace messages
--------------------------------------------------------------------------------

-- | Clean, structured trace of the TxSubmission2 protocol as seen from the
-- generator side. Replaces the verbose @Show@-based tracing in
-- @ouroboros-network@'s @TraceSendRecv@ with fields that are easy to parse and
-- verify.
--
-- Every constructor carries a @target@ field identifying the remote node (the 
-- 'Runtime.targetName' of the 'Runtime.Target').
data TxSubmission
  = -- | The node asked for transaction identifiers (@MsgRequestTxIds@).
    --
    -- * 'String':               target node name.
    -- * @['Api.TxId']@:         TxIds we have not yet received an ACK for.
    -- * 'Int':                  number of TxIds the node is acknowledging (ACK).
    -- * 'Int':                  number of new TxIds the node is requesting (REQ).
    RequestTxIds !String [Api.TxId] !Int !Int
    -- | We replied to @MsgRequestTxIds@ with TxId\/size pairs.
    --
    -- * 'String':               target node name.
    -- * 'Int':                  number of TxIds the node is acknowledging (ACK).
    -- * 'Int':                  number of new TxIds the node is requesting (REQ).
    -- * @['Api.TxId']@:         updated unacked TxIds (after ACK + new announcements).
    -- * @[('Api.TxId','Int')]@: TxIds we announced in this reply with its sizes in bytes.
  | ReplyTxIds   !String !Int !Int [Api.TxId] [(Api.TxId,Int)]
    -- | The node asked for full transactions by TxId (@MsgRequestTxs@).
    --
    -- * 'String':               target node name.
    -- * @['Api.TxId']@:         TxIds the node requested.
  | RequestTxs   !String [Api.TxId]
    -- | We replied to @MsgRequestTxs@ with the requested transactions.
    --
    -- * 'String':               target node name.
    -- * @['Api.TxId']@:         TxIds the node requested.
    -- * @[('Api.TxId','Int')]@: TxIds we actually sent (subset of requested; a
    --                           TxId is missing if it wasn't in the unacked list).
  | ReplyTxs     !String [Api.TxId] [(Api.TxId,Int)]

-- | Namespace: @TxCentrifuge.TxSubmission.*@. The outer prefix is set via
-- 'Logging.mkCardanoTracer' in 'setupTracers'.
instance Logging.MetaTrace TxSubmission where
  namespaceFor RequestTxIds{} = Logging.Namespace [] ["RequestTxIds"]
  namespaceFor ReplyTxIds{}   = Logging.Namespace [] ["ReplyTxIds"]
  namespaceFor RequestTxs{}   = Logging.Namespace [] ["RequestTxs"]
  namespaceFor ReplyTxs{}     = Logging.Namespace [] ["ReplyTxs"]
  severityFor (Logging.Namespace _ ["RequestTxIds"]) _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["ReplyTxIds"])   _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["RequestTxs"])   _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["ReplyTxs"])     _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["RequestTxIds"]) = Just
    "Node requested tx id announcements (blocking or non-blocking)."
  documentFor (Logging.Namespace _ ["ReplyTxIds"]) = Just
    "We replied with tx id announcements and sizes."
  documentFor (Logging.Namespace _ ["RequestTxs"]) = Just
    "Node requested full transactions by TxId."
  documentFor (Logging.Namespace _ ["ReplyTxs"]) = Just
    "We sent the requested transactions."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["RequestTxIds"]
    , Logging.Namespace [] ["ReplyTxIds"]
    , Logging.Namespace [] ["RequestTxs"]
    , Logging.Namespace [] ["ReplyTxs"]
    ]

-- | Machine-readable and human-readable rendering. All TxId lists are omitted
-- below 'Logging.DDetailed' to avoid the cost of hex-encoding every transaction
-- identifier on every protocol round-trip.
--
-- Machine format ('Logging.DNormal'):
--
-- @
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3 }
-- { \"target\": \"n\" }
-- { \"target\": \"n\" }
-- { \"target\": \"n\" }
-- @
--
-- Machine format ('Logging.DDetailed' and above):
--
-- @
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3, \"unacked\": [\"ab..\"] }
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3, \"txs\": [{\"id\":\"ab..\",\"size\":9}], \"unacked\": [\"ab..\"] }
-- { \"target\": \"n\", \"txIds\": [\"ab..\"] }
-- { \"target\": \"n\", \"txs\": [{\"id\":\"ab..\",\"size\":9}], \"requested\": [\"ab..\"] }
-- @
instance Logging.LogFormatting TxSubmission where
  forMachine dtal (RequestTxIds target unacked ack req) = mconcat $
       [ "target"    .= target
       , "ack"       .= ack
       , "req"       .= req
       ]
    ++ [ "unacked"   .= map Api.serialiseToRawBytesHexText unacked
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (ReplyTxIds target ack req unacked announced) = mconcat $
       [ "target"    .= target ]
    ++ [ "ack"       .= ack
       | dtal >= Logging.DDetailed
       ]
    ++ [ "req"       .= req
       | dtal >= Logging.DDetailed
       ]
    ++ [ "txs"       .= map
                          (\(txId,txSize) ->
                            object
                              [ "id"   .= Api.serialiseToRawBytesHexText txId
                              , "size" .= txSize
                              ]
                          )
                          announced
       | dtal >= Logging.DDetailed
       ]
    ++ [ "unacked"   .= map Api.serialiseToRawBytesHexText unacked
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (RequestTxs target txIds) = mconcat $
       [ "target"    .= target ]
    ++ [ "txIds"     .= map Api.serialiseToRawBytesHexText txIds
       | dtal >= Logging.DDetailed
       ]
  forMachine dtal (ReplyTxs target requested sent) = mconcat $
       [ "target"    .= target ]
    ++ [ "txs"       .= map
                          (\(txId,txSize) ->
                            object
                              [ "id"   .= Api.serialiseToRawBytesHexText txId
                              , "size" .= txSize
                              ]
                          )
                          sent
       | dtal >= Logging.DDetailed
       ]
    ++ [ "requested" .= map Api.serialiseToRawBytesHexText requested
       | dtal >= Logging.DDetailed
       ]
  forHuman (RequestTxIds target _unacked ack req) =
       "RequestTxIds [" <> Text.pack target <> "]"
    <> " ack=" <> Text.pack (show ack)
    <> " req=" <> Text.pack (show req)
  forHuman (ReplyTxIds target ack req _unacked _announced) =
       "ReplyTxIds [" <> Text.pack target <> "]"
    <> " ack=" <> Text.pack (show ack)
    <> " req=" <> Text.pack (show req)
  forHuman (RequestTxs target _txIds) =
       "RequestTxs [" <> Text.pack target <> "]"
  forHuman (ReplyTxs target _requested _sent) =
       "ReplyTxs [" <> Text.pack target <> "]"

