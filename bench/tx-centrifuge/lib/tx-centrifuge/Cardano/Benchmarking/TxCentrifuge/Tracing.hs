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
  ( Tracers (..)
  , setupTracers, nullTracers
  , BuilderTrace (..)
  , mkBuilderNewTx
  , mkBuilderRecycle
  , TxSubmission (..)
    -- * Re-exports
  , traceWith
  ) where

--------------------------------------------------------------------------------

----------
-- base --
----------
import Control.Exception (SomeException, try)
-----------
-- aeson --
-----------
import Data.Aeson (Value (String), (.=), object)
-----------------
-- cardano-api --
-----------------
import Cardano.Api qualified as Api
-----------------
-- containers --
-----------------
import Data.Map.Strict qualified as Map
-------------------
-- contra-tracer --
-------------------
import "contra-tracer" Control.Tracer (Tracer (..), traceWith)
-------------------------
-- ouroboros-consensus --
-------------------------
import Ouroboros.Consensus.Cardano qualified as Consensus (CardanoBlock)
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Mempool
import Ouroboros.Consensus.Shelley.Eras qualified as Eras
-- Orphan instances needed for LedgerSupportsProtocol (ShelleyBlock ...)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
-----------------------
-- ouroboros-network --
-----------------------
import Ouroboros.Network.Driver.Simple qualified as Simple
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
  { -- | Builder trace: transaction construction and recycling events.
    trBuilder
      :: !(Tracer IO BuilderTrace)
    -- | Clean, structured TxSubmission2 trace emitted by TxSubmission.hs.
  , trTxSubmission
      :: !(Tracer IO TxSubmission)
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
  -- Builder (TxCentrifuge.Builder.NewTx, TxCentrifuge.Builder.Recycle).
  !builderTr <-
    Logging.mkCardanoTracer
      stdoutTrace trForward mbTrEkg ["TxCentrifuge", "Builder"]
  Logging.configureTracers
    configReflection trConfig [builderTr]
  -- TxSubmission (TxCentrifuge.TxSubmission.*).
  !txSubTraceTr <-
    Logging.mkCardanoTracer
      stdoutTrace trForward mbTrEkg ["TxCentrifuge", "TxSubmission"]
  Logging.configureTracers
    configReflection trConfig [txSubTraceTr]
  -- TxSubmission2 (low-level protocol trace).
  !txSub2Trace <-
    Logging.mkCardanoTracer
      stdoutTrace trForward mbTrEkg ["TxSubmission2"]
  Logging.configureTracers
    configReflection trConfig [txSub2Trace]
  -- KeepAlive.
  !keepAliveTr <-
    Logging.mkCardanoTracer
      stdoutTrace trForward mbTrEkg ["KeepAlive"]
  Logging.configureTracers
    configReflection trConfig [keepAliveTr]
  pure Tracers
    { trBuilder =
        Tracer $ Logging.traceWith builderTr
    , trTxSubmission =
        Tracer $ Logging.traceWith txSubTraceTr
    , trTxSubmission2 =
        Tracer $ Logging.traceWith txSub2Trace
    , trKeepAlive =
        Tracer $ Logging.traceWith keepAliveTr
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

-- | Trace messages emitted by the payload builder thread.
--
-- == Builder pipeline
--
-- The builder consumes input UTxOs (unspent funds) from the input queue, builds
-- and signs a transaction, and enqueues the result for workers to submit. Each
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
  = -- | A new transaction was built.
    --
    -- * 'String' — builder name (the workload name, see 'Runtime.builderName').
    --
    -- * 'Api.TxId' — Blake2b-256 hash identifying the new transaction.
    --   Obtain via @'Api.getTxId' ('Api.getTxBody' signedTx)@.
    --
    -- * @['Fund.Fund']@ (inputs) — funds consumed by this transaction. Each
    --   fund's 'Fund.fundTxIn' is a 'Api.TxIn' pointing to an existing UTxO
    --   on the ledger.
    --
    -- * @['Fund.Fund']@ (outputs) — funds produced by this transaction. Each
    --   fund's 'Fund.fundTxIn' is derived from the new 'Api.TxId' and a
    --   sequential 'Api.TxIx' index (0, 1, 2, ...).
    BuilderNewTx !String !Api.TxId [Fund.Fund] [Fund.Fund]
    -- | Output funds were recycled back to the workload's input queue.
    --
    -- * 'String' — builder name (the workload name, see 'Runtime.builderName').
    --
    -- In the tx-centrifuge's closed-loop pipeline, output funds of a
    -- transaction are recycled so they can be consumed by future transactions,
    -- enabling indefinite-duration runs without pre-generating all UTxOs.
    --
    -- * @['Fund.Fund']@ — the recycled output funds.
  | BuilderRecycle !String [Fund.Fund]

-- | Build a 'BuilderNewTx' trace from the builder name, a signed transaction,
-- and its input and output funds. Extracts the 'Api.TxId' from the transaction
-- body.
mkBuilderNewTx :: String               -- ^ Builder name.
               -> Api.Tx Api.ConwayEra -- ^ Signed transaction.
               -> [Fund.Fund]          -- ^ Input funds (consumed).
               -> [Fund.Fund]          -- ^ Output funds (produced).
               -> BuilderTrace
mkBuilderNewTx name tx = BuilderNewTx name (Api.getTxId (Api.getTxBody tx))

-- | Build a 'BuilderRecycle' trace from the builder name and the recycled
-- output funds.
mkBuilderRecycle :: String      -- ^ Builder name.
                 -> [Fund.Fund] -- ^ Recycled output funds.
                 -> BuilderTrace
mkBuilderRecycle = BuilderRecycle

-- | Machine-readable ('forMachine') and human-readable ('forHuman') rendering
-- of 'BuilderTrace' messages.
--
-- Machine format ('Logging.DNormal'):
--
-- @
-- { \"builder\": \"workload-name\"
-- , \"txId\": \"\<64-char hex\>\"
-- , \"inputs\": [\"\<txid\>#\<ix\>\", ...]
-- , \"outputs\": [\"\<txid\>#\<ix\>\", ...]
-- }
-- @
--
-- Machine format ('Logging.DDetailed' and above):
--
-- @
-- { \"builder\": \"workload-name\"
-- , \"txId\": \"\<64-char hex\>\"
-- , \"inputs\": [{\"utxo\": \"\<txid\>#\<ix\>\", \"lovelace\": 1000000}, ...]
-- , \"outputs\": [{\"utxo\": \"\<txid\>#\<ix\>\", \"lovelace\": 500000}, ...]
-- }
-- @
--
-- Human format:
--
-- @
-- NewTx [workload-name] \<txid\> inputs=[\<txid\>#\<ix\>,...] outputs=[\<txid\>#\<ix\>,...]
-- @
instance Logging.LogFormatting BuilderTrace where
  forMachine dtal (BuilderNewTx name txId inputs outputs) = mconcat
    [ "builder" .= name
    , "txId"    .= String (Api.serialiseToRawBytesHexText txId)
    , "inputs"  .= map (renderFund dtal) inputs
    , "outputs" .= map (renderFund dtal) outputs
    ]
  forMachine dtal (BuilderRecycle name outputs) = mconcat
    [ "builder" .= name
    , "outputs" .= map (renderFund dtal) outputs
    ]
  forHuman (BuilderNewTx name txId inputs outputs) =
       "NewTx [" <> Text.pack name <> "] "
    <> Api.serialiseToRawBytesHexText txId
    <> " inputs=["  <> renderFundTxIns inputs  <> "]"
    <> " outputs=[" <> renderFundTxIns outputs <> "]"
  forHuman (BuilderRecycle name outputs) =
       "Recycle [" <> Text.pack name <> "]"
    <> " outputs=[" <> renderFundTxIns outputs <> "]"

-- | Render a single fund for 'forMachine' output.
--
-- * 'Logging.DMinimal', 'Logging.DNormal': just the UTxO reference as a
--   string (@\"\<txid\>#\<ix\>\"@).
-- * 'Logging.DDetailed', 'Logging.DMaximum': a JSON object with @\"utxo\"@
--   and @\"lovelace\"@ fields.
renderFund :: Logging.DetailLevel -> Fund.Fund -> Value
renderFund dtal fund
  | dtal >= Logging.DDetailed =
      object [ "utxo"     .= Api.renderTxIn (Fund.fundTxIn fund)
             , "lovelace" .= Fund.fundValue fund
             ]
  | otherwise =
      String (Api.renderTxIn (Fund.fundTxIn fund))

-- | Render a list of funds as comma-separated @\"\<txid\>#\<ix\>\"@ references.
renderFundTxIns :: [Fund.Fund] -> Text.Text
renderFundTxIns = Text.intercalate "," . map (Api.renderTxIn . Fund.fundTxIn)

-- | Namespace: @TxCentrifuge.Builder.NewTx@ and @TxCentrifuge.Builder.Recycle@.
-- The outer prefix @[\"TxCentrifuge\", \"Builder\"]@ is set when creating the
-- tracer via 'Logging.mkCardanoTracer' in 'setupTracers'.
instance Logging.MetaTrace BuilderTrace where
  namespaceFor BuilderNewTx{}   = Logging.Namespace [] ["NewTx"]
  namespaceFor BuilderRecycle{} = Logging.Namespace [] ["Recycle"]
  severityFor (Logging.Namespace _ ["NewTx"])   _ = Just Logging.Info
  severityFor (Logging.Namespace _ ["Recycle"]) _ = Just Logging.Info
  severityFor _ _ = Nothing
  documentFor (Logging.Namespace _ ["NewTx"]) = Just
    "A new transaction was built from input UTxOs, producing output UTxOs."
  documentFor (Logging.Namespace _ ["Recycle"]) = Just
    "Output UTxOs were recycled back to the workload's input queue for reuse."
  documentFor _ = Nothing
  allNamespaces =
    [ Logging.Namespace [] ["NewTx"]
    , Logging.Namespace [] ["Recycle"]
    ]

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
  = -- | The node asked us to announce transaction identifiers
    -- (@MsgRequestTxIds@).
    --
    -- * 'String':       target node name.
    -- * @['Api.TxId']@: TxIds we have not yet received an ACK for.
    -- * 'Int':          number of TxIds the node is acknowledging (ACK).
    -- * 'Int':          number of new TxIds the node is requesting (REQ).
    RequestTxIds !String [Api.TxId] !Int !Int
    -- | We replied to @MsgRequestTxIds@ with TxId\/size pairs.
    --
    -- * 'String':       target node name.
    -- * 'Int':          number of TxIds the node is acknowledging (ACK).
    -- * 'Int':          number of new TxIds the node is requesting (REQ).
    -- * @['Api.TxId']@: updated unacked TxIds (after ACK + new announcements).
    -- * @['Api.TxId']@: TxIds we announced in this reply.
  | ReplyTxIds   !String !Int !Int [Api.TxId] [Api.TxId]
    -- | The node asked for full transactions by TxId (@MsgRequestTxs@).
    --
    -- * 'String':       target node name.
    -- * @['Api.TxId']@: TxIds the node requested.
  | RequestTxs   !String [Api.TxId]
    -- | We replied to @MsgRequestTxs@ with the requested transactions.
    --
    -- * 'String':       target node name.
    -- * @['Api.TxId']@: TxIds the node requested.
    -- * @['Api.TxId']@: TxIds we actually sent (subset of requested; a TxId is
    --   missing if it was not in the unacked list).
  | ReplyTxs     !String [Api.TxId] [Api.TxId]

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
-- { \"target\": \"n\", \"ack\": 0, \"req\": 3, \"txIds\": [\"ab..\"], \"unacked\": [\"ab..\"] }
-- { \"target\": \"n\", \"txIds\": [\"ab..\"] }
-- { \"target\": \"n\", \"txIds\": [\"ab..\"], \"requested\": [\"ab..\"] }
-- @
instance Logging.LogFormatting TxSubmission where
  forMachine dtal (RequestTxIds target unacked ack req) = mconcat $
    [ "target" .= target
    , "ack"    .= ack
    , "req"    .= req
    ]
    ++ [ "unacked" .= map Api.serialiseToRawBytesHexText unacked
       | dtal >= Logging.DDetailed ]
  forMachine dtal (ReplyTxIds target ack req unacked announced) = mconcat $
    [ "target" .= target ]
    ++ [ "ack"     .= ack
       | dtal >= Logging.DDetailed ]
    ++ [ "req"     .= req
       | dtal >= Logging.DDetailed ]
    ++ [ "txIds"   .= map Api.serialiseToRawBytesHexText announced
       | dtal >= Logging.DDetailed ]
    ++ [ "unacked" .= map Api.serialiseToRawBytesHexText unacked
       | dtal >= Logging.DDetailed ]
  forMachine dtal (RequestTxs target txIds) = mconcat $
    [ "target" .= target ]
    ++ [ "txIds"  .= map Api.serialiseToRawBytesHexText txIds
       | dtal >= Logging.DDetailed ]
  forMachine dtal (ReplyTxs target requested sent) = mconcat $
    [ "target" .= target ]
    ++ [ "txIds"     .= map Api.serialiseToRawBytesHexText sent
       | dtal >= Logging.DDetailed ]
    ++ [ "requested" .= map Api.serialiseToRawBytesHexText requested
       | dtal >= Logging.DDetailed ]
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
