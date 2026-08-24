{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Walking one mempool snapshot and tallying what colours it holds.
module Cardano.Benchmarking.MempoolMonitor.Snapshot
  ( Snapshot (..)
  , ColorKey (..)
  , monitorClient
  , txColorKey
  , colorKeyLabel
  , shares
  , distinctColors
  , localShare
  ) where

import Cardano.Api qualified as Api
import Cardano.Benchmarking.TxFirehose.Color (Color, colorFromOctets, colorHex, colorMetadataLabel)
import Control.Concurrent (threadDelay)
import Cardano.Ledger.Api (auxDataTxL)
import Cardano.Ledger.Api.Tx.AuxData (Metadatum (B), metadataTxAuxDataL)
import Data.Array.Byte (ByteArray)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Primitive.ByteArray (indexByteArray, sizeofByteArray)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word8)
import Lens.Micro ((^.))
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
  ( ClientStAcquired (SendMsgGetSizes, SendMsgNextTx, SendMsgRelease)
  , ClientStIdle (SendMsgAcquire)
  , LocalTxMonitorClient (LocalTxMonitorClient)
  )
import Ouroboros.Network.Protocol.LocalTxMonitor.Type (MempoolSizeAndCapacity)

-- | A transaction is either tagged with a colour or it is not ours to explain.
data ColorKey
  = Colored !Color
  | Uncolored
  deriving (Eq, Ord, Show)

-- | One drained snapshot: what the mempool held, and what it cost to find out.
data Snapshot = Snapshot
  { snapSlot :: !Api.SlotNo
  , snapSizes :: !MempoolSizeAndCapacity
  , snapColors :: !(Map ColorKey Int)
  , snapDrained :: !Int
  , snapDuration :: !NominalDiffTime
  , snapTaken :: !UTCTime
  }

-- | Acquire, size, drain, release, wait, repeat.
--
-- The snapshot is acquired once per round and walked within it, so the whole
-- tally is consistent. Draining is a round trip per transaction, which is why
-- the interval between rounds is generous.
monitorClient ::
  Int ->
  (Snapshot -> IO ()) ->
  LocalTxMonitorClient Api.TxIdInMode Api.TxInMode Api.SlotNo IO ()
monitorClient intervalMicros emit = LocalTxMonitorClient (pure idle)
 where
  idle =
    SendMsgAcquire $ \slot -> do
      started <- getCurrentTime
      pure . SendMsgGetSizes $ \sizes ->
        drain slot sizes started Map.empty 0

  drain slot sizes started !tally !drained =
    pure . SendMsgNextTx $ \case
      Just tx -> drain slot sizes started (count (txColorKey tx) tally) (drained + 1)
      Nothing -> do
        finished <- getCurrentTime
        emit
          Snapshot
            { snapSlot = slot
            , snapSizes = sizes
            , snapColors = tally
            , snapDrained = drained
            , snapDuration = finished `diffUTCTime` started
            , snapTaken = finished
            }
        pure . SendMsgRelease $ do
          threadDelay intervalMicros
          pure idle

  count key = Map.insertWith (+) key 1

-- | The colour a transaction carries, if any.
--
-- Reads the ledger tx's auxiliary data directly rather than going through
-- cardano-api's transaction body view, which would build a record per tx and is
-- worth avoiding when a drain walks tens of thousands of them.
txColorKey :: Api.TxInMode -> ColorKey
txColorKey = \case
  Api.TxInByronSpecial{} -> Uncolored
  Api.TxInMode sbe (Api.ShelleyTx _ ledgerTx) ->
    Api.shelleyBasedEraConstraints sbe $
      case ledgerTx ^. auxDataTxL of
        SNothing -> Uncolored
        SJust auxData ->
          case Map.lookup colorMetadataLabel (auxData ^. metadataTxAuxDataL) of
            Just (B bytes) -> maybe Uncolored Colored (colorFromOctets (octets bytes))
            _ -> Uncolored

-- | The ledger keeps metadata bytes in a 'ByteArray', so unpack the few we want.
octets :: ByteArray -> [Word8]
octets ba = [indexByteArray ba i | i <- [0 .. sizeofByteArray ba - 1]]

-- | How a colour prints when it needs a name.
colorKeyLabel :: ColorKey -> String
colorKeyLabel = \case
  Colored c -> colorHex c
  Uncolored -> "(none)"

-- | Colour shares of the snapshot, largest first.
shares :: Snapshot -> [(ColorKey, Int, Double)]
shares snap =
  [ (key, n, fromIntegral n / fromIntegral total)
  | (key, n) <- sortOn (negate . snd) (Map.toList (snapColors snap))
  ]
 where
  total = max 1 (snapDrained snap)

-- | Colours actually present, ignoring untagged transactions.
distinctColors :: Snapshot -> Int
distinctColors = length . filter isColored . Map.keys . snapColors
 where
  isColored = \case
    Colored{} -> True
    Uncolored -> False

-- | Share held by this node's own colour, when one was declared.
localShare :: Maybe Color -> Snapshot -> Maybe Double
localShare mOwn snap = do
  own <- mOwn
  let n = Map.findWithDefault 0 (Colored own) (snapColors snap)
  pure (fromIntegral n / fromIntegral (max 1 (snapDrained snap)))
