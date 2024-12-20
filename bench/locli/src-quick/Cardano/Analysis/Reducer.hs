
module Cardano.Analysis.Reducer
       (module Cardano.Analysis.Reducer)
       where

import           Cardano.Api (SlotNo)

import           Cardano.Analysis.Reducer.Util
import           Cardano.Unlog.LogObject (LOBody (..), LogObject (..))
import           Cardano.Util

import           Prelude hiding (log, seq)

import           Data.Reducer
import           Data.Word


data TxsInMempool = TxsInMempoolPerSlot

instance Reducer TxsInMempool where
  type instance Elem   TxsInMempool = BySlot LogObject
  type instance Accum  TxsInMempool = ([(SlotNo, Word64)], Word64)
  type instance Result TxsInMempool = [(SlotNo, Word64)]

  initialOf _ = ([], 0)

  -- every slot (x-axis) contains one value on the y-axis (ideal for plotting a curve)
  reducerOf _ (acc, currentTxns) (BySlot (slotNo, objs)) =
    case safeLast objs of
      [ LogObject{loBody = LOMempoolTxs txns} ]
        -> ((slotNo, txns) : acc, txns)
      _ -> ((slotNo, currentTxns) : acc, currentTxns)

  resultOf _ = reverse . fst


data ResourceMeasure = ResourceMeasurePerSlot

instance Reducer ResourceMeasure where
  type instance Elem   ResourceMeasure = BySlot Word64
  type instance Accum  ResourceMeasure = [(SlotNo, Word64)]
  type instance Result ResourceMeasure = [(SlotNo, Word64)]

  initialOf _ = []

  reducerOf _ acc (BySlot (slotNo, objs)) =
    case safeLast objs of
      [measurement] -> (slotNo, measurement) : acc
      _             -> acc

  resultOf _ = reverse


data Silence = Silence {threshold :: NominalDiffTime, startTime :: UTCTime}

instance Reducer Silence where
  type instance Elem   Silence = (UTCTime, SMaybe SlotNo)
  type instance Accum  Silence = ([(SlotNo, NominalDiffTime)], (UTCTime, SlotNo))
  type instance Result Silence = [(SlotNo, NominalDiffTime)]

  initialOf Silence{startTime} = ([], (startTime, 0))

  reducerOf Silence{threshold} (acc, (lastHeard, slot)) (now, mSlot) =
    (acc', (now, slot'))
    where
      slot'   = fromSMaybe slot mSlot
      silence = now `diffUTCTime` lastHeard
      acc'
        | silence >= threshold  = (slot', silence) : acc
        | otherwise             = acc

  resultOf _ = reverse . fst
