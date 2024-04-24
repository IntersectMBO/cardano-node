{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

module Cardano.Tracer.FilterReduce
  (
    FilterReduce (MkFilterReduce), (<|>), filterReduce

  , countLines
  , countTraces

  , silences

  , countNamespace

  , missedSlots

  , utxoSize
  , heapChanges, liveChanges, rssChanges
  ) where

--------------------------------------------------------------------------------

-- base.
--
-- package: time.
import           Data.Time.Clock (NominalDiffTime)
-- package: text.
import qualified Data.Text as Text
-- library.
import qualified Data.Log as Log
import qualified Cardano.Tracer.Trace as Trace
import qualified Cardano.Tracer.Filter as Filter
import qualified Cardano.Tracer.Reducer as Reducer

--------------------------------------------------------------------------------

data FilterReduce = forall f r.
                    ( Show f
                    , Filter.Filter f
                    , Show r
                    , Reducer.Reducer r
                    , Filter.FilterInput  f ~ Text.Text
                    , Filter.FilterOutput f ~ Reducer.Elem r
                    , Eq (Reducer.Elem r)
                    ) => MkFilterReduce f r

instance Show FilterReduce where
  show (MkFilterReduce f r) = show f ++ " <|> " ++ show r

(<|>) :: ( Show f
         , Filter.Filter f
         , Show r
         , Reducer.Reducer r
         , Filter.FilterInput  f ~ Text.Text
         , Filter.FilterOutput f ~ Reducer.Elem r
         , Eq (Reducer.Elem r)
         )
      => f -> r -> FilterReduce
f <|> r = MkFilterReduce f r

--------------------------------------------------------------------------------

filterReduce :: ( Filter.Filter f
                , Reducer.Reducer r
                , Filter.FilterInput  f ~ Text.Text
                , Filter.FilterOutput f ~ Reducer.Elem r
                )
             =>
                f -> r -> FilePath -> IO (Reducer.Accum r)
filterReduce f r filePath = do
  Log.lineFoldl'
    (\ !acc textLine ->
      case Filter.filterOf f textLine of
        (Just filterOutput) -> Reducer.reducerOf r acc filterOutput
        Nothing -> acc
    )
    (Reducer.initialOf r)
    filePath

--------------------------------------------------------------------------------

countLines :: FilterReduce
countLines = MkFilterReduce
  Filter.Id
  (Reducer.Count :: Reducer.Count Text.Text)

countTraces :: FilterReduce
countTraces = MkFilterReduce
  (Filter.ParseTrace Filter.<-> Filter.RightTrace)
  (Reducer.Count :: Reducer.Count Trace.Trace)

silences :: NominalDiffTime -> FilterReduce
silences s = MkFilterReduce
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.RightAt
  )
  (Reducer.Silences s)

countNamespace :: Text.Text -> FilterReduce
countNamespace ns = MkFilterReduce
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.Namespace ns
  )
  (Reducer.Count :: Reducer.Count Trace.Trace)

missedSlots :: FilterReduce
missedSlots = MkFilterReduce
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.Namespace "Forge.Loop.StartLeadershipCheckPlus"
    Filter.<->
    (Filter.Aeson :: Filter.Aeson Trace.DataWithSlot)
  )
  Reducer.MissedSlots

utxoSize :: FilterReduce
utxoSize = MkFilterReduce
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.Namespace "Forge.Loop.StartLeadershipCheckPlus"
    Filter.<->
    Filter.RightAt
    Filter.<->
    (Filter.AesonWithAt :: Filter.AesonWithAt (Trace.Remainder Trace.DataWithUtxoSize))
  )
  (Reducer.Changes (Trace.utxoSize . Trace.remainderData))

heapChanges :: FilterReduce
heapChanges = MkFilterReduce
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.Namespace "Resources"
    Filter.<->
    Filter.RightAt
    Filter.<->
    (Filter.AesonWithAt :: Filter.AesonWithAt (Trace.Remainder Trace.DataResources))
  )
  (Reducer.Changes (Trace.resourcesHeap . Trace.remainderData))

liveChanges :: FilterReduce
liveChanges = MkFilterReduce
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.Namespace "Resources"
    Filter.<->
    Filter.RightAt
    Filter.<->
    (Filter.AesonWithAt :: Filter.AesonWithAt (Trace.Remainder Trace.DataResources))
  )
  (Reducer.Changes (Trace.resourcesLive . Trace.remainderData))

rssChanges :: FilterReduce
rssChanges = MkFilterReduce
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.Namespace "Resources"
    Filter.<->
    Filter.RightAt
    Filter.<->
    (Filter.AesonWithAt :: Filter.AesonWithAt (Trace.Remainder Trace.DataResources))
  )
  (Reducer.Changes (Trace.resourcesRSS . Trace.remainderData))

--------------------------------------------------------------------------------

type family Id a where
  Id a = a

type family Composable i o where
  Composable i o = i -> (i -> Maybe o)
{--
  Composable f1 f2 =  Filter.FilterInput f1
                   -> (Filter.FilterOutput f1 -> Maybe (Filter.FilterOutput f2))
                   -> Maybe (Filter.FilterOutput f2)
--}

_compose :: ( Filter.Filter f1
            , Filter.Filter f2
            , Filter.FilterOutput f1 ~ Filter.FilterInput f2
            )
         => f1 -> f2 -> Filter.FilterInput f1 -> Maybe (Filter.FilterOutput f2)
_compose f1 f2 inputOfF1 =
  Filter.filterOf f1 inputOfF1 >>= Filter.filterOf f2
