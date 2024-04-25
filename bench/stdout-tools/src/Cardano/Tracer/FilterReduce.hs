{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

module Cardano.Tracer.FilterReduce
  (
    FilterReduce (MkFilterReduce), (<|>), filterReduce

  , countLines, countLinesFR
  , countTraces, countTracesFR

  , silences, silencesFR

  , countNamespace, countNamespaceFR

  , missedSlots, missedSlotsFR

  , utxoSize, utxoSizeFR
  , heapChanges, heapChangesFR
  , liveChanges, liveChangesFR
  , rssChanges, rssChangesFR
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

countLines :: (Filter.Id Text.Text, Reducer.Count Text.Text)
countLines = (,)
  Filter.Id
  Reducer.Count

countLinesFR :: FilterReduce
countLinesFR = uncurry MkFilterReduce countLines

countTraces :: (Filter.Compose Filter.ParseTrace Filter.RightTrace, Reducer.Count Trace.Trace)
countTraces = (,)
  (Filter.ParseTrace Filter.<-> Filter.RightTrace)
  Reducer.Count

countTracesFR :: FilterReduce
countTracesFR = uncurry MkFilterReduce countTraces

silences :: NominalDiffTime
         -> ( Filter.Compose
                (Filter.Compose Filter.ParseTrace Filter.RightTrace)
                Filter.RightAt
            , Reducer.Silences
            )
silences s = (,)
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.RightAt
  )
  (Reducer.Silences s)

silencesFR :: NominalDiffTime -> FilterReduce
silencesFR s = uncurry MkFilterReduce (silences s)

countNamespace :: Text.Text
               -> ( Filter.Compose
                   (Filter.Compose Filter.ParseTrace Filter.RightTrace)
                   Filter.Namespace
                  , Reducer.Count Trace.Trace
                  )
countNamespace ns = (,)
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.Namespace ns
  )
  (Reducer.Count :: Reducer.Count Trace.Trace)

countNamespaceFR :: Text.Text -> FilterReduce
countNamespaceFR ns = uncurry MkFilterReduce (countNamespace ns)

missedSlots :: ( Filter.Compose
                   (Filter.Compose
                     (Filter.Compose Filter.ParseTrace Filter.RightTrace)
                     Filter.Namespace
                   )
                   (Filter.Aeson (Trace.Remainder Trace.DataWithSlot))
               , Reducer.MissedSlots
               )
missedSlots = (,)
  ( Filter.ParseTrace
    Filter.<->
    Filter.RightTrace
    Filter.<->
    Filter.Namespace "Forge.Loop.StartLeadershipCheckPlus"
    Filter.<->
    (Filter.Aeson :: Filter.Aeson (Trace.Remainder Trace.DataWithSlot))
  )
  Reducer.MissedSlots

missedSlotsFR :: FilterReduce
missedSlotsFR = uncurry MkFilterReduce missedSlots

utxoSize :: ( Filter.Compose
               (Filter.Compose
                 (Filter.Compose
                   (Filter.Compose Filter.ParseTrace Filter.RightTrace)
                   Filter.Namespace
                 )
                 Filter.RightAt
               )
               (Filter.AesonWithAt (Trace.Remainder Trace.DataWithUtxoSize))
           , Reducer.Changes (Trace.Remainder Trace.DataWithUtxoSize)
           )
utxoSize = (,)
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

utxoSizeFR :: FilterReduce
utxoSizeFR = uncurry MkFilterReduce utxoSize

heapChanges :: ( Filter.Compose
                   (Filter.Compose
                     (Filter.Compose
                       (Filter.Compose Filter.ParseTrace Filter.RightTrace)
                       Filter.Namespace
                     )
                     Filter.RightAt
                   )
                   (Filter.AesonWithAt (Trace.Remainder Trace.DataResources))
               , Reducer.Changes (Trace.Remainder Trace.DataResources)
               )
heapChanges = (,)
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

heapChangesFR :: FilterReduce
heapChangesFR = uncurry MkFilterReduce heapChanges

liveChanges :: ( Filter.Compose
                   (Filter.Compose
                     (Filter.Compose
                       (Filter.Compose Filter.ParseTrace Filter.RightTrace)
                       Filter.Namespace
                     )
                     Filter.RightAt
                   )
                   (Filter.AesonWithAt (Trace.Remainder Trace.DataResources))
               , Reducer.Changes (Trace.Remainder Trace.DataResources)
               )
liveChanges = (,)
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

liveChangesFR :: FilterReduce
liveChangesFR = uncurry MkFilterReduce liveChanges

rssChanges :: ( Filter.Compose
                  (Filter.Compose
                    (Filter.Compose
                      (Filter.Compose Filter.ParseTrace Filter.RightTrace)
                      Filter.Namespace
                    )
                    Filter.RightAt
                  )
                  (Filter.AesonWithAt (Trace.Remainder Trace.DataResources))
              , Reducer.Changes (Trace.Remainder Trace.DataResources)
              )
rssChanges = (,)
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

rssChangesFR :: FilterReduce
rssChangesFR = uncurry MkFilterReduce rssChanges

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
