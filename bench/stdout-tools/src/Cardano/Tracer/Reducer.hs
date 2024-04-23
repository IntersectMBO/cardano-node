{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

module Cardano.Tracer.Reducer
  (
    Reducer (..)

  -- Plain counters (do not inspect the trace message)
  , CountLines (..)
  , CountTraces (..)

  -- Basic time reducers (only inspect the timestamp).
  , Silences (..)

  -- Basic namespace reducers (only inspect the namespace).
  , CountNS (..)

  -- Others (inspect the JSON data past the timestamp and namespace).
  , MissedSlots (..)
  , ResourcesChanges (..)
  , UtxoSize (..)

  ) where

--------------------------------------------------------------------------------

-- base.
import           Control.Monad (when, foldM)
import           Data.Kind (Type)
import           Data.Foldable (toList)
-- package: time.
import           Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
-- package: text.
import qualified Data.Text as Text
-- package: containers.
import qualified Data.Sequence as Seq
-- package: aeson.
import qualified Data.Aeson as Aeson
-- library.
import qualified Cardano.Tracer.Trace as Trace
import qualified Cardano.Tracer.Filter as Filter

--------------------------------------------------------------------------------

-- TODO: Show should not be here
class Show r => Reducer r where
  type family Accum r :: Type
  initialOf :: r -> Accum r
  reducerOf :: r -> Accum r -> Text.Text -> Accum r
  showAns   :: r -> Accum r -> String
  printAns  :: r -> Accum r -> IO ()
  printAns r acc = putStrLn $ showAns r acc

--------------------------------------------------------------------------------

data CountLines = CountLines
  deriving Show

data CountTraces = CountTraces
  deriving Show

newtype Silences = Silences NominalDiffTime
  deriving Show

newtype CountNS = CountNS Text.Text
  deriving Show

data MissedSlots = MissedSlots
  deriving Show

newtype ResourcesChanges = ResourcesChanges (Trace.DataResources -> Integer)

instance Show ResourcesChanges where
  show _ = "ResourcesChanges"

data UtxoSize = UtxoSize
  deriving Show

--------------------------------------------------------------------------------

instance Reducer CountLines where
  type instance Accum CountLines = Int
  initialOf _ = 0
  reducerOf _ l _ = l + 1
  showAns   _ = show

instance Reducer CountTraces where
  type instance Accum CountTraces = Int
  initialOf _ = 0
  reducerOf _ l textLine = do
    let traceFilter =
             return textLine
         >>= Filter.filterOf  Filter.ParseTrace
         >>= Filter.filterOf  Filter.RightTrace
    case traceFilter of
      Nothing -> l
      Just _ -> l + 1
  showAns   _ = show

instance Reducer Silences where
  type instance Accum Silences = (Maybe UTCTime, Seq.Seq (NominalDiffTime, UTCTime, UTCTime))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf (Silences s) ans@(maybePrevAt, sq) textLine = do
    let traceFilter =
             return textLine
         >>= Filter.filterOf  Filter.ParseTrace
         >>= Filter.filterOf  Filter.RightTrace
         >>= Filter.filterOf  Filter.RightAt
    case traceFilter of
      Nothing -> ans
      Just (thisTraceAt, _) -> 
        case maybePrevAt of
          Nothing -> (Just thisTraceAt, sq)
          (Just prevTraceAt) ->
            let diffTime = diffUTCTime thisTraceAt prevTraceAt
            in  if diffTime >= s
            then (Just thisTraceAt, sq Seq.|> (diffTime, prevTraceAt, thisTraceAt))
            else (Just thisTraceAt, sq)
  showAns   _ = show
  printAns _ (_,sq) = mapM_
    (\(ndt, t1, _) ->
      putStrLn $ show ndt ++ " (" ++ show t1 ++ ")"
    )
    (toList sq)

instance Reducer CountNS where
  type instance Accum CountNS = Int
  initialOf _ = 0
  reducerOf (CountNS ns) l textLine = do
    let traceFilter =
             return textLine
         >>= Filter.filterOf  Filter.ParseTrace
         >>= Filter.filterOf  Filter.RightTrace
         -- Filtering first by namespace is way faster than directly decoding JSON.
         >>= Filter.filterOf (Filter.Namespace ns)
    case traceFilter of
      Nothing -> l
      Just _ -> l + 1
  showAns  _ = show

instance Reducer MissedSlots where
  type instance Accum MissedSlots = (Maybe Integer, Seq.Seq Integer)
  initialOf _ = (Nothing, Seq.empty)
  reducerOf MissedSlots ans@(maybePrevSlot, !sq) textLine = do
    let traceFilter =
             return textLine
         >>= Filter.filterOf  Filter.ParseTrace
         >>= Filter.filterOf  Filter.RightTrace
         -- Filtering first by namespace is way faster than directly decoding JSON.
         >>= Filter.filterOf (Filter.Namespace "Forge.Loop.StartLeadershipCheckPlus")
    case traceFilter of
      Nothing -> ans
      Just (Trace.Trace _ _ remainder) ->
        case Aeson.eitherDecodeStrictText remainder of
        (Right !dataWithSlot) ->
          -- TODO: Use `unsnoc` when available
          let actualSlot = Trace.slot $ Trace.remainderData dataWithSlot
          in case maybePrevSlot of
            Nothing -> (Just actualSlot, Seq.empty)
            (Just prevSlot) ->
              if actualSlot == prevSlot + 1
              then (Just actualSlot, sq)
              else (Just actualSlot, sq Seq.>< Seq.fromList [(prevSlot+1)..(actualSlot-1)])
        (Left _) -> ans
  showAns _ = show
  printAns _ (_, sq) = do
    ans <- foldM
      (\maybePrevSlot lostSlot ->
        case maybePrevSlot of
          Nothing -> putStr (show lostSlot) >> return (Just lostSlot)
          (Just prevSlot) ->
            if prevSlot + 1 == lostSlot
            then return (Just lostSlot)
            else putStrLn (".." ++ show prevSlot) >> return Nothing
      )
      Nothing
      (toList sq)
    when (ans /= Nothing) (putStrLn "")

instance Reducer ResourcesChanges where
  type instance Accum ResourcesChanges = (Maybe Integer, Seq.Seq (UTCTime, Integer))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf (ResourcesChanges f) ans@(maybePrevResource, sq) textLine = do
    let traceFilter =
             return textLine
         >>= Filter.filterOf  Filter.ParseTrace
         >>= Filter.filterOf  Filter.RightTrace
         -- Filtering first by namespace is way faster than directly decoding JSON.
         >>= Filter.filterOf (Filter.Namespace "Resources")
         >>= Filter.filterOf  Filter.RightAt
         >>= Filter.filterOf (Filter.Resource f)
    case traceFilter of
      Nothing -> ans
      Just (at, actualResource) ->
        case maybePrevResource of
          Nothing -> (Just actualResource, Seq.singleton (at, actualResource))
          (Just prevResource) ->
            if actualResource == prevResource
            then ans
            else (Just actualResource, sq Seq.|> (at, actualResource))
  showAns _ = show
  printAns _ (_, sq) = mapM_
    (\(t,h) -> putStrLn $ show t ++ ": " ++ show h)
    (toList sq)

{-
  { "at":"2024-04-05T23:13:43.425867818Z"
  , "ns":"Forge.Loop.StartLeadershipCheckPlus"
  , "data":{
      "chainDensity":0
    , "delegMapSize":1002000
    , "kind":"TraceStartLeadershipCheck"
    , "slot":0
    , "utxoSize":41002003
  }
  , "sev":"Info"
  , "thread":"270"
  , "host":"client-ssd-eu-01"
}
-}
instance Reducer UtxoSize where
  type instance Accum UtxoSize = (Maybe Integer, Seq.Seq (UTCTime, Integer))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf UtxoSize ans@(maybePrevSize, sq) textLine = do
    let traceFilter =
             return textLine
         >>= Filter.filterOf  Filter.ParseTrace
         >>= Filter.filterOf  Filter.RightTrace
         -- Filtering first by namespace is way faster than directly decoding JSON.
         >>= Filter.filterOf (Filter.Namespace "Forge.Loop.StartLeadershipCheckPlus")
         >>= Filter.filterOf  Filter.RightAt
         >>= Filter.filterOf  Filter.UtxoSize
    case traceFilter of
      Nothing -> ans
      Just (at, actualSize) ->
        case maybePrevSize of
          Nothing -> (Just actualSize, Seq.singleton (at, actualSize))
          (Just prevSize) ->
            if actualSize == prevSize
            then ans
            else (Just actualSize, sq Seq.|> (at, actualSize))
  showAns _ = show
  printAns _ (_, sq) = mapM_
    (\(t,h) -> putStrLn $ show t ++ ": " ++ show h)
    (toList sq)
