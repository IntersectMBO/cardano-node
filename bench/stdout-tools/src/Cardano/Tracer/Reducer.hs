{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

module Cardano.Tracer.Reducer
  ( Reducer (..)
  , CountLines (..)
  , CountStartLeadershipCheckPlus (..)
  , HeapChanges (..)
  , MissedSlots (..)
  , OneSecondSilences (..)
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

--------------------------------------------------------------------------------

class Show r => Reducer r where
  type family Accum r :: Type
  initialOf :: r -> Accum r
  reducerOf :: r -> Accum r -> (Either Text.Text Trace.Trace) -> Accum r
  showAns   :: r -> Accum r -> String
  printAns  :: r -> Accum r -> IO ()
  printAns r acc = putStrLn $ showAns r acc

--------------------------------------------------------------------------------

data CountLines = CountLines
  deriving Show

data CountStartLeadershipCheckPlus = CountStartLeadershipCheckPlus
  deriving Show

data HeapChanges = HeapChanges
  deriving Show

data MissedSlots = MissedSlots
  deriving Show

data OneSecondSilences = OneSecondSilences
  deriving Show

--------------------------------------------------------------------------------

instance Reducer CountLines where
  type instance Accum CountLines = Int
  initialOf _ = 0
  reducerOf _ = (\l _ -> l + 1)
  showAns   _ = show

instance Reducer CountStartLeadershipCheckPlus where
  type instance Accum CountStartLeadershipCheckPlus = Int
  initialOf _ = 0
  reducerOf _ = (\l eitherTrace ->
    case eitherTrace of
      (Left _) -> l
      (Right trace) ->
        if Trace.ns trace == "Forge.Loop.StartLeadershipCheckPlus"
        then l + 1
        else l
    )
  showAns  _ = show

instance Reducer HeapChanges where
  type instance Accum HeapChanges = (Maybe Integer, Seq.Seq (UTCTime, Integer))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ ans (Left _) = ans
  reducerOf _ ans@(maybePrevHeap, sq) (Right trace) =
    case Aeson.eitherDecodeStrictText (Trace.remainder trace) of
      (Right !remainder) ->
        -- TODO: Use `unsnoc` when available
        let actualHeap = Trace.resourcesHeap $ Trace.remainderData remainder
        in case maybePrevHeap of
          Nothing -> (Just actualHeap, Seq.singleton (Trace.at trace, actualHeap))
          (Just prevHeap) ->
            if actualHeap == prevHeap
            then ans
            else (Just actualHeap, sq Seq.|> (Trace.at trace, actualHeap))
      (Left _) -> ans
  showAns _ = show
  printAns _ (_, sq) = mapM_
    (\(t,h) -> putStrLn $ show t ++ ": " ++ show h)
    (toList sq)

instance Reducer MissedSlots where
  type instance Accum MissedSlots = (Maybe Integer, Seq.Seq Integer)
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ ans (Left _) = ans
  reducerOf _ ans@(maybePrevSlot, !sq) (Right !(Trace.Trace _ "Forge.Loop.StartLeadershipCheckPlus" remainder)) =
    case Aeson.eitherDecodeStrictText remainder of
      (Right !dataWithSlot) ->
        -- TODO: Use `unsnoc` when available
        let actualSlot = Trace.slot $ Trace.remainderData dataWithSlot
        in case maybePrevSlot of
          Nothing -> (Just actualSlot, Seq.empty)
          (Just prevSlot) ->
            if actualSlot == prevSlot + 1
            then (Just actualSlot, sq)
            else (Just actualSlot, sq Seq.>< (Seq.fromList [(prevSlot+1)..(actualSlot-1)]))
      (Left _) -> ans
  reducerOf _ ans (Right _) = ans
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

instance Reducer OneSecondSilences where
  type instance Accum OneSecondSilences = (Maybe UTCTime, Seq.Seq (NominalDiffTime, UTCTime, UTCTime))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ (Nothing, sq) (Left _) = (Nothing, sq)
  reducerOf _ (Nothing, sq) (Right trace) = (Just $ Trace.at trace, sq)
  reducerOf _ (Just prevTraceAt, sq) (Left _) = (Just prevTraceAt, sq)
  reducerOf _ (Just prevTraceAt, sq) (Right trace) =
    let thisTraceAt = Trace.at trace
        diffTime = diffUTCTime thisTraceAt prevTraceAt
    in  if diffTime >  fromInteger 2
    then (Just thisTraceAt, sq Seq.|> (diffTime, prevTraceAt, thisTraceAt))
    else (Just thisTraceAt, sq)
  showAns   _ = show
  printAns _ (_,sq) = mapM_
    (\(ndt, t1, _) ->
      putStrLn $ (show ndt) ++ " (" ++ (show t1) ++ ")"
    )
    (toList sq)
