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


-- TODO: Show should not be here
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

data CountTraces = CountTraces
  deriving Show

data Silences = Silences NominalDiffTime
  deriving Show

data CountNS = CountNS Text.Text
  deriving Show

data MissedSlots = MissedSlots
  deriving Show

data ResourcesChanges = ResourcesChanges (Trace.DataResources -> Integer)

instance Show ResourcesChanges where
  show _ = "ResourcesChanges"

--------------------------------------------------------------------------------

instance Reducer CountLines where
  type instance Accum CountLines = Int
  initialOf _ = 0
  reducerOf _ l _ = l + 1
  showAns   _ = show

instance Reducer CountTraces where
  type instance Accum CountTraces = Int
  initialOf _ = 0
  reducerOf _ l (Left _) = l
  reducerOf _ l (Right _) = l + 1
  showAns   _ = show

instance Reducer Silences where
  type instance Accum Silences = (Maybe UTCTime, Seq.Seq (NominalDiffTime, UTCTime, UTCTime))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ (Nothing, sq) (Left _) = (Nothing, sq)
  reducerOf _ (Nothing, sq) (Right (Trace.Trace (Right thisTraceAt) _ _)) = (Just thisTraceAt, sq)
  reducerOf _ (Just prevTraceAt, sq) (Left _) = (Just prevTraceAt, sq)
  reducerOf (Silences s) (Just prevTraceAt, sq) (Right (Trace.Trace (Right thisTraceAt) _ _)) =
    let diffTime = diffUTCTime thisTraceAt prevTraceAt
    in  if diffTime >= s
    then (Just thisTraceAt, sq Seq.|> (diffTime, prevTraceAt, thisTraceAt))
    else (Just thisTraceAt, sq)
  reducerOf _ _ (Right (Trace.Trace (Left err) _ _)) = error err
  showAns   _ = show
  printAns _ (_,sq) = mapM_
    (\(ndt, t1, _) ->
      putStrLn $ show ndt ++ " (" ++ show t1 ++ ")"
    )
    (toList sq)

instance Reducer CountNS where
  type instance Accum CountNS = Int
  initialOf _ = 0
  reducerOf _ l (Left _) = l
  reducerOf (CountNS ns) l (Right (Trace.Trace _ text _)) =
    if text == ns
    then l + 1
    else l
  showAns  _ = show

instance Reducer MissedSlots where
  type instance Accum MissedSlots = (Maybe Integer, Seq.Seq Integer)
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ ans (Left _) = ans
  reducerOf _ ans@(maybePrevSlot, !sq) (Right (Trace.Trace _ "Forge.Loop.StartLeadershipCheckPlus" remainder)) =
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

instance Reducer ResourcesChanges where
  type instance Accum ResourcesChanges = (Maybe Integer, Seq.Seq (UTCTime, Integer))
  initialOf _ = (Nothing, Seq.empty)
  reducerOf _ ans (Left _) = ans
  -- Filtering first by namespace is way faster than directly decoding JSON.
  reducerOf (ResourcesChanges f) ans@(maybePrevResource, sq) (Right (Trace.Trace eitherAt "Resources" remainder)) =
    case Aeson.eitherDecodeStrictText remainder of
      (Right !aeson) ->
        -- TODO: Use `unsnoc` when available
        let actualResource = f $ Trace.remainderData aeson
        in case eitherAt of
          (Left err) -> error err
          (Right at) ->
            case maybePrevResource of
              Nothing -> (Just actualResource, Seq.singleton (at, actualResource))
              (Just prevResource) ->
                if actualResource == prevResource
                then ans
                else (Just actualResource, sq Seq.|> (at, actualResource))
      (Left _) -> ans
  reducerOf _ ans _ = ans
  showAns _ = show
  printAns _ (_, sq) = mapM_
    (\(t,h) -> putStrLn $ show t ++ ": " ++ show h)
    (toList sq)
