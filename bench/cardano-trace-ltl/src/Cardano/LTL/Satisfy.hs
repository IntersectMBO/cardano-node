{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Strict              #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Cardano.LTL.Satisfy(
    SatisfactionResult(..)
  , satisfies
  , satisfiesS
  , SatisfyMetrics(..)
  ) where

import           Cardano.LTL.Lang.Formula
import           Cardano.LTL.Rewrite

import           Prelude                             hiding (lookup)

import           Cardano.LTL.Lang.HomogeneousFormula (eval)
import           Cardano.LTL.Progress
import           Data.IORef                          (IORef, modifyIORef')
import           Data.List                           (foldl')
import           Data.Word                           (Word64)
import           Streaming
#ifdef TRACE
import qualified Cardano.LTL.Lang.Formula.Prec       as Prec
import           Cardano.LTL.Pretty                  (prettyFormula)
import qualified Data.Text                           as Text
import           Debug.Trace                         (trace)
#endif


-- This file concerns checking formula satisfiability against some input.
-- The input can be either pure and finite foldable (e.g. a list, cf. `satisfies`)
--   or effectful and potentially infinite (i.e. a Stream, cf. `satisfiesS`).

-- | The result of checking satisfaction of a formula against a timeline.
-- | If unsatisfied, stores points in the timeline "relevant" to the formula.
data SatisfactionResult event ty = Satisfied | Unsatisfied (Relevance event ty) deriving (Show, Eq)

traceFormula :: Show ty => String -> Formula event ty -> Formula event ty
traceFormula ~str x =
#ifdef TRACE
  trace (str <> " " <> Text.unpack (prettyFormula x Prec.Universe)) x
#else
  x
#endif

handleNext :: (Event event ty, Ord event, Ord ty, Show ty)
           => (Int, Formula event ty)
           -> event
           -> Either (SatisfactionResult event ty) (Int, Formula event ty)
handleNext (!n, !formula0) m =
  let formula1 = traceFormula ("(" <> show (1 + n) <> ")\ninitial:") formula0 in
  let formula2 = traceFormula "next:" $ next formula1 m in
  let formula3 = traceFormula "rewrite-hom:" (rewriteHomogeneous formula2) in
  let formula4 = traceFormula "rewrite-frag:" $ rewriteFragment formula3 in
  let formula5 = traceFormula "rewrite-id:" (rewriteIdentity formula4) in
  case formula5 of
    Top     -> Left Satisfied
    Bottom  -> Left (Unsatisfied (relevance formula0))
    formula -> Right (n + 1, formula5)

handleEnd :: (Ord event, Ord ty, Show ty) => (Int, Formula event ty) -> SatisfactionResult event ty
handleEnd (!n, !formula) =
    if (eval . terminate) formula
    then Satisfied
    else Unsatisfied (relevance formula)

merge :: Either a a -> a
merge = either id id

-- | Check if the formula is satisfied in the given event timeline.
satisfies :: (Event event ty, Ord event, Ord ty, Show ty, Foldable f)
          => Formula event ty
          -> f event
          -> SatisfactionResult event ty
satisfies formula xs = merge $ handleEnd <$> foldl' (\acc e -> acc >>= flip handleNext e) (Right (0, formula)) xs


data SatisfyMetrics event ty = SatisfyMetrics {
  eventsConsumed   :: Word64,
  currentFormula   :: Formula event ty,
  -- | μs
  currentTimestamp :: Word64
}

-- | Given a formula and a stream of events, forms an `IO` computation that returns a `SatisfactionResult` once
--    the formula is equivalent to ⊤ or ⊥. This may happen either once the stream terminates or if
--    the formula is falsified early by some prefix of the stream.
satisfiesS :: (Event event ty, Ord event, Ord ty, Show ty)
           => Formula event ty
           -> Stream (Of event) IO ()
           -> IORef (SatisfyMetrics event ty)
           -> IO (SatisfactionResult event ty)
satisfiesS formula input metrics = run $ mapped (pure. pure . runIdentity) $ unfold (go metrics) (0, formula,  input) where
  go :: (Event event ty, Ord event, Ord ty, Show ty)
     => IORef (SatisfyMetrics event ty)
     -> (Int, Formula event ty, Stream (Of event) IO ())
     -> IO (Either (SatisfactionResult event ty) (Identity (Int, Formula event ty, Stream (Of event) IO ())))
  go metrics (!n, !formula, !input) = inspect input >>= \case
    Left () -> pure $ Left $
      handleEnd (n, formula)
    Right (event :> more) -> do
      modifyIORef' metrics (\x -> SatisfyMetrics (1 + x.eventsConsumed) formula (beg event))
      pure $
        fmap
          (\(!n', !formula') -> Identity (n', formula', more))
          (handleNext (n, formula) event)
