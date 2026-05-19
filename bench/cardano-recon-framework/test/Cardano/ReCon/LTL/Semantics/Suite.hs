module Cardano.ReCon.LTL.Semantics.Suite (semanticsTests) where

import           Cardano.ReCon.LTL.Formula
import qualified Cardano.ReCon.LTL.Formula.Prec as Prec
import           Cardano.ReCon.LTL.Formula.Pretty (prettyFormula)
import           Cardano.ReCon.LTL.Satisfy (SatisfactionResult (..), satisfies)

import           Data.Map (singleton)
import           Data.Set (fromList)
import           Data.Text (unpack)

import           Test.Tasty
import           Test.Tasty.HUnit

data Ty = Start | Success | Failure deriving (Show, Eq, Ord)

data Msg = Msg Ty Int | Placeholder deriving (Show, Eq, Ord)

instance Event Msg Ty where
  ofTy (Msg t _) t'  = t == t'
  ofTy Placeholder _ = False

  intProps (Msg _ i) _ = singleton "idx" (fromIntegral i)
  intProps Placeholder _ = mempty

  textProps _ _ = mempty

  beg _ = 0

-- Generic two-tag event type for arithmetic-focused tests.
data Tag = P | Q deriving (Show, Eq, Ord)

data Evt = Evt Tag Int | Noop deriving (Show, Eq, Ord)

instance Event Evt Tag where
  ofTy (Evt t _) t' = t == t'
  ofTy Noop       _ = False

  intProps (Evt _ i) _ = singleton "idx" (fromIntegral i)
  intProps Noop      _ = mempty

  textProps _ _ = mempty

  beg _ = 0

log1 :: [Msg]
log1 = [Msg Start 2, Placeholder, Msg Success 2]

log2 :: [Msg]
log2 = [Msg Start 1, Placeholder, Msg Failure 1, Placeholder]

log3 :: [Msg]
log3 = [Placeholder]

log4 :: [Msg]
log4 = [Msg Start 2, Placeholder]

log5 :: [Msg]
log5 = [ Msg Start 1
       , Placeholder
       , Msg Failure 1
       , Placeholder
       , Msg Start 4
       , Placeholder
       , Msg Success 4
       , Placeholder
       ]

log6 :: [Msg]
log6 = [ Msg Start 1
       , Placeholder
       , Msg Failure 1
       , Placeholder
       , Msg Start 4
       , Placeholder
       , Msg Success 7
       , Placeholder
       ]

log7 :: [Msg]
log7 = [Msg Start 2, Placeholder, Placeholder, Msg Failure 2]

log8 :: [Msg]
log8 =
  [
    Placeholder
  , Msg Success 1
  , Placeholder
  , Msg Start 1
  , Placeholder
  , Placeholder
  ]

log9 :: [Msg]
log9 =
  [
    Placeholder
  , Msg Success 1
  , Placeholder
  , Placeholder
  ]

log10 :: [Msg]
log10 =
  [
    Msg Start 1
  , Msg Success 1
  , Msg Start 2
  , Msg Success 2
  ]

log11 :: [Msg]
log11 =
  [
    Msg Start 1
  , Msg Success 1
  , Msg Start 1
  , Msg Success 2
  ]

log12 :: [Msg]
log12 =
  [
    Msg Start 1
  , Placeholder
  , Msg Success 1
  , Msg Start 2
  , Placeholder
  ]

log13 :: [Msg]
log13 =
  [
    Msg Start 1
  , Placeholder
  , Msg Success 0
  , Msg Start 2
  , Msg Success 2
  , Placeholder
  ]

logEmpty :: [Msg]
logEmpty = []

log14 :: [Evt]
log14 = [Evt P 1, Evt Q 2]

log15 :: [Evt]
log15 = [Evt P 1, Evt Q 1]

log16 :: [Evt]
log16 = [Evt P 3, Noop, Evt Q 4]

log17 :: [Evt]
log17 = [Evt P 2, Evt Q 4]

log18 :: [Evt]
log18 = [Evt P 2, Evt Q 2]

-- ∀i. ☐ (Start{"idx" = i} ⇒ ♢³ (Success{"idx" = i} ∨ Failure{"idx" = i}))
-- Start must be followed by either a corresponding success or failure within 3 temporal steps.
prop1 :: Formula Msg Ty
prop1 = PropIntForall "i" $ Forall 0 $
  Implies
    (Atom Start (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))
    (ExistsN 3 $
      Or
        (Atom Success (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))
        (Atom Failure (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))

    )

-- ∀i. ¬ (Success{"idx" = i} ∨ Failure{"idx" = i}) |˜¹⁰⁰ Start{"idx" = i}
-- Start mustn't be preceded by a corresponding success or failure.
prop2 :: Formula Msg Ty
prop2 = PropIntForall "i" $ UntilN
  100
  (Not $
    Or
      (Atom Success (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))
      (Atom Failure (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))
  )
  (Atom Start (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))

-- ☐ (∃i. i = 1 ∧ (Start("idx" = i) ⇒ ♢³ Success("idx" = i)))
prop3 :: Formula Msg Ty
prop3 = Forall 0 $ PropIntExists "i" $ And
  (PropIntBinRel Eq mempty (IntVar 1 "i") (IntConst 1))
  (
  Implies
    (Atom Start (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))
    (
      ExistsN 3
        (Atom Success (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))
    )
  )

-- ∀i. ☐ (P{"idx" = i} ⇒ ♢³ Q{"idx" = i + 1})
-- Tests IntSum in a PropConstraint.
prop4 :: Formula Evt Tag
prop4 = PropIntForall "i" $ Forall 0 $
  Implies
    (Atom P (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))
    (ExistsN 3
      (Atom Q (fromList [IntPropConstraint "idx" (IntSum (IntVar 1 "i") (IntConst 1))])))

-- ∀i. ☐ (P{"idx" = i} ⇒ ♢³ Q{"idx" = 2·i})
-- Tests non-unit coefficient in a PropConstraint.
prop5 :: Formula Evt Tag
prop5 = PropIntForall "i" $ Forall 0 $
  Implies
    (Atom P (fromList [IntPropConstraint "idx" (IntVar 1 "i")]))
    (ExistsN 3
      (Atom Q (fromList [IntPropConstraint "idx" (IntVar 2 "i")])))

-- ∀i. 2·i = i + i
-- Pure Presburger tautology: tests IntSum and non-unit coefficient in PropIntBinRel.
prop6 :: Formula Evt Tag
prop6 = PropIntForall "i"
  (PropIntBinRel Eq mempty (IntVar 2 "i") (IntSum (IntVar 1 "i") (IntVar 1 "i")))

semanticsTests :: TestTree
semanticsTests = testGroup "Semantics"
  [ prop1SatisfiabilityTests
  , prop2SatisfiabilityTests
  , prop3SatisfiabilityTests
  , prop4SatisfiabilityTests
  , prop5SatisfiabilityTests
  , prop6SatisfiabilityTests
  ]

prop1SatisfiabilityTests :: TestTree
prop1SatisfiabilityTests = testGroup ("Satisfiability of: " <> unpack (prettyFormula prop1 Prec.Universe))
  [ testCase (show log1 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop1 log1 @?= Satisfied
  , testCase (show log2 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop1 log2 @?= Satisfied
  , testCase (show log3 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop1 log3 @?= Satisfied
  , testCase (show log4 <> " does not satisfy the formula") $
      satisfies CrashOnMissingKey prop1 log4 @?= Unsatisfied
        (fromList [(Msg Start 2, Start)])
  , testCase (show log5 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop1 log5 @?= Satisfied
  , testCase (show log6 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop1 log6 @?= Unsatisfied
        (fromList [(Msg Start 1,Start),(Msg Start 4,Start),(Msg Success 7,Success),(Msg Failure 1,Failure)])
  , testCase (show log7 <> " does not satisfy the formula") $
      satisfies CrashOnMissingKey prop1 log7 @?= Unsatisfied
        (fromList [(Msg Start 2, Start)])
  ]

prop2SatisfiabilityTests :: TestTree
prop2SatisfiabilityTests = testGroup ("Satisfiability of: " <> unpack (prettyFormula prop2 Prec.Universe))
  [
    testCase (show log1 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop2 log1 @?= Satisfied
  , testCase (show log5 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop2 log5 @?= Satisfied
  , testCase (show logEmpty <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop2 logEmpty @?= Satisfied
  , testCase (show log8 <> "does not satisfy the formula") $
      satisfies CrashOnMissingKey prop2 log8 @?= Unsatisfied
        (fromList [(Msg Start 1,Start),(Msg Success 1,Success)])
  , testCase (show log9 <> " does not satisfy the formula") $
      satisfies CrashOnMissingKey prop2 log9 @?= Unsatisfied
        (fromList [(Msg Success 1,Success)])
  , testCase (show log10 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop2 log10 @?= Satisfied
  , testCase (show log11 <> " does not satisfy the formula") $
      satisfies CrashOnMissingKey prop2 log11 @?=
      Unsatisfied
        (fromList [(Msg Start 1,Start),(Msg Success 1,Success),(Msg Success 2,Success)])

  ]

prop3SatisfiabilityTests :: TestTree
prop3SatisfiabilityTests = testGroup ("Satisfiability of: " <> unpack (prettyFormula prop3 Prec.Universe))
  [
    testCase (show log12 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop3 log12 @?= Satisfied
  , testCase (show log13 <> " does not satisfy the formula") $
      satisfies CrashOnMissingKey prop3 log13 @?= Unsatisfied (fromList [(Msg Start 1,Start),(Msg Success 0,Success)])
  ]

prop4SatisfiabilityTests :: TestTree
prop4SatisfiabilityTests = testGroup ("Satisfiability of: " <> unpack (prettyFormula prop4 Prec.Universe))
  [ testCase "[] satisfies the formula" $
      satisfies CrashOnMissingKey prop4 ([] :: [Evt]) @?= Satisfied
  , testCase (show log14 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop4 log14 @?= Satisfied
  , testCase (show log16 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop4 log16 @?= Satisfied
  , testCase (show log15 <> " does not satisfy the formula") $
      satisfies CrashOnMissingKey prop4 log15 @?=
        Unsatisfied (fromList [(Evt P 1, P), (Evt Q 1, Q)])
  ]

prop5SatisfiabilityTests :: TestTree
prop5SatisfiabilityTests = testGroup ("Satisfiability of: " <> unpack (prettyFormula prop5 Prec.Universe))
  [ testCase "[] satisfies the formula" $
      satisfies CrashOnMissingKey prop5 ([] :: [Evt]) @?= Satisfied
  , testCase (show log17 <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop5 log17 @?= Satisfied
  , testCase (show log18 <> " does not satisfy the formula") $
      satisfies CrashOnMissingKey prop5 log18 @?=
        Unsatisfied (fromList [(Evt P 2, P), (Evt Q 2, Q)])
  ]

prop6SatisfiabilityTests :: TestTree
prop6SatisfiabilityTests = testGroup ("Satisfiability of: " <> unpack (prettyFormula prop6 Prec.Universe))
  [ testCase "[] satisfies the formula" $
      satisfies CrashOnMissingKey prop6 ([] :: [Evt]) @?= Satisfied
  , testCase (show [Noop] <> " satisfies the formula") $
      satisfies CrashOnMissingKey prop6 [Noop] @?= Satisfied
  ]
