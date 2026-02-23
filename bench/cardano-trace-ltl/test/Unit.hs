{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications #-}

module Main(main) where

import           Cardano.LTL.Check               (Error (..), checkFormula)
import           Cardano.LTL.Lang.Formula
import qualified Cardano.LTL.Lang.Formula.Parser as Parser
import qualified Cardano.LTL.Lang.Formula.Prec   as Prec
import           Cardano.LTL.Pretty              (prettyFormula)
import           Cardano.LTL.Satisfy             (SatisfactionResult (..),
                                                  satisfies)
import           Data.Map                        (singleton)
import           Data.Set                        (fromList)
import           Data.Text                       (Text, unpack)
import qualified Data.Text                       as Text
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec
import Cardano.LTL.Lang.Formula.Parser (Context(..))

data Ty = Start | Success | Failure deriving (Show, Eq, Ord)

data Msg = Msg Ty Int | Placeholder deriving (Show, Eq, Ord)

instance Event Msg Ty where
  ofTy (Msg t _) t'  = t == t'
  ofTy Placeholder _ = False

  props (Msg _ i) _ = singleton "idx" (IntValue i)

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

logEmpty :: [Msg]
logEmpty = []

-- ∀i. ☐ (Start{"idx" = i} ⇒ ♢³ (Success{"idx" = i} ∨ Failure{"idx" = i}))
-- Start must be followed by either a corresponding success or failure within 3 temporal steps.
prop1 :: Formula Msg Ty
prop1 = PropForall "i" $ Forall 0 $
  Implies
    (Atom Start (fromList [PropConstraint "idx" (Var "i")]))
    (ExistsN 3 $
      Or
        (Atom Success (fromList [PropConstraint "idx" (Var "i")]))
        (Atom Failure (fromList [PropConstraint "idx" (Var "i")]))

    )

-- ∀i. ¬ (Success{"idx" = i} ∨ Failure{"idx" = i}) |˜¹⁰⁰ Start{"idx" = i}
-- Start mustn't be preceded by a corresponding success or failure.
prop2 :: Formula Msg Ty
prop2 = PropForall "i" $ UntilN
  100
  (Not $
    Or
      (Atom Success (fromList [PropConstraint "idx" (Var "i")]))
      (Atom Failure (fromList [PropConstraint "idx" (Var "i")]))
  )
  (Atom Start (fromList [PropConstraint "idx" (Var "i")]))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [syntacticTests, prop1SatisfiabilityTests, prop2SatisfiabilityTests, parserTests]

syn1 = PropForall "i" (Atom () (fromList [PropConstraint "idx" (Var "i")]))
syn2 = PropForall "i" (Atom () (fromList [PropConstraint "idx" (Var "j")]))

syntacticTests :: TestTree
syntacticTests = testGroup "Syntanctic checks"
  [
    testCase (unpack $ prettyFormula syn1 Prec.Universe <> " is syntactically valid") $
      [] @?= checkFormula mempty syn1
  ,
    testCase (unpack $ prettyFormula syn2 Prec.Universe <> " is syntactically invalid") $
      [UnboundPropVarIdentifier "j"] @?= checkFormula mempty syn2
  ]

prop1SatisfiabilityTests :: TestTree
prop1SatisfiabilityTests = testGroup ("Satisfiability of: " <> unpack (prettyFormula prop1 Prec.Universe))
  [ testCase (show log1 <> " satisfies the formula") $
      satisfies prop1 log1 @?= Satisfied
  , testCase (show log2 <> " satisfies the formula") $
      satisfies prop1 log2 @?= Satisfied
  , testCase (show log3 <> " satisfies the formula") $
      satisfies prop1 log3 @?= Satisfied
  , testCase (show log4 <> " does not satisfy the formula") $
      satisfies prop1 log4 @?= Unsatisfied
        (fromList [(Msg Start 2, Start)])
  , testCase (show log5 <> " satisfies the formula") $
      satisfies prop1 log5 @?= Satisfied
  , testCase (show log6 <> " satisfies the formula") $
      satisfies prop1 log6 @?= Unsatisfied
        (fromList [(Msg Start 4,Start),(Msg Success 7,Success)])
  , testCase (show log7 <> " does not satisfy the formula") $
      satisfies prop1 log7 @?= Unsatisfied
        (fromList [(Msg Start 2, Start)])
  ]

prop2SatisfiabilityTests :: TestTree
prop2SatisfiabilityTests = testGroup ("Satisfiability of: " <> unpack (prettyFormula prop2 Prec.Universe))
  [
    testCase (show log1 <> " satisfies the formula") $
      satisfies prop2 log1 @?= Satisfied
  , testCase (show log5 <> " satisfies the formula") $
      satisfies prop2 log5 @?= Satisfied
  , testCase (show logEmpty <> " satisfies the formula") $
      satisfies prop2 logEmpty @?= Satisfied
  , testCase (show log8 <> "does not satisfy the formula") $
      satisfies prop2 log8 @?= Unsatisfied
        (fromList [(Msg Start 1,Start),(Msg Success 1,Success)])
  , testCase (show log9 <> " does not satisfy the formula") $
      satisfies prop2 log9 @?= Unsatisfied
        (fromList [(Msg Success 1,Success)])
  , testCase (show log10 <> " satisfies the formula") $
      satisfies prop2 log10 @?= Satisfied
  , testCase (show log11 <> " does not satisfy the formula") $
      satisfies prop2 log11 @?=
      Unsatisfied
        (fromList [(Msg Start 1,Start),(Msg Success 1,Success),(Msg Success 2,Success)])

  ]

formula0 :: Text
formula0 = "☐ ᪲ (∀x. \"Forge.Loop.StartLeadershipCheck\"{\"slot\" = x} ⇒ \
  \♢³⁰⁰⁰ (\"Forge.Loop.NodeIsLeader\"{\"slot\" = x} ∨ \"Forge.Loop.NodeNotLeader\"{\"slot\" = x}))"

formula1 :: Text
formula1 =
  "☐ ᪲₄₂ (∀i. (¬ (\"NodeIsLeader\"{\"slot\" = i} ∨ \"NodeNotLeader\"{\"slot\" = i}) |¹²³ \"StartLeadershipCheck\"{\"slot\" = i}))"

formula2 :: Text
formula2 =
  "☐² (⊥ ∨ ◯ ⊤ ∧ ◯² (◯¹ (◯⁰ ⊤)))"

formula3 :: Text
formula3 = "☐ ᪲₁ ⊤"

formula4 :: Text
formula4 = "∀(x ∈ {1, 2, 3}). x = 1 ∨ x = 2 ∨ x = 3"

formula5 :: Text
formula5 = "∀x ∈ {1, 2, 3}. x = 1 ∨ x = 2 ∨ x = 3"

emptyCtx :: Context
emptyCtx = Context []

parserTests :: TestTree
parserTests = testGroup "Parsing"
  [
    testCase (Text.unpack formula0) $
      parse (Parser.formula @Text @() emptyCtx Parser.text) "input" formula0 @?=
        Right
          (Forall
            0
            (PropForall
              "x"
              (Implies
                (Atom "Forge.Loop.StartLeadershipCheck" (fromList [PropConstraint "slot" (Var "x")]))
                (ExistsN
                  3000
                  (Or
                    (Atom "Forge.Loop.NodeIsLeader" (fromList [PropConstraint "slot" (Var "x")]))
                    (Atom "Forge.Loop.NodeNotLeader" (fromList [PropConstraint "slot" (Var "x")])))))))
  ,
    testCase (Text.unpack formula1) $
      parse (Parser.formula @Text @() emptyCtx Parser.text) "input" formula1 @?=
        Right
          (Forall
            42
            (PropForall
              "i"
              (UntilN
                123
                (Not
                  (Or
                    (Atom "NodeIsLeader" (fromList [PropConstraint "slot" (Var "i")]))
                    (Atom "NodeNotLeader" (fromList [PropConstraint "slot" (Var "i")]))))
                (Atom "StartLeadershipCheck" (fromList [PropConstraint "slot" (Var "i")])))))
  ,
    testCase (Text.unpack formula2) $
      parse (Parser.formula @Text @() emptyCtx Parser.text) "input" formula2 @?=
        Right (ForallN 2 (Or Bottom (And (Next Top) (NextN 2 (NextN 1 (NextN 0 Top))))))
  ,
    testCase (Text.unpack formula3) $
      parse (Parser.formula @Text @() emptyCtx Parser.text) "input" formula3 @?=
        Right (Forall 1 Top)
  ,
    testCase (Text.unpack formula4) $
      parse (Parser.formula @Text @() emptyCtx Parser.text) "input" formula4 @?=
        Right
          (PropForallN
            "x"
            (fromList [IntValue 1,IntValue 2,IntValue 3])
            (Or
              (PropEq (fromList []) (Var "x") (IntValue 1))
              (Or
                (PropEq (fromList []) (Var "x") (IntValue 2))
                (PropEq (fromList []) (Var "x") (IntValue 3)))))
  ,
    testCase (Text.unpack formula5) $
      parse (Parser.formula @Text @() emptyCtx Parser.text) "input" formula5 @?=
        Right
          (PropForallN
            "x"
            (fromList [IntValue 1,IntValue 2,IntValue 3])
            (Or
              (PropEq (fromList []) (Var "x") (IntValue 1))
              (Or
                (PropEq (fromList []) (Var "x") (IntValue 2))
                (PropEq (fromList []) (Var "x") (IntValue 3)))))
  ]
