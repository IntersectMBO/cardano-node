module Cardano.ReCon.Presburger.Syntax.Suite (syntaxTests) where

import           Cardano.ReCon.Presburger.Formula (Formula (..), BinRel (..))
import           Cardano.ReCon.Integer.Polynomial.Term (IntTerm (..))
import           Cardano.ReCon.Presburger.Parser (formula)

import           Data.Text (Text)
import           Text.Megaparsec (parse)

import           Test.Tasty
import           Test.Tasty.HUnit

-- | Run the parser and stringify any error for comparison.
p :: Text -> Either String Formula
p src = case parse formula (show src) src of
  Right x  -> Right x
  Left err -> Left (show err)

-- | Assert that a formula parses to the expected AST.
(@==) :: Text -> Formula -> Assertion
input @== expected = p input @?= Right expected
infix 1 @==

syntaxTests :: TestTree
syntaxTests = testGroup "Syntax"
  [ atomTests
  , intTermTests
  , relConstraintTests
  , divConstraintTests
  , connTests
  , precedenceTests
  , quantifierTests
  ]

atomTests :: TestTree
atomTests = testGroup "Atoms"
  [ testCase "⊥" $ "⊥" @== Bottom
  , testCase "⊤" $ "⊤" @== Top
  ]

intTermTests :: TestTree
intTermTests = testGroup "IntTerm atoms in equality"
  [ testCase "integer constant: 0 = 0" $
      "0 = 0" @== IntBinRel Eq (IntConst 0) (IntConst 0)

  , testCase "negative constant: -3 = 0" $
      "-3 = 0" @== IntBinRel Eq (IntConst (-3)) (IntConst 0)

  , testCase "bare variable: x = 0" $
      "x = 0" @== IntBinRel Eq (IntVar 1 "x") (IntConst 0)

  , testCase "scaled variable: 3·x = 0" $
      "3·x = 0" @== IntBinRel Eq (IntVar 3 "x") (IntConst 0)

  , testCase "negative coefficient: -2·x = 0" $
      "-2·x = 0" @== IntBinRel Eq (IntVar (-2) "x") (IntConst 0)

  , testCase "sum: x + y = 0" $
      "x + y = 0" @== IntBinRel Eq (IntSum (IntVar 1 "x") (IntVar 1 "y")) (IntConst 0)

  , testCase "subtraction: x - 2 = 0" $
      "x - 2 = 0" @== IntBinRel Eq (IntSum (IntVar 1 "x") (IntConst (-2))) (IntConst 0)

  , testCase "multi-term: 3·x + y - 1 = 0" $
      "3·x + y - 1 = 0" @==
        IntBinRel Eq
          (IntSum (IntSum (IntVar 3 "x") (IntVar 1 "y")) (IntConst (-1)))
          (IntConst 0)
  ]

relConstraintTests :: TestTree
relConstraintTests = testGroup "Relation operators"
  [ testCase "= (IntEq)"  $ "x = 5"  @== IntBinRel Eq  (IntVar 1 "x") (IntConst 5)
  , testCase "< (IntLt)"  $ "x < 5"  @== IntBinRel Lt  (IntVar 1 "x") (IntConst 5)
  , testCase "≤ (IntLte)" $ "x ≤ 5"  @== IntBinRel Lte (IntVar 1 "x") (IntConst 5)
  , testCase "> (IntGt)"  $ "x > 5"  @== IntBinRel Gt  (IntVar 1 "x") (IntConst 5)
  , testCase "≥ (IntGte)" $ "x ≥ 5"  @== IntBinRel Gte (IntVar 1 "x") (IntConst 5)
  ]

divConstraintTests :: TestTree
divConstraintTests = testGroup "Divisibility"
  [ testCase "3 ∣ x" $
      "3 ∣ x" @== IntDiv 3 (IntVar 1 "x")

  , testCase "2 ∣ x + 1" $
      "2 ∣ x + 1" @== IntDiv 2 (IntSum (IntVar 1 "x") (IntConst 1))

  , testCase "5 ∣ 3·x - y" $
      -- mul (-1) (IntVar 1 "y") = IntVar (-1) "y"
      "5 ∣ 3·x - y" @==
        IntDiv 5 (IntSum (IntVar 3 "x") (IntVar (-1) "y"))
  ]

connTests :: TestTree
connTests = testGroup "Logical connectives"
  [ testCase "¬ atom" $
      "¬(x < 5)" @== Not (IntBinRel Lt (IntVar 1 "x") (IntConst 5))

  , testCase "∧" $
      "x < 5 ∧ y > 0" @==
        And (IntBinRel Lt (IntVar 1 "x") (IntConst 5))
            (IntBinRel Gt (IntVar 1 "y") (IntConst 0))

  , testCase "∨" $
      "x < 5 ∨ y > 0" @==
        Or (IntBinRel Lt (IntVar 1 "x") (IntConst 5))
           (IntBinRel Gt (IntVar 1 "y") (IntConst 0))

  , testCase "⇒" $
      "x < 5 ⇒ y > 0" @==
        Implies (IntBinRel Lt (IntVar 1 "x") (IntConst 5))
                (IntBinRel Gt (IntVar 1 "y") (IntConst 0))

  , testCase "right-assoc ∧" $
      "x = 0 ∧ y = 1 ∧ z = 2" @==
        And (IntBinRel Eq (IntVar 1 "x") (IntConst 0))
            (And (IntBinRel Eq (IntVar 1 "y") (IntConst 1))
                 (IntBinRel Eq (IntVar 1 "z") (IntConst 2)))

  , testCase "right-assoc ⇒" $
      "x = 0 ⇒ y = 1 ⇒ z = 2" @==
        Implies (IntBinRel Eq (IntVar 1 "x") (IntConst 0))
                (Implies (IntBinRel Eq (IntVar 1 "y") (IntConst 1))
                         (IntBinRel Eq (IntVar 1 "z") (IntConst 2)))
  ]

precedenceTests :: TestTree
precedenceTests = testGroup "Precedence"
  [ testCase "∧ tighter than ∨: x = 0 ∨ y = 1 ∧ z = 2" $
      -- parses as x=0 ∨ (y=1 ∧ z=2)
      "x = 0 ∨ y = 1 ∧ z = 2" @==
        Or (IntBinRel Eq (IntVar 1 "x") (IntConst 0))
           (And (IntBinRel Eq (IntVar 1 "y") (IntConst 1))
                (IntBinRel Eq (IntVar 1 "z") (IntConst 2)))

  , testCase "∨ tighter than ⇒: x = 0 ⇒ y = 1 ∨ z = 2" $
      -- parses as x=0 ⇒ (y=1 ∨ z=2)
      "x = 0 ⇒ y = 1 ∨ z = 2" @==
        Implies (IntBinRel Eq (IntVar 1 "x") (IntConst 0))
                (Or (IntBinRel Eq (IntVar 1 "y") (IntConst 1))
                    (IntBinRel Eq (IntVar 1 "z") (IntConst 2)))

  , testCase "parens override precedence: (x = 0 ∨ y = 1) ∧ z = 2" $
      "(x = 0 ∨ y = 1) ∧ z = 2" @==
        And (Or (IntBinRel Eq (IntVar 1 "x") (IntConst 0))
               (IntBinRel Eq (IntVar 1 "y") (IntConst 1)))
            (IntBinRel Eq (IntVar 1 "z") (IntConst 2))
  ]

quantifierTests :: TestTree
quantifierTests = testGroup "Quantifiers"
  [ testCase "∃x. x < 5" $
      "∃x. x < 5" @==
        IntExists "x" (IntBinRel Lt (IntVar 1 "x") (IntConst 5))

  , testCase "∀x. x ≥ 0" $
      "∀x. x ≥ 0" @==
        IntForall "x" (IntBinRel Gte (IntVar 1 "x") (IntConst 0))

  , testCase "quantifier scope covers conjunction: ∀x. x > 0 ⇒ x ≥ 1" $
      "∀x. x > 0 ⇒ x ≥ 1" @==
        IntForall "x"
          (Implies (IntBinRel Gt  (IntVar 1 "x") (IntConst 0))
                   (IntBinRel Gte (IntVar 1 "x") (IntConst 1)))

  , testCase "nested quantifiers: ∀x. ∃y. y > x" $
      "∀x. ∃y. y > x" @==
        IntForall "x"
          (IntExists "y" (IntBinRel Gt (IntVar 1 "y") (IntVar 1 "x")))

  , testCase "quantifier in parens inside ∧: x > 0 ∧ (∀y. y > x)" $
      "x > 0 ∧ (∀y. y > x)" @==
        And (IntBinRel Gt (IntVar 1 "x") (IntConst 0))
            (IntForall "y" (IntBinRel Gt (IntVar 1 "y") (IntVar 1 "x")))
  ]
