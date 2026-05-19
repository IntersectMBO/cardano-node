module Cardano.ReCon.Integer.Polynomial.Semantics.Suite (semanticsTests) where

import           Cardano.ReCon.Integer.Polynomial.Term (IntTerm (..))
import           Cardano.ReCon.Integer.Polynomial.Value (normalise)

import           Test.Tasty
import           Test.Tasty.HUnit

semanticsTests :: TestTree
semanticsTests = testGroup "Semantics"
  [ constantTests
  , variableTests
  , combinationTests
  ]

-- | Shorthand: assert that normalising the left side gives the right side.
(@~>) :: IntTerm -> IntTerm -> Assertion
src @~> expected = normalise src @?= expected
infix 1 @~>

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

constantTests :: TestTree
constantTests = testGroup "Constants"
  [ testCase "IntConst 0" $
      IntConst 0 @~> IntConst 0

  , testCase "IntConst 5" $
      IntConst 5 @~> IntConst 5

  , testCase "constant folding: 3 + 4 = 7" $
      IntSum (IntConst 3) (IntConst 4) @~> IntConst 7
  ]

-- ---------------------------------------------------------------------------
-- Variables
-- ---------------------------------------------------------------------------

variableTests :: TestTree
variableTests = testGroup "Variables"
  [ testCase "bare variable x" $
      IntVar 1 "x"
        @~> IntSum (IntConst 0) (IntVar 1 "x")

  , testCase "scaled variable 3·x" $
      IntVar 3 "x"
        @~> IntSum (IntConst 0) (IntVar 3 "x")

  , testCase "constant + variable: 2 + 3·x" $
      IntSum (IntConst 2) (IntVar 3 "x")
        @~> IntSum (IntConst 2) (IntVar 3 "x")
  ]

-- ---------------------------------------------------------------------------
-- Combining / normalising
-- ---------------------------------------------------------------------------

combinationTests :: TestTree
combinationTests = testGroup "Combination"
  [ testCase "like terms combined: 2·x + 3·x = 5·x" $
      IntSum (IntVar 2 "x") (IntVar 3 "x")
        @~> IntSum (IntConst 0) (IntVar 5 "x")

  , testCase "cancellation: x + (-x) = 0" $
      IntSum (IntVar 1 "x") (IntVar (-1) "x")
        @~> IntConst 0

  , testCase "alphabetical ordering: y + x sorted as x, y" $
      IntSum (IntVar 1 "y") (IntVar 1 "x")
        @~> IntSum (IntConst 0)
              (IntSum (IntVar 1 "x") (IntVar 1 "y"))
  ]
