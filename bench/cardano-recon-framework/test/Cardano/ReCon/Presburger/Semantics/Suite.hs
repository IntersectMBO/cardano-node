module Cardano.ReCon.Presburger.Semantics.Suite (semanticsTests) where

import           Cardano.ReCon.Presburger.Decide (eval)
import           Cardano.ReCon.Presburger.Parser (formula)

import           Data.Text (Text)
import           Text.Megaparsec (parse)

import           Test.Tasty
import           Test.Tasty.HUnit

-- | Parse a closed formula and evaluate it with the decision procedure.
-- Aborts the test with a parse error if the text doesn't parse.
check :: Text -> Bool -> Assertion
check src expected = case parse formula "" src of
  Left err  -> assertFailure (show err)
  Right phi -> eval phi @?= expected

-- | Assert a formula is valid (evaluates to True).
(|=) :: Text -> Bool -> TestTree
src |= b = testCase (show src) $ check src b
infix 0 |=

semanticsTests :: TestTree
semanticsTests = testGroup "Semantics"
  [ groundTests
  , existentialTests
  , universalTests
  , mixedQuantifierTests
  , divisibilityTests
  ]

-- ---------------------------------------------------------------------------
-- Ground formulas (no quantifiers)
-- ---------------------------------------------------------------------------

groundTests :: TestTree
groundTests = testGroup "Ground"
  [ "⊤"           |= True
  , "⊥"           |= False
  , "3 < 5"       |= True
  , "5 < 3"       |= False
  , "3 = 3"       |= True
  , "4 = 3"       |= False
  , "3 ≤ 3"       |= True
  , "3 ≥ 4"       |= False
  , "2 ∣ 6"       |= True
  , "2 ∣ 7"       |= False
  , "¬(3 < 5)"    |= False
  , "¬⊥"          |= True
  , "3 < 5 ∧ 1 < 2" |= True
  , "3 < 5 ∧ 5 < 3" |= False
  , "3 < 5 ∨ 5 < 3" |= True
  , "5 < 3 ∨ 6 < 4" |= False
  , "3 < 5 ⇒ 1 < 2" |= True
  , "3 < 5 ⇒ 5 < 3" |= False
  , "5 < 3 ⇒ 1 = 2" |= True   -- false premise makes implication true
  ]

-- ---------------------------------------------------------------------------
-- Existential quantification
-- ---------------------------------------------------------------------------

existentialTests :: TestTree
existentialTests = testGroup "Existential"
  [ -- trivial witness
    "∃x. x = 0"             |= True
    -- x < x is never satisfied
  , "∃x. x < x"             |= False
    -- x = 1 or x = 2 are witnesses
  , "∃x. x > 0 ∧ x < 3"    |= True
    -- no integer satisfies x > 5 ∧ x < 5
  , "∃x. x > 5 ∧ x < 5"    |= False
    -- 2x = 3 has no integer solution
  , "∃x. 2·x = 3"           |= False
    -- 2x = 4 has solution x = 2
  , "∃x. 2·x = 4"           |= True
    -- x = -7 is a witness
  , "∃x. x < 0"             |= True
    -- x = 10 is a witness
  , "∃x. x > 9"             |= True
  ]

-- ---------------------------------------------------------------------------
-- Universal quantification
-- ---------------------------------------------------------------------------

universalTests :: TestTree
universalTests = testGroup "Universal"
  [ -- reflexivity of equality
    "∀x. x = x"             |= True
    -- successor: x < x+1 for all integers
  , "∀x. x < x + 1"         |= True
    -- x = 0 is a counterexample (or x < 0)
  , "∀x. x > 0"             |= False
    -- in ℤ: x > 0 implies x ≥ 1 (no integer strictly between 0 and 1)
  , "∀x. x > 0 ⇒ x ≥ 1"   |= True
    -- there exist negative integers
  , "∀x. x ≥ 0"             |= False
    -- x ≤ x always
  , "∀x. x ≤ x"             |= True
    -- ¬(x < x) always
  , "∀x. ¬(x < x)"          |= True
  ]

-- ---------------------------------------------------------------------------
-- Mixed / alternating quantifiers
-- ---------------------------------------------------------------------------

mixedQuantifierTests :: TestTree
mixedQuantifierTests = testGroup "Mixed quantifiers"
  [ -- for any x, y = x+1 is a witness for y > x
    "∀x. ∃y. y > x"                           |= True
    -- no single y is greater than every integer
  , "∃y. ∀x. y > x"                           |= False
    -- every integer is even or odd
  , "∀x. ∃y. x = 2·y ∨ x = 2·y + 1"          |= True
    -- in ℤ: x < y implies x+1 ≤ y (no integer strictly between x and x+1)
  , "∀x. ∀y. x < y ⇒ x + 1 ≤ y"             |= True
    -- there is no largest integer
  , "∀x. ∃y. y > x ∧ y < x + 3"              |= True
  ]

-- ---------------------------------------------------------------------------
-- Divisibility
-- ---------------------------------------------------------------------------

divisibilityTests :: TestTree
divisibilityTests = testGroup "Divisibility"
  [ -- 6 is divisible by both 2 and 3, and lies in (0,7)
    "∃x. 2 ∣ x ∧ 3 ∣ x ∧ x > 0 ∧ x < 7"    |= True
    -- every integer is even or has an even successor
  , "∀x. 2 ∣ x ∨ 2 ∣ x + 1"                  |= True
    -- no multiple of 7 lies strictly in (0,7)
  , "∃x. 7 ∣ x ∧ x > 0 ∧ x < 7"             |= False
    -- if x is odd then x+1 is even
  , "∀x. ¬(2 ∣ x) ⇒ 2 ∣ x + 1"              |= True
    -- Chinese remainder: ∃x. 2∣x ∧ 3∣(x+1)  (x=0: 2∣0 ∧ 3∣1? No. x=3: 2∣3? No. x=6: 3∣7? No. x=-3: 2∣(-3)? No. Hmm, x=3 fails, let me think...)
    -- Actually: 2|x and 3|(x+1). x must be even. x+1 must be divisible by 3.
    -- x=2: x+1=3, 3|3 ✓  → True
  , "∃x. 2 ∣ x ∧ 3 ∣ x + 1"                  |= True
    -- every integer leaves remainder 0 or 1 when divided by 2
  , "∀x. 2 ∣ x ∨ 2 ∣ x - 1"                  |= True
  ]
