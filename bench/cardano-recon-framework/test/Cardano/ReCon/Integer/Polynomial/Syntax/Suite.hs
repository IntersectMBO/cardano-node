module Cardano.ReCon.Integer.Polynomial.Syntax.Suite (syntaxTests) where

import           Cardano.ReCon.Integer.Polynomial.Parser (intTerm)
import           Cardano.ReCon.Integer.Polynomial.Term (IntTerm (..))

import           Data.Text (Text)
import           Text.Megaparsec (parse)

import           Test.Tasty
import           Test.Tasty.HUnit

-- | Run the parser and stringify any error for comparison.
p :: Text -> Either String IntTerm
p src = case parse intTerm (show src) src of
  Right x  -> Right x
  Left err -> Left (show err)

-- | Assert that a term parses to the expected AST.
(@==) :: Text -> IntTerm -> Assertion
input @== expected = p input @?= Right expected
infix 1 @==

syntaxTests :: TestTree
syntaxTests = testGroup "Syntax"
  [ constantTests
  , variableTests
  , sumTests
  ]

constantTests :: TestTree
constantTests = testGroup "Constants"
  [ testCase "zero"             $ "0"   @== IntConst 0
  , testCase "positive"         $ "42"  @== IntConst 42
  , testCase "negative"         $ "-3"  @== IntConst (-3)
  ]

variableTests :: TestTree
variableTests = testGroup "Variables"
  [ testCase "bare variable"          $ "x"    @== IntVar 1 "x"
  , testCase "scaled variable"        $ "3·x"  @== IntVar 3 "x"
  , testCase "negative coefficient"   $ "-2·x" @== IntVar (-2) "x"
  ]

sumTests :: TestTree
sumTests = testGroup "Sums"
  [ testCase "x + y" $
      "x + y" @== IntSum (IntVar 1 "x") (IntVar 1 "y")

  , testCase "x - 2" $
      "x - 2" @== IntSum (IntVar 1 "x") (IntConst (-2))

  , testCase "3·x + y - 1" $
      "3·x + y - 1" @==
        IntSum (IntSum (IntVar 3 "x") (IntVar 1 "y")) (IntConst (-1))
  ]
