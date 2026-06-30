module Main (main) where

import qualified Cardano.Timeseries.Elab.Expr.Parser.Suite as ParserSuite
import qualified Cardano.Timeseries.Elab.Suite             as ElabSuite
import qualified Cardano.Timeseries.Interp.Suite           as InterpSuite

import           GHC.IO.Encoding                           (setLocaleEncoding, utf8)
import           Test.Tasty

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "Unit tests"
    [ testGroup "Parser" [ParserSuite.parserTests]
    , testGroup "Elab"   [ElabSuite.elabTests]
    , testGroup "Interp" [InterpSuite.interpTests]
    ]
