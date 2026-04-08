{- HLINT ignore "Redundant map" -}
module Cardano.ReCon.Integration.Suite (integrationTests) where

import           Cardano.ReCon.LTL.Formula (Formula, interpTimeunit)
import           Cardano.ReCon.LTL.Formula.Parser (Context (..))
import qualified Cardano.ReCon.LTL.Formula.Parser as Parser
import           Cardano.ReCon.LTL.Formula.Yaml (readFormulas, readPropValues)
import           Cardano.ReCon.LTL.Satisfy (SatisfactionResult (..), satisfies)
import           Cardano.ReCon.Trace.Event ()
import qualified Cardano.ReCon.Trace.Feed as Feed
import           Cardano.ReCon.Trace.Feed (TemporalEvent)

import           Control.Monad (forM_)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Exit (die)
import           System.FilePath ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit

formulasFile :: FilePath
formulasFile = "examples/cfgs/formulas.yaml"

contextFile :: FilePath
contextFile = "examples/cfgs/context.yaml"

tracesDir :: FilePath
tracesDir = "examples/extracts"

eventDuration :: Word
eventDuration = 10          -- μs per event bucket (= --duration 10)

second :: Word
second = 1_000_000          -- μs                 (= --timeunit second)

integrationTests :: IO TestTree
integrationTests = do
  ctx      <- readPropValues contextFile >>= orDie
  formulas <- readFormulas formulasFile
                (Context { interpDomain = Map.toList ctx, varKinds = Map.empty })
                Parser.name
                >>= orDie
  let fs = map (interpTimeunit (\u -> u * second `div` eventDuration)) formulas
  pure $ testGroup "Trace integration"
    [ testGroup "Positive — every formula must pass"
        [ mkPositive fs "ok-1.txt"
        , mkPositive fs "ok-2.txt"
        , mkPositive fs "ok-3.txt"
        , mkPositive fs "ok-4.txt"
        , mkPositive fs "ok-5.txt"
        ]
    , testGroup "Negative — at least one formula must fail"
        [ mkNegative fs "fail-1.txt"
        , mkNegative fs "fail-2.txt"
        , mkNegative fs "fail-3.txt"
        , mkNegative fs "fail-4.txt"
        , mkNegative fs "fail-5.txt"
        , mkNegative fs "fail-6.txt"
        ]
    ]

mkPositive :: [Formula TemporalEvent Text] -> String -> TestTree
mkPositive fs name = testCase name $ do
  events <- Feed.read (tracesDir </> name) eventDuration
  forM_ fs $ \phi ->
    satisfies phi events @?= Satisfied

mkNegative :: [Formula TemporalEvent Text] -> String -> TestTree
mkNegative fs name = testCase name $ do
  events <- Feed.read (tracesDir </> name) eventDuration
  assertBool "expected at least one Unsatisfied" $
    any isUnsatisfied (map (`satisfies` events) fs)

isUnsatisfied :: SatisfactionResult event ty -> Bool
isUnsatisfied (Unsatisfied _) = True
isUnsatisfied _               = False

orDie :: Either Text a -> IO a
orDie (Right x)  = pure x
orDie (Left err) = die (Text.unpack err)
