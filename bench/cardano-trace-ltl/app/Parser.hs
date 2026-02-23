{-# LANGUAGE LambdaCase #-}
module Main(main) where


import           Prelude                         hiding (read)

import qualified Cardano.LTL.Lang.Formula.Parser as Parser
import           Cardano.LTL.Lang.Formula.Yaml
import           Cardano.LTL.Pretty              (prettyFormula)
import qualified Cardano.LTL.Pretty              as Prec
import           Data.Foldable                   (traverse_)
import qualified Data.Text.IO                    as Text
import           System.Environment              (getArgs)


-- | Given a filename pointing to a .yaml document,
--   read and parse it. The expected value of the yaml is an array of strings.
--   Every string must be a valid formula.
main :: IO ()
main = do
  [!filename] <- getArgs
  readFormulas filename (Parser.Context []) Parser.text >>= \case
    Left err -> Text.putStrLn err
    Right formulas -> do
      traverse_ (\f -> Text.putStrLn (prettyFormula f Prec.Universe)) formulas
