module Cardano.Logging.DocuGenerator.Tree (Tree (..), foldTree, printTree, printList, toForest) where

import           Data.Function (on)
import           Data.List (groupBy, intersperse)
import           Data.Text.Internal (Text)
import           Data.Text.Internal.Builder (Builder)
import           Data.Tree (Forest, Tree (..), foldTree, unfoldForest)

-- T ::= ∙ x
--     |
--       ∙ x
--          T
--          T
--         ...
--          T
--
-- Example:
--
--  ∙ BlockFetch
--    ∙ Client
--      ∙ AcknowledgedFetchRequest
--      ∙ AddedFetchRequest
--      ∙ ClientMetrics
--    ∙ Decision
--    ∙ Remote
printTree :: Tree Text -> Text
printTree =
  foldTree (\x -> mconcat . intersperse "\n" . ("∙ " <> x :) . map ("\t" <>))

printList :: (a -> Builder) -> [a] -> Builder
printList fmt = mconcat . intersperse "\n" . map fmt

-- Convert a list of namespaces to a tree representation
toForest :: [[Text]] -> Forest Text
toForest = unfoldForest build . groupByHead
 where
  groupByHead = groupBy (on (==) head)

  build :: [[Text]] -> (Text, [[[Text]]])
  build group@(representative : _) = (head representative, (groupByHead . filter (not . null) . map tail) group)
