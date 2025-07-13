module Cardano.Logging.DocuGenerator.RoseTree (RoseTree (..), printTree, printList, toTree) where

import Cardano.Logging.Utils
import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text.Internal (Text)
import Data.Text.Internal.Builder (Builder, fromText)

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
data RoseTree = RoseTree Text [RoseTree]

printTree :: Int -> RoseTree -> Builder
printTree lvl (RoseTree x nested) =
  indent lvl (fromText $ "∙ " <> x <> "\n") <> mconcat (map (printTree (lvl + 1)) nested)

printList :: (a -> Builder) -> [a] -> Builder
printList _ [] = ""
printList fmt [x] = fmt x
printList fmt (x : xs) = fmt x <> "\n" <> printList fmt xs

-- Convert a list of namespaces to a tree representation
toTree :: [[Text]] -> [RoseTree]
toTree nss = map go heads
 where
  heads :: [Text]
  heads = nub (mapMaybe listToMaybe nss)

  go :: Text -> RoseTree
  go h = RoseTree h (toTree matchingHead)
   where
    matchingHead :: [[Text]]
    matchingHead =
      mapMaybe (\ns' -> do guard (head ns' == h); guard (not $ null $ tail ns'); Just (tail ns')) nss
