module Cardano.Logging.Consistency (
    NSWarnings
  , checkTraceConfiguration
  , checkTraceConfiguration'
) where

import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T


import           Cardano.Logging.ConfigurationParser
import           Cardano.Logging.Types

-- | Warniings as a list of text
type NSWarnings = [T.Text]

  -- | A data structure for the lookup of namespaces as nested maps
newtype NSLookup = NSLookup (Map.Map T.Text NSLookup)
  deriving Show

checkTraceConfiguration ::
     FilePath
  -> TraceConfig
  -> [[T.Text]]
  -> IO NSWarnings
checkTraceConfiguration configFileName defaultTraceConfig allNamespaces' = do
    trConfig <- readConfigurationWithDefault configFileName defaultTraceConfig
    pure $ checkTraceConfiguration' trConfig allNamespaces'


checkTraceConfiguration' ::
     TraceConfig
  -> [[T.Text]]
  -> NSWarnings
checkTraceConfiguration' trConfig allNamespaces' =
    let namespaces     = Map.keys (tcOptions trConfig)
        (nsLookup, systemWarnings) = asNSLookup allNamespaces'
        configWarnings = mapMaybe (checkNamespace nsLookup) namespaces
        allWarnings    = map ("System namespace error: "<>) systemWarnings ++
                           map ("Config namespace error: " <>) configWarnings
    in allWarnings

-- | Check if a single namespace is legal. Returns just a warning test,
-- if this is not the case
checkNamespace :: NSLookup -> [T.Text] -> Maybe T.Text
checkNamespace nsLookup ns = go nsLookup ns
  where
    go :: NSLookup -> [T.Text] -> Maybe T.Text
    go _ [] = Nothing
    go (NSLookup l) (nshd : nstl) = case Map.lookup nshd l of
                                      Nothing -> Just ("Illegal namespace "
                                                        <> T.intercalate "." ns)
                                      Just l2 -> go l2 nstl

-- | Builds a namespace lookup structure from a list of namespaces
-- Warns if namespaces are not unique, and if a namespace is a subnamespace
-- of other namespaces
asNSLookup :: [[T.Text]] -> (NSLookup, NSWarnings)
asNSLookup = foldl' (fillLookup []) (NSLookup Map.empty, [])
  where
    fillLookup :: [T.Text] -> (NSLookup, NSWarnings) -> [T.Text] -> (NSLookup, NSWarnings)
    fillLookup _nsFull (NSLookup nsl, nsw)  [] = (NSLookup nsl, nsw)
    fillLookup nsFull (NSLookup nsl, nsw) (ns1 : nstail) =
      case Map.lookup ns1 nsl of
        Nothing   ->  let nsNew = Map.empty
                          (NSLookup nsl2, nsw2) = fillLookup
                                                    (nsFull <> [ns1])
                                                    (NSLookup nsNew, [])
                                                    nstail
                          res = NSLookup (Map.insert ns1 (NSLookup nsl2) nsl)
                          newWarnings =  nsw <> nsw2
                      in (res, newWarnings)
        Just (NSLookup nsm)
                  ->  let (NSLookup nsl2, nsw2) = fillLookup
                                                  (nsFull <> [ns1])
                                                  (NSLookup nsm, [])
                                                  nstail
                          res = NSLookup (Map.insert ns1 (NSLookup nsl2) nsl)
                          condWarning = if null nstail
                                          then
                                            if Map.null nsm
                                              then Just ("Duplicate namespace "
                                                        <> T.intercalate "." (nsFull <> [ns1]))
                                              else Just ("Inner namespace duplicate "
                                                        <> T.intercalate "." (nsFull <> [ns1]))
                                          else Nothing
                          newWarnings = case condWarning of
                                           Nothing -> nsw <> nsw2
                                           Just w  -> w : (nsw <> nsw2)
                      in (res, newWarnings)

