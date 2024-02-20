module Cardano.Logging.Consistency (
    NSWarnings
  , checkTraceConfiguration
  , checkTraceConfiguration'
) where

import           Cardano.Logging.ConfigurationParser
import           Cardano.Logging.Types

import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T

-- | Warnings as a list of text
type NSWarnings = [T.Text]

-- | A data structure for the lookup of namespaces as nested maps
newtype NSLookup = NSLookup (Map.Map T.Text NSLookup)
  deriving Show


-- | Checks if all namespaces in this configuration are legal.
--   Legal in this case means that it can be found by a hierarchcical
--   lookup in all namespaces.
--   Warns if namespaces in all namespaces are not unique,
--   Warns if namespaces in all namespaces are ending in the
--   middle of another namespace.
--   The namespaces in allNamespaces are consistent with the namespaces for the
--   severityFor, privacyFor, detailsFor, documentFor and metricsDofFor functions.
checkTraceConfiguration ::
     FilePath
  -> TraceConfig
  -> [([T.Text], [T.Text])]
  -> IO NSWarnings
checkTraceConfiguration configFileName defaultTraceConfig allNamespaces' = do
    trConfig <- readConfigurationWithDefault configFileName defaultTraceConfig
    pure $ checkTraceConfiguration' trConfig allNamespaces'

checkTraceConfiguration' ::
     TraceConfig
  -> [([T.Text], [T.Text])]
  -> NSWarnings
checkTraceConfiguration' trConfig allNamespaces' =
    let configNS        = Map.keys (tcOptions trConfig)
        emptyInner      = filter (null . snd) allNamespaces'
        allNamespaces'' = map (uncurry (<>)) allNamespaces'
        (nsLookup, systemWarnings) = asNSLookup allNamespaces''
        configWarnings  = mapMaybe (checkNamespace nsLookup) configNS
        allWarnings     = map ("System namespace error: "<>) systemWarnings
                            ++ map (\(ns, _) -> "Empty inner namespace: "
                                              <> T.intercalate "." ns) emptyInner
                              ++ map ("Config namespace error: " <>) configWarnings
    in allWarnings

-- | Check if a single namespace is legal. Legal in this case means that
--   it can be found by a hierarchcical lookup in all namespaces
checkNamespace :: NSLookup -> [T.Text] -> Maybe T.Text
checkNamespace nsLookup ns = go nsLookup ns
  where
    go :: NSLookup -> [T.Text] -> Maybe T.Text
    go _ [] = Nothing
    go (NSLookup l) (nshd : nstl) = case Map.lookup nshd l of
                                      Nothing -> Just ("Illegal namespace "
                                                        <> T.intercalate "." ns)
                                      Just l2 -> go l2 nstl

-- | Warns if namespaces in all namespaces are not unique,
--   Warns as well if namespaces in all namespaces are ending in the
--   middle of another namespace.
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

