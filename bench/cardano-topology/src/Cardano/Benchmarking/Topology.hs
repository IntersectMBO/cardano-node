{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-name-shadowing #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Topology (
    Types.Topology (..)
  , Types.Node (..)
  , Types.Location (..)
  , Types.AWSRegion (..)
  , CoreNodesParams (..)
  , mkTopology, mkCoreNodes, mkExplorer
) where

import           Prelude hiding (id)

import           Data.Function  ((&))
import           Data.List      (tails, sortOn, uncons)
import           Data.Maybe     (isJust)

import qualified Cardano.Benchmarking.Topology.Types as Types

--------------------------------------------------------------------------------

-- | Four types of topologies for the core nodes.
data CoreNodesParams
  -- FIXME: Currently not used ???
  = Line
    { tpSize      :: Int              -- ^ Number of nodes to use.
    , tpLocation  :: Types.Location   -- ^ Unique location to use.
    , tpIdPools   :: Int -> Maybe Int -- ^ Node i pool number.
    }
  | UniCircle
    { tpSize      :: Int              -- ^ Number of nodes to use.
    , tpLocation  :: Types.Location   -- ^ Unique location to use.
    , tpIdPools   :: Int -> Maybe Int -- ^ Node i pool number.
    }
  | Torus
    { tpSize      :: Int              -- ^ Number of nodes to use.
    , tpLocations :: [Types.Location] -- ^ Locations to use.
    , tpIdPools   :: Int -> Maybe Int -- ^ Node i pool number.
    }
  | TorusDense
    { tpSize      :: Int              -- ^ Number of nodes to use.
    , tpLocations :: [Types.Location] -- ^ Locations to use.
    , tpIdPools   :: Int -> Maybe Int -- ^ Node i pool number.
    }

--------------------------------------------------------------------------------

-- | Create a Topology, optional explorer node that connects to all core nodes.
mkTopology :: CoreNodesParams -> Maybe Types.Location -> Types.Topology
mkTopology coreNodesParams maybeExplorerLocation =
  let coreNodes = mkCoreNodes coreNodesParams
  in  Types.Topology {
      Types.coreNodes = coreNodes
    , Types.relayNodes = case maybeExplorerLocation of
        Nothing -> []
        (Just explorerLocation) -> [ mkExplorer explorerLocation coreNodes ]
  }

mkCoreNodes :: CoreNodesParams -> [Types.Node]
mkCoreNodes coreNodesParams = map mkNode (mkCoreNodes' coreNodesParams)

mkExplorer :: Types.Location -> [Types.Node] -> Types.Node
mkExplorer explorerLocation coreNodes =
  (Types.Node {
      name   = "explorer"
    , nodeId = length coreNodes
    , region = explorerLocation
    -- Explorer producers sorted by numeric id, not region or something else.
    , producers = map Types.name (sortOn Types.nodeId coreNodes)
    , org = "IOHK"
    , pools = Nothing
    , stakePool = Nothing
  })

--------------------------------------------------------------------------------

-- | Intermediate structure to work with Nodes producers by ID instead of name.
data Spec = Spec
  { specId :: Int
  , loc    :: Types.Location
  , mpools :: Maybe Int
  , links  :: [Int]
  }
  deriving Show

idName :: Int -> String
idName = ("node-" <>) . show

-- | Create a node from Spec adding the default values.
mkNode :: Spec -> Types.Node
mkNode Spec{..} = Types.Node{..} where
  name = idName nodeId
  org = "IOHK"
  nodeId = specId
  pools = mpools
  stakePool = Just $ isJust mpools
  region = loc
  producers = idName <$> links

mkCoreNodes' :: CoreNodesParams -> [Spec]
mkCoreNodes' Line{..} = breakLoop tpSize phase1 where
  -- Nodes' ids starting from zero.
  specIds = [0..(tpSize - 1)]
  -- Assign each node the location, an empty links list and pool count.
  phase0 = mkInitial <$> specIds
    where
      mkInitial :: Int -> Spec
      mkInitial specId =
        Spec {
            links = []
          , mpools = tpIdPools specId
          , loc = tpLocation
          , ..
        }
  -- Connect into a ring with bidirectional = true
  phase1 = intraConnectRing False True phase0
mkCoreNodes' UniCircle{..} = phase1 where
  -- Nodes' ids starting from zero.
  specIds = [0..(tpSize - 1)]
  -- Assign each node the location, an empty links list and pool count.
  phase0 = mkInitial <$> specIds
    where
      mkInitial :: Int -> Spec
      mkInitial specId =
        Spec {
          links = []
        , mpools = tpIdPools specId
        , loc = tpLocation
        , ..
        }
  -- Connect into a ring with bidirectional = false
  phase1 = intraConnectRing False False phase0
mkCoreNodes' Torus{..} = mkCoreNodesTorus tpSize tpLocations tpIdPools False
-- The dense Torus, with each node having 4 intra-region connections, needs
-- a minimun of nodes.
mkCoreNodes' TorusDense{..} = mkCoreNodesTorus tpSize tpLocations tpIdPools True

mkCoreNodesTorus ::Int -> [Types.Location] -> (Int -> Maybe Int) -> Bool -> [Spec]
mkCoreNodesTorus tpSize' tpLocations' tpIdPools' dense = concat phase3 where
  -- Nodes' ids starting from zero.
  -- For the usual torus/dense topology it returns:
  -- [ 0, 1, 2,.., 51 ]
  specIds = [0..(tpSize' - 1)]
  -- A list of repeating locations (in the same order).
  -- For the usual torus/dense topology it returns:
  -- [ "eu", "us", "ap", "eu", "us", "ap" .. "eu" ]
  specLocs = take tpSize' $ cycle tpLocations'
  -- Orderly (zip) assign the locations, an empty links list and pool count.
  -- For the usual torus/dense topology it returns:
  -- [
  --      ( 0,"eu"), ( 1,"us"), ( 2,"ap"),
  --      ..
  --      (45,"eu"), (46,"us"), (47,"ap"),
  --      (48,"eu"), (49,"us"), (50,"ap"), (51,"eu")
  --  ]
  phase0 = zipWith mkInitial specIds specLocs
    where
      mkInitial :: Int -> Types.Location -> Spec
      mkInitial specId loc =
        Spec {
          links = []
        , mpools = tpIdPools' specId
        , ..
        }
  -- Split into per-location lists (list of lists).
  -- For the usual torus/dense topology it returns:
  -- [
  --    [ (0,"eu"), (3,"eu"), .. (48,"eu"), (51,"eu") ]
  --    [ (1,"us"), (4,"us"), .. (49,"us") ]
  --    [ (2,"ap"), (5,"ap"), .. (50,"ap") ]
  -- ]
  phase1 =
      [
          filter ((== l) . loc) phase0
        | l <- tpLocations'
      ]
    &
      filter (not . null)
  -- Establish intra-location connections for every location list.
  -- For the usual torus topology: it first adds a connection to the
  -- inmmediate next same-region node and one to the inmmediate previous
  -- same-region node.
  -- [
  --     Spec {specId =  0, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 3,51]}
  --   , Spec {specId =  3, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 6, 0]}
  --   , Spec {specId =  6, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 9, 3]}
  --   ...
  --   , Spec {specId = 45, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [48,42]}
  --   , Spec {specId = 48, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [51,45]}
  --   , Spec {specId = 51, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 0,48]}
  -- ]
  -- For the dense version of the Torus it add two more intra links:
  -- [
  --     Spec {specId = 0,  loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 3,51,18,36]}
  --   , Spec {specId = 3,  loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 6, 0,21,39]}
  --   , Spec {specId = 6,  loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 9, 3,24,42]}
  --   ...
  --   , Spec {specId = 45, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [48,42, 9,27]}
  --   , Spec {specId = 48, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [51,45,12,30]}
  --   , Spec {specId = 51, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 0,48,15,33]}
  -- ]
  phase2 = intraConnectRing dense True <$> phase1
  -- Establish inter-location connections.
  phase3 = if length phase2 > 1
    then interConnect phase2
    else phase2
  interConnect :: [[Spec]] -> [[Spec]]
  interConnect xss =
    case nlocs of
      0 -> xss
      1 -> xss
      _ -> take nlocs $ fmap linker (tails $ cycle xss)
    where
      nlocs = length xss
      linker (xs:xss') =
        [ x { links = ids <> links x }
          | (x, i) <- zip xs [0..]
          , let ids = idOf i <$> rings
        ]
        where
          rings = take (nlocs - 1) $ cycle <$> xss'
          idOf n xs' = specId (xs' !! n)
      linker [] = error "Invariant failure: empty list of specs"

breakLoop :: Int -> [Spec] -> [Spec]
breakLoop tpSize
  = updateHead (filterLinks (/= tpSize - 1))
  . updateLast (filterLinks (/= 0))

filterLinks :: (Int -> Bool) -> Spec -> Spec
filterLinks f s@Spec{..} = s { links = filter f links }

{-- Examples:

intraConnectRing False False $
> map
  (\id' -> Spec id' (Types.AWS Types.EU_CENTRAL_1) Nothing [])
  [0,3..51]
[
  Spec {specId =  0, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 3]}
, Spec {specId =  3, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 6]}
, Spec {specId =  6, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 9]}
...
, Spec {specId = 45, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [48]}
, Spec {specId = 48, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [51]}
, Spec {specId = 51, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 0]}
]

intraConnectRing False True $
> map
  (\id' -> Spec id' (Types.AWS Types.EU_CENTRAL_1) Nothing [])
  [0,3..51]
[
  Spec {specId =  0, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 3,51]}
, Spec {specId =  3, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 6, 0]}
, Spec {specId =  6, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 9, 3]}
...
, Spec {specId = 45, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [48,42]}
, Spec {specId = 48, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [51,45]}
, Spec {specId = 51, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 0,48]}
]

intraConnectRing True False $
> map
  (\id' -> Spec id' (Types.AWS Types.EU_CENTRAL_1) Nothing [])
  [0,3..51]
[
  Spec {specId =  0, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 3]}
, Spec {specId =  3, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 6]}
, Spec {specId =  6, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 9]}
...
, Spec {specId = 45, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [48]}
, Spec {specId = 48, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [51]}
, Spec {specId = 51, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 0]}
]

intraConnectRing True True $
> map
  (\id' -> Spec id' (Types.AWS Types.EU_CENTRAL_1) Nothing [])
  [0,3..51]
[
  Spec {specId = 0,  loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 3,51,18,36]}
, Spec {specId = 3,  loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 6, 0,21,39]}
, Spec {specId = 6,  loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 9, 3,24,42]}
...
, Spec {specId = 45, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [48,42, 9,27]}
, Spec {specId = 48, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [51,45,12,30]}
, Spec {specId = 51, loc = AWS EU_CENTRAL_1, mpools = Nothing, links = [ 0,48,15,33]}
]

--}
intraConnectRing :: Bool -> Bool -> [Spec] -> [Spec]
intraConnectRing withChords bidirectional specs =
  case len of
    0 -> []
    1 -> specs
    2 -> connect 1 specs
    _ -> connect 1                             -- next
       $ if not bidirectional
         then specs
         else connect (len - 1)                -- prev
              $ if not withChords
                then specs
                -- Add two more intra-region connections.
                else connect (len `div` 3)     -- chord 1
                     $ if len < 9
                       then specs
                       else connect ((len * 2) `div` 3) -- chord 2
                            specs
 where
   len = length specs
   connect :: Int -> [Spec] -> [Spec]
   connect offt xs = take (length xs) $ fmap linker (tails ring)
     where linker (x:xs') =
             x { links = idOf (offt - 1) xs'
                       : links x }
           linker [] = error "Invariant failure: empty list of specs"
           ring = cycle xs
           idOf n xs' = specId (xs' !! n)

--- * Aux
---
-- | Update the first element of a list, if it exists.
--   O(1).
updateHead :: (a -> a) -> [a] -> [a]
updateHead f xs = case uncons xs of
  Nothing -> []
  Just (x,xs') -> f x:xs'

-- | Update the last element of a list, if it exists.
--   O(n).
updateLast :: (a -> a) -> [a] -> [a]
updateLast _ [] = []
updateLast f (a : as) = loop a as
  -- Using a helper function to minimize the pattern matching.
  where
  loop a []       = [f a]
  loop a (b : bs) = a : loop b bs
