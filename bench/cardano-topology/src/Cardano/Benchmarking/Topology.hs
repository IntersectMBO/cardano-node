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
import           Data.List      (tails)
import           Data.Maybe     (isJust)

import qualified Cardano.Benchmarking.Topology.Types as Types

--------------------------------------------------------------------------------

-- | Three types of topologies for the core nodes.
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
    , producers = map Types.name coreNodes
    , org = "IOHK"
    , pools = Nothing
    , stakePool = Just False
  })

--------------------------------------------------------------------------------

-- | Intermediate structure to work with Nodes producers by ID instead of name.
data Spec = Spec
  { id     :: Int
  , loc    :: Types.Location
  , mpools :: Maybe Int
  , links  :: [Int]
  }
  deriving Show

idName :: Int -> String
idName = ("node-" <>) . show

mkNode :: Spec -> Types.Node
mkNode Spec{..} = Types.Node{..} where
  name = idName nodeId
  org = "IOHK"
  nodeId = id
  pools = mpools
  stakePool = Just $ isJust mpools
  region = loc
  producers = idName <$> links

mkCoreNodes' :: CoreNodesParams -> [Spec]
mkCoreNodes' Line{..} = breakLoop tpSize phase1 where
  specIds = [0..(tpSize - 1)]
  -- Assign locations and pool counts;  set initial links.
  phase0 = mkInitial <$> specIds
    where
      mkInitial :: Int -> Spec
      mkInitial id =
        Spec {
          links = []
        , mpools = tpIdPools id
        , loc = tpLocation
        , ..
        }
  -- Connect into a ring
  phase1 = intraConnectRing False True phase0
mkCoreNodes' UniCircle{..} = phase1 where
  specIds = [0..(tpSize - 1)]
  -- Assign locations and pool counts;  set initial links.
  phase0 = mkInitial <$> specIds
    where
      mkInitial :: Int -> Spec
      mkInitial id =
        Spec {
          links = []
        , mpools = tpIdPools id
        , loc = tpLocation
        , ..
        }
  -- Connect into a ring
  phase1 = intraConnectRing False False phase0
mkCoreNodes' Torus{..} = concat phase3 where
  specIds = [0..(tpSize - 1)]
  specLocs = take tpSize $ cycle tpLocations
  -- Assign locations and pool counts;  set initial links.
  phase0 = zipWith mkInitial specIds specLocs
    where
      mkInitial :: Int -> Types.Location -> Spec
      mkInitial id loc =
        Spec {
          links = []
        , mpools = tpIdPools id
        , ..
        }
  -- Split into per-location lists.
  phase1 =
        [
                  filter ((== l) . loc) phase0
                | l <- tpLocations
        ]
     &
        filter (not . null)
  -- Establish intra-location connections.
  phase2 = intraConnectRing (tpSize < 6) True <$> phase1
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
        [  x { links = ids <> links x }
           | (x, i) <- zip xs [0..]
           , let ids = idOf i <$> rings
        ]
        where
          rings = take (nlocs - 1) $ cycle <$> xss'
          idOf n xs' = id (xs' !! n)
      linker [] = error "Invariant failure: empty list of specs"

breakLoop :: Int -> [Spec] -> [Spec]
breakLoop tpSize
  = updateHead (filterLinks (/= tpSize - 1))
  . updateLast (filterLinks (/= 0))

filterLinks :: (Int -> Bool) -> Spec -> Spec
filterLinks f s@Spec{..} = s { links = filter f links }

intraConnectRing :: Bool -> Bool -> [Spec] -> [Spec]
intraConnectRing withChords bidirectional specs =
  case len of
    0 -> []
    1 -> specs
    2 -> connect 1
         specs
    _ -> connect 1                             -- next
       $ if not bidirectional
         then specs
         else connect (len - 1)                -- prev
              $ if not withChords
                then specs
                else connect (len `div` 3)     -- chord 1
                     $ if len < 9
                       then specs
                       else connect ((len * 2) `div` 3) -- chord 2
                            specs
 where
   len = length specs
   connect :: Int -> [Spec] -> [Spec]
   connect offt xs =
       take (length xs) $
       fmap linker (tails ring)
     where linker (x:xs') =
             x { links = idOf (offt - 1) xs'
                       : links x }
           linker [] = error "Invariant failure: empty list of specs"
           ring = cycle xs
           idOf n xs' = id (xs' !! n)

--- * Aux
---
-- | Update the first element of a list, if it exists.
--   O(1).
updateHead :: (a -> a) -> [a] -> [a]
updateHead _ []       = []
updateHead f (a : as) = f a : as

-- | Update the last element of a list, if it exists.
--   O(n).
updateLast :: (a -> a) -> [a] -> [a]
updateLast _ [] = []
updateLast f (a : as) = loop a as
  -- Using a helper function to minimize the pattern matching.
  where
  loop a []       = [f a]
  loop a (b : bs) = a : loop b bs
