module Cardano.Tracer.Handlers.RTView.State.Displayed
  ( DisplayedForNode
  , DisplayedElements
  , DisplayedNodes
  , PageReloadedFlag
  , getDisplayedValue
  , initDisplayedElements
  , initDisplayedNodes
  , initPageReloadFlag
  , pageWasReload
  , pageWasNotReload
  , saveDisplayedValue
  , updateDisplayedNodes
  , updateDisplayedElements
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

import           Cardano.Tracer.Types (ConnectedNodes, NodeId)
import           Cardano.Tracer.Utils (initConnectedNodes)

type ElementId    = Text
type ElementValue = Text

-- | We store all currently displayed values for all elements in each node panel.
--
--   There are 2 reasons for it:
--
--   1. If received 'TraceObject' contains the data with the same value again, we
--      won't update corresponding element, to keep web-traffic.
--   2. If the user reloaded the page, all previously displayed elements will be
--      updated in the same state again.
--
type DisplayedForNode  = Map ElementId ElementValue
type DisplayedElements = TVar (Map NodeId DisplayedForNode)

initDisplayedElements :: IO DisplayedElements
initDisplayedElements = newTVarIO M.empty

-- | This set contains ids of currently displayed nodes panels.
type DisplayedNodes = ConnectedNodes

initDisplayedNodes :: IO DisplayedNodes
initDisplayedNodes = initConnectedNodes

updateDisplayedNodes :: DisplayedNodes -> Set NodeId -> IO ()
updateDisplayedNodes displayed connected = atomically $
  modifyTVar' displayed . const $ connected

getDisplayedValue
  :: DisplayedElements
  -> NodeId
  -> ElementId
  -> IO (Maybe ElementValue)
getDisplayedValue displayedElements nodeId elId =
  maybe Nothing (M.lookup elId) . M.lookup nodeId <$> readTVarIO displayedElements

saveDisplayedValue
  :: DisplayedElements
  -> NodeId
  -> ElementId
  -> ElementValue
  -> IO ()
saveDisplayedValue displayedElements nodeId elId elValue = atomically $
  modifyTVar' displayedElements $ M.adjust (M.insert elId elValue) nodeId

-- | We have to delete 'NodeId' from 'displayed' if it's not presented in 'connected'.
updateDisplayedElements :: DisplayedElements -> Set NodeId -> IO ()
updateDisplayedElements displayedElements connected = atomically $
  modifyTVar' displayedElements $
    M.fromList . mapMaybe removeForDisconnectedNode . M.toList
 where
  removeForDisconnectedNode elsForThisNode@(nodeId, _) =
    if nodeId `S.member` connected
      then Just elsForThisNode
      else Nothing

-- | If the user reloaded the web-page, after DOM re-rendering, we have to restore
--   displayed state of all elements that they have _before_ page's reload.
type PageReloadedFlag = TVar Bool

initPageReloadFlag :: IO PageReloadedFlag
initPageReloadFlag = newTVarIO True

pageWasReload, pageWasNotReload :: PageReloadedFlag -> IO ()
pageWasReload    flag = atomically $ modifyTVar' flag . const $ True
pageWasNotReload flag = atomically $ modifyTVar' flag . const $ False
