module Cardano.Tracer.Handlers.RTView.State.Displayed
  ( DisplayedElements
  , PageReloadedFlag
  , cleanupDisplayedValues
  , getDisplayedValue
  , getDisplayedValuePure
  , initDisplayedElements
  , initPageReloadFlag
  , pageWasReload
  , pageWasNotReload
  , saveDisplayedValue
  , updateDisplayedElements
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Data.List ((\\))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

import           Cardano.Tracer.Types (NodeId)

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

cleanupDisplayedValues
  :: DisplayedElements
  -> IO ()
cleanupDisplayedValues displayedElements = atomically $
  modifyTVar' displayedElements $ M.map (const M.empty)

getDisplayedValue
  :: DisplayedElements
  -> NodeId
  -> ElementId
  -> IO (Maybe ElementValue)
getDisplayedValue displayedElements nodeId elId =
  maybe Nothing (M.lookup elId) . M.lookup nodeId <$> readTVarIO displayedElements

getDisplayedValuePure
  :: Map NodeId DisplayedForNode
  -> NodeId
  -> ElementId
  -> Maybe ElementValue
getDisplayedValuePure displayed nodeId elId =
  maybe Nothing (M.lookup elId) . M.lookup nodeId $ displayed

saveDisplayedValue
  :: DisplayedElements
  -> NodeId
  -> ElementId
  -> ElementValue
  -> IO ()
saveDisplayedValue displayedElements nodeId elId elValue = atomically $
  modifyTVar' displayedElements $ \currentDisplayedEls ->
    case M.lookup nodeId currentDisplayedEls of
      Nothing -> M.insert nodeId (M.singleton elId elValue) currentDisplayedEls
      Just elsForNode ->
        let elsForNode' =
              case M.lookup elId elsForNode of
                Nothing -> M.insert elId elValue elsForNode
                Just _  -> M.adjust (const elValue) elId elsForNode
        in M.adjust (const elsForNode') nodeId currentDisplayedEls

-- | Makes 'displayedElements' up-to-date with 'connected'. There are 3 cases:
--
--   1. 'displayedElements' contains all node ids from 'connected'
--      - just keep it.
--   2. 'displayedElements' contains some node ids that is not presented in 'connected'
--      - remove them, these ids correspond to disconnected nodes.
--   3. 'displayedElements' doesn't contain some node ids that presented in 'connected'
--      - add them, these ids correspond to newly connected nodes.
--
--   2-nd and 3-d cases can coexist.
--
updateDisplayedElements
  :: DisplayedElements
  -> Set NodeId
  -> IO ()
updateDisplayedElements displayedElements connected = atomically $
  modifyTVar' displayedElements $ \currentDisplayedEls ->
    let connectedIds = S.toList connected
        displayedIds = M.keys currentDisplayedEls
    in if displayedIds == connectedIds
         then currentDisplayedEls -- 1-st case.
         else
           let disconnectedIds   = displayedIds \\ connectedIds -- In 'displayedIds' but not in 'connectedIds'.
               newlyConnectedIds = connectedIds \\ displayedIds -- In 'connectedIds' but not in 'displayedIds'.
               withoutDisconnected = deleteDisconnected disconnectedIds currentDisplayedEls
           in addNewlyConnected newlyConnectedIds withoutDisconnected
 where
  deleteDisconnected = go
   where
    go [] els = els
    go (anId:ids) els = go ids $ M.delete anId els

  addNewlyConnected = go
   where
    go [] els = els
    go (anId:ids) els = go ids $ M.insert anId M.empty els

-- | If the user reloaded the web-page, after DOM re-rendering, we have to restore
--   displayed state of all elements that they have _before_ page's reload.
type PageReloadedFlag = TVar Bool

initPageReloadFlag :: IO PageReloadedFlag
initPageReloadFlag = newTVarIO True

pageWasReload, pageWasNotReload :: PageReloadedFlag -> IO ()
pageWasReload    = setFlag True
pageWasNotReload = setFlag False

setFlag :: Bool -> PageReloadedFlag -> IO ()
setFlag state flag = atomically . modifyTVar' flag . const $ state
