{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.Metrics.Utils
  ( RouteDictionary(..)
  , renderListOfConnectedNodes
  , renderJson
  , nodeNames
  , computeRoutes
  ) where

import qualified Data.ByteString.Lazy as Lazy
import           Data.Foldable (for_)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (head)
import qualified Data.Bimap as Bimap

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (readTVar)
import           Data.Aeson (encode)
import           Cardano.Tracer.Environment (TracerEnv(..))
import qualified System.Metrics as EKG
import           Cardano.Tracer.Types (NodeName, NodeId, MetricsStores)
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5 (Markup, a, li, ul, body, title, head, (!), textValue, html, toHtml) -- hiding (map)
import           Text.Blaze.Html5.Attributes hiding (title)
import           Text.Slugify (slugify)


newtype RouteDictionary = RouteDictionary
  { getRouteDictionary :: [(Text, (EKG.Store, NodeName))]
  }

renderListOfConnectedNodes :: Text -> [NodeName] -> Lazy.ByteString
renderListOfConnectedNodes metricsTitle nodenames
  | [] <- nodenames
  = "There are no connected nodes yet."
  | otherwise
  = renderHtml do mkPage mkHref nodenames

  where
  mkHref :: NodeName -> Markup
  mkHref nodeName =
    a ! href (textValue ("/" <> slugify nodeName))
      $ toHtml nodeName'
   where
    nodeName' = T.unpack nodeName

  mkPage :: (NodeName -> Markup) -> [NodeName] -> Html
  mkPage f hrefs = html do
    head $ title $ toHtml metricsTitle
    body $ ul $ for_ hrefs (li . f)

renderJson :: RouteDictionary -> Lazy.ByteString
renderJson (RouteDictionary routeDict) = encode do
  Map.fromList
    [ (nodeName, "/" <> slug)
    | (slug, (_store, nodeName)) <- routeDict
    ]

nodeNames :: RouteDictionary -> [NodeName]
nodeNames (RouteDictionary routeDict) = map (snd . snd) routeDict

computeRoutes :: TracerEnv -> IO RouteDictionary
computeRoutes TracerEnv{teConnectedNodesNames, teAcceptedMetrics} = atomically do
  nIdsWithNames :: Map NodeId NodeName <-
    Bimap.toMap <$> readTVar teConnectedNodesNames

  acceptedMetrics :: Map NodeId MetricsStores <-
    readTVar teAcceptedMetrics

  let mapFromNodeId :: Map NodeId (NodeName, MetricsStores)
      mapFromNodeId = Map.intersectionWith (,) nIdsWithNames acceptedMetrics

      routes :: [(Text, (EKG.Store, NodeName))]
      routes = [ (slugify nodeName, (metric, nodeName))
               | (nodeName, (metric, _)) <- Map.elems mapFromNodeId
               ]

  pure (RouteDictionary routes)
