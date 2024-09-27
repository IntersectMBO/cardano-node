{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Tracer.Handlers.Metrics.Utils
  ( module Cardano.Tracer.Handlers.Metrics.Utils
  ) where

import           Cardano.Tracer.Environment (TracerEnv (..))
import           Cardano.Tracer.Types (MetricsStores, NodeId, NodeName)

import           Prelude hiding (head)

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (readTVar)
import           Data.Aeson (encode)
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Lazy as Lazy
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Types (ResponseHeaders, hContentType)
import qualified System.Metrics as EKG
import           Text.Blaze.Html (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Html5 (Markup, a, body, head, html, li, textValue, title, toHtml, ul,
                   (!))
import           Text.Blaze.Html5.Attributes hiding (title)
import           Text.Slugify (slugify)


newtype RouteDictionary = RouteDictionary
  { getRouteDictionary :: [(Text, (EKG.Store, NodeName))]
  }

renderListOfConnectedNodes :: Text -> RouteDictionary -> Lazy.ByteString
renderListOfConnectedNodes metricsTitle (nodeNames -> nodenames)
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



contentHdrJSON, contentHdrOpenMetrics, contentHdrUtf8Html, contentHdrUtf8Text :: ResponseHeaders
contentHdrJSON        = [(hContentType, "application/json")]
contentHdrOpenMetrics = [(hContentType, "application/openmetrics-text; version=1.0.0; charset=utf-8")]
contentHdrUtf8Html    = [(hContentType, "text/html; charset=utf-8")]
contentHdrUtf8Text    = [(hContentType, "text/plain; charset=utf-8")]
