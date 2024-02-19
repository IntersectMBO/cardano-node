{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.EKG
  ( mkEKGMetricsWindow
  ) where

import           Cardano.Tracer.Handlers.RTView.UI.Utils

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

mkEKGMetricsWindow :: String -> UI Element
mkEKGMetricsWindow anId = do
  closeIt <- UI.button #. "delete"
  metricsWindow <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-ekg-metrics-modal" #+
          [ UI.header #. "modal-card-head rt-view-ekg-metrics-head" #+
              [ UI.p #. "modal-card-title rt-view-ekg-metrics-title" #+
                  [ string "EKG metrics from "
                  , UI.span ## (anId <> "__node-name-for-ekg-metrics")
                            #. "has-text-weight-bold"
                            # set text anId
                  ]
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-ekg-metrics-body" #+
              [ UI.div ## "ekg-metrics-columns" #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.span #. "is-size-4 has-text-weight-bold" # set text "Metric name"
                      , UI.p #. "mt-3" #+
                          [ UI.span ## (anId <> "__node-ekg-metrics-names") #. "is-family-monospace" # set text "—"
                          ]
                      ]
                  , UI.div #. "column has-text-right" #+
                      [ UI.span #. "is-size-4 has-text-weight-bold" # set text "Metric value"
                      , UI.p #. "mt-3" #+
                          [ UI.span ## (anId <> "__node-ekg-metrics-values") # set text "—"
                          ]
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element metricsWindow #. "modal"
  return metricsWindow
