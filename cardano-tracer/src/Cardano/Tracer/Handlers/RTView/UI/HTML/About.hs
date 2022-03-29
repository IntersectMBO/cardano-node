{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.About
  ( mkAboutInfo
  ) where

import qualified Data.Text as T
import           Data.Version (showVersion)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Directory (makeAbsolute)
import           System.Environment (getArgs)
import           System.Info.Extra (isWindows, isMac)

import           Cardano.Git.Rev (gitRev)

import           Cardano.Tracer.Handlers.RTView.System
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Paths_cardano_tracer (version)

mkAboutInfo :: UI Element
mkAboutInfo = do
  pathToConfig <- liftIO $ getArgs >>= \case
    ["-c",       path] -> makeAbsolute path
    ["--config", path] -> makeAbsolute path
    _                  -> return ""
  copyPath <- image "has-tooltip-multiline has-tooltip-top rt-view-copy-icon" copySVG
                    # set dataTooltip "Click to copy the path"
  on UI.click copyPath . const $
    copyTextToClipboard pathToConfig
  closeIt <- UI.button #. "delete"
  pid <- getProcessId
  info <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card" #+
          [ UI.header #. "modal-card-head rt-view-about-head" #+
              [ UI.p #. "modal-card-title rt-view-about-title" # set text "About"
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-about-body" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column ml-1 is-one-third" #+
                      [ UI.p #. "mb-3" #+
                          [ image "rt-view-overview-icon" versionSVG
                          , string "Version"
                          ]
                      , UI.p #. "mb-3" #+
                          [ image "rt-view-overview-icon" commitSVG
                          , string "Commit"
                          ]
                      , UI.p #. "mb-3" #+
                          [ image "rt-view-overview-icon" platformSVG
                          , string "Platform"
                          ]
                      , UI.p #. "mb-3" #+
                          [ image "rt-view-overview-icon" configSVG
                          , string "Configuration"
                          ]
                      , UI.p #. "mb-1" #+
                          [ image "rt-view-overview-icon" serverSVG
                          , string "Process ID"
                          ]
                      ]
                  , UI.div #. "column has-text-weight-semibold" #+
                      [ UI.p #. "mb-3" #+ [string $ showVersion version]
                      , UI.p #. "mb-3" #+
                          [ UI.anchor
                               #. ("rt-view-href is-family-monospace has-text-weight-normal"
                                   <> " has-tooltip-multiline has-tooltip-top")
                               # set UI.href ("https://github.com/input-output-hk/cardano-node/commit/" <> commit)
                               # set UI.target "_blank"
                               # set dataTooltip "Browse repository on this commit"
                               # set text commit
                          , image "rt-view-href-icon" externalLinkSVG
                          ]
                      , UI.p #. "mb-3" #+
                          [ string $ if | isWindows -> "Windows"
                                        | isMac     -> "macOS"
                                        | otherwise -> "Linux"
                          ]
                      , UI.p #. "mb-3" #+
                          [ UI.span #. ("tag is-info is-light is-rounded is-medium mr-3"
                                        <> " has-tooltip-multiline has-tooltip-top rt-view-logs-path")
                                    # set dataTooltip "The path to configuration file"
                                    # set text (shortenPath pathToConfig)
                          , element copyPath
                          ]
                      , UI.p #. "mb-1" #+
                          [ string $ show pid
                          ]
                      ]
                  ]
              ]
          ]
      ]
  on UI.click closeIt . const $ element info #. "modal"
  return info
 where
  commit = T.unpack . T.take 7 $ gitRev
