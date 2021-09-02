{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo
    ( mkOwnInfo
    ) where

import           Control.Monad (forM, void)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           System.FilePath.Posix (takeDirectory)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, set, string, text, (#), (#+), (#.))

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons

{-
import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.Config (configFileIsProvided, notificationsFileIsProvided,
                                        logFilesDir, savedConfigurationFile,
                                        savedNotificationsFile, savedRTViewParamsFile)
import           Cardano.RTView.GUI.Elements (HTMLClass (..), (#.), hideIt)
import           Cardano.RTView.GUI.JS.Utils (copyTextToClipboard)
import           Cardano.RTView.Git.Rev (gitRev)
import           Cardano.RTView.SupportedNodes (supportedNodesVersions)
import           Paths_cardano_rt_view (version)
-}

mkOwnInfo :: Element -> UI Element
mkOwnInfo closeIt =
  UI.div #. "modal" #+
    [ UI.div #. "modal-background" #+ []
    , UI.div #. "modal-content" #+
        [ UI.div #. "container" #+
            [ UI.div #. "box" #+
                [ UI.div #. "columns" #+
                    [ UI.div #. "column has-text-right" #+
                        [ UI.p #. "mb-1" #+ [ string "Version"
                                            , image "rt-view-what-icon" questionSVG
                                                    # set UI.title__ "Version of cardano-tracer RTView is a part of"
                                            ]
                        , UI.p #. "mb-1" #+ [ string "Commit"
                                            , image "rt-view-what-icon" questionSVG
                                                    # set UI.title__ "Git commit cardano-tracer was built from"
                                            ]
                        , UI.p #. "mb-1" #+ [ string "Platform"
                                            , image "rt-view-what-icon" questionSVG
                                                    # set UI.title__ "Platform cardano-tracer is running on"
                                            ]
                        , UI.p           #+ [ string "Supported nodes"
                                            , image "rt-view-what-icon" questionSVG
                                                    # set UI.title__ "Versions of the nodes RTView was tested with"
                                            ]
                        ]
                    , UI.div #. "column has-text-weight-semibold" #+
                        [ UI.p #. "mb-1" #+ [string "1.0"]
                        , UI.p #. "mb-1" #+ [string "abcdefg"]
                        , UI.p #. "mb-1" #+ [string "Linux"]
                        , UI.div #. "tags are-medium" #+
                            [ UI.span #. "tag is-link" # set text "1.27.0"
                            , UI.span #. "tag is-link" # set text "1.28.0"
                            , UI.span #. "tag is-link" # set text "1.29.0"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    , element closeIt
    ]
 where
  image imgClass svg = UI.span #. imgClass # set UI.html svg
