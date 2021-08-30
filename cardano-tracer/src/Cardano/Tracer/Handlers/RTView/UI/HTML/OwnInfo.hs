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




  {-
mkOwnInfo'
  :: Configuration
  -> RTViewParams
  -> UI Element
mkOwnInfo' config params = do
  closeButton <- UI.img #. [W3DisplayTopright, RTViewInfoClose]
                        # set UI.src "/static/images/times.svg"
                        # set UI.title__ "Close"
  versions <- nodesVersions
  (pathToConfigFile, pathToNotificationsFile, pathToParamsFile, pathToLogsDir)
    <- liftIO $ getPaths config params

  copyPathToConfigFile
    <- UI.img #. [RTViewInfoCopyPathIcon]
              # set UI.src "/static/images/clipboard.svg"
              # set UI.title__ "Copy this path to clipboard"
  copyPathToNotificationsFile
    <- UI.img #. [RTViewInfoCopyPathIcon]
              # set UI.src "/static/images/clipboard.svg"
              # set UI.title__ "Copy this path to clipboard"
  copyPathToParamsFile
    <- UI.img #. [RTViewInfoCopyPathIcon]
              # set UI.src "/static/images/clipboard.svg"
              # set UI.title__ "Copy this path to clipboard"
  copyPathToLogsDir
    <- UI.img #. [RTViewInfoCopyPathIcon]
              # set UI.src "/static/images/clipboard.svg"
              # set UI.title__ "Copy this path to clipboard"

  void $ UI.onEvent (UI.click copyPathToConfigFile) $ \_ ->
    UI.runFunction $ UI.ffi copyTextToClipboard pathToConfigFile
  void $ UI.onEvent (UI.click copyPathToNotificationsFile) $ \_ ->
    UI.runFunction $ UI.ffi copyTextToClipboard pathToNotificationsFile
  void $ UI.onEvent (UI.click copyPathToParamsFile) $ \_ ->
    UI.runFunction $ UI.ffi copyTextToClipboard pathToParamsFile
  void $ UI.onEvent (UI.click copyPathToLogsDir) $ \_ ->
    UI.runFunction $ UI.ffi copyTextToClipboard pathToLogsDir

  let rtViewVersion = showVersion version

  ownInfo <-
    UI.div #. [W3Modal] #+
      [ UI.div #. [W3ModalContent, W3AnimateTop, W3Card4] #+
          [ UI.div #. [W3Container, RTViewInfoTop] #+
              [ element closeButton
              , UI.h2 #+ [ string "RTView Info" ]
              ]
          , UI.div #. [W3Container, W3Row, RTViewInfoContainer] #+
              [ UI.div #. [W3Half] #+
                  [ vSpacer
                  , UI.div #+ [string "Version"  # set UI.title__ "Version of RTView"]
                  , UI.div #+ [string "Commit"   # set UI.title__ "Git commit RTView was built from"]
                  , UI.div #+ [string "Platform" # set UI.title__ "Platform RTView is working on"]
                  , vSpacer
                  , UI.div #+ [string "Supported nodes" # set UI.title__ "Versions of the nodes RTView was tested with"]
                  , vSpacer
                  , UI.div #+ [string "Configuration file" # set UI.title__ "The path to RTView configuration file"]
                  , UI.div #+ [string "Notifications file" # set UI.title__ "The path to RTView notifications file"]
                  , UI.div #+ [string "Parameters file"    # set UI.title__ "The path to RTView parameters file"]
                  , UI.div #+ [string "Logs directory"     # set UI.title__ "The path to RTView logs directory"]
                  , vSpacer
                  ]
              , UI.div #. [W3Half, NodeInfoValues] #+
                  [ vSpacer
                  , UI.div #+
                      [ UI.anchor #. [CommitLink]
                                  # set UI.href ("https://github.com/input-output-hk/cardano-rt-view/releases/tag/"
                                                 <> rtViewVersion)
                                  # set UI.target "_blank"
                                  # set UI.title__ ("See release tag "
                                                    <> rtViewVersion <> " in cardano-rt-view repository")
                                  # set UI.text rtViewVersion
                      ]
                  , UI.div #+
                      [ UI.anchor #. [CommitLink]
                                  # set UI.href ("https://github.com/input-output-hk/cardano-rt-view/commit/"
                                                 <> gitRev)
                                  # set UI.target "_blank"
                                  # set UI.title__ "Browse cardano-rt-view repository on this commit"
                                  # set UI.text (take 7 gitRev)
                      ]
                  , UI.div #+ [string rtViewPlaform]
                  , vSpacer
                  , UI.div #+ (intersperse (string ", ") versions)
                  , vSpacer
                  , UI.div #+
                      [ string (preparePathIfNeeded pathToConfigFile) # set UI.title__ pathToConfigFile
                      , element copyPathToConfigFile
                      ]
                  , UI.div #+
                      [ string (preparePathIfNeeded pathToNotificationsFile) # set UI.title__ pathToNotificationsFile
                      , element copyPathToNotificationsFile
                      ] 
                  , UI.div #+
                      [ string (preparePathIfNeeded pathToParamsFile) # set UI.title__ pathToParamsFile
                      , element copyPathToParamsFile
                      ]
                  , UI.div #+
                      [ string (preparePathIfNeeded pathToLogsDir) # set UI.title__ pathToLogsDir
                      , element copyPathToLogsDir
                      ]
                  , vSpacer
                  ]
             ]
          ]
      ]
  void $ UI.onEvent (UI.click closeButton) $ \_ -> do
    element ownInfo # hideIt

  return ownInfo

vSpacer :: UI Element
vSpacer = UI.div #. [NodeInfoVSpacer] #+ []

rtViewPlaform :: String
#if defined(mingw32_HOST_OS)
rtViewPlaform = "Windows"
#elif defined(linux_HOST_OS)
rtViewPlaform = "Linux"
#elif defined(darwin_HOST_OS)
rtViewPlaform = "macOS"
#else
rtViewPlaform = "Unknown"
#endif

nodesVersions :: UI [UI Element]
nodesVersions =
  forM supportedNodesVersions $ \ver -> do
    element <$> UI.anchor #. [CommitLink]
                          # set UI.href ("https://github.com/input-output-hk/cardano-node/releases/tag/"
                               <> T.unpack ver)
                          # set UI.target "_blank"
                          # set UI.title__ ("See release tag " <> T.unpack ver <> " in cardano-node repository")
                          # set UI.text (T.unpack ver)

getPaths
  :: Configuration
  -> RTViewParams
  -> IO (FilePath, FilePath, FilePath, FilePath)
getPaths config params = do
  logsDir <-
    (find (\sd -> scKind sd == FileSK) <$> CM.getSetupScribes config) >>= \case
      Nothing -> logFilesDir
      Just sd -> return . takeDirectory . T.unpack . scName $ sd
  configFile <-
    if configFileIsProvided params
      then return $ rtvConfig params
      else savedConfigurationFile
  notificationsFile <-
    if notificationsFileIsProvided params
      then return $ rtvNotifications params
      else savedNotificationsFile
  paramsFile <- savedRTViewParamsFile
  return (configFile, notificationsFile, paramsFile, logsDir)

preparePathIfNeeded :: FilePath -> String
preparePathIfNeeded aPath = if tooLongPath then shortenedPath else aPath
 where
  tooLongPath = len > 20
  len = length aPath
  shortenedPath = take 10 aPath <> "..." <> drop (len - 10) aPath
  -}
