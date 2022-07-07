{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.UI.Utils
  ( (##)
  , dataAction
  , dataParent
  , dataState
  , dataTooltip
  , findByClassAndDo
  , findAndDo
  , findAndSet
  , findAndSetHTML
  , findAndSetText
  , justCleanText
  , setTextValue
  , setTextValues
  , setTextAndClasses
  , findByClassAndSet
  , findAndAdd
  , findAndHide
  , findAndShow
  , findAndGetValue
  , image
  , showIt
  , showInline
  , showFlex
  , hideIt
  , pageTitle
  , pageTitleNotify
  , shortenName
  , shortenPath
  , setDisplayedValue
  , delete'
  , fadeInModal
  , exportErrorsToJSONFile
  ) where

import           Control.Monad (unless, void)
import           Control.Monad.Extra (whenJustM)
import           Data.String.QQ
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Foreign.JavaScript as JS
import qualified Foreign.RemotePtr as Foreign
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.JQuery (Easing (..), fadeIn, fadeOut)

import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Errors
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Types

(##) :: UI Element -> String -> UI Element
(##) el anId = el # set UI.id_ anId

findAndDo
  :: UI.Window
  -> Text
  -> (Element -> UI ())
  -> UI ()
findAndDo window elId =
  whenJustM (UI.getElementById window (unpack elId))

findByClassAndDo
  :: UI.Window
  -> Text
  -> (Element -> UI ())
  -> UI ()
findByClassAndDo window className doIt =
  UI.getElementsByClassName window (unpack className) >>= mapM_ doIt

findAndSet
  :: (UI Element -> UI Element)
  -> UI.Window
  -> Text
  -> UI ()
findAndSet doIt window elId =
  findAndDo window elId $ \el -> void $ element el # doIt

findAndSetHTML
  :: Text
  -> UI.Window
  -> Text
  -> UI ()
findAndSetHTML markup = findAndSet (set html $ unpack markup)

findAndSetText
  :: Text
  -> UI.Window
  -> Text
  -> UI ()
findAndSetText t = findAndSet (set text $ unpack t)

justCleanText :: Text -> UI ()
justCleanText elId =
  UI.runFunction $ UI.ffi setTextValue' elId T.empty

setTextValue :: Text -> Text -> UI ()
setTextValue elId textToSet =
  UI.runFunction $ UI.ffi setTextValue' elId textToSet

setTextValues :: [(Text, Text)] -> UI ()
setTextValues [] = return ()
setTextValues idsWithValues = UI.runFunction $ UI.ffi setAllValues
 where
  setAllValues = T.unpack . T.concat $ zipWith (curry setValue) [1 :: Int .. ] idsWithValues

  setValue (n, (elId, textToSet)) =
    let elN = T.pack $ show n in
    "var el" <> elN <> " = document.getElementById(\""
    <> elId
    <> "\"); if (el" <> elN <> " !== null) el" <> elN <> ".innerHTML = \""
    <> textToSet
    <> "\";"

setTextValue' :: String
setTextValue' = [s|
var el = document.getElementById(%1);
if (el !== null)
  el.innerHTML = %2;
|]

setTextAndClasses :: Text -> Text -> Text -> UI ()
setTextAndClasses elId textToSet classesToSet =
  UI.runFunction $ UI.ffi setTextAndClasses' elId textToSet classesToSet

setTextAndClasses' :: String
setTextAndClasses' = [s|
var el = document.getElementById(%1);
if (el !== null) {
  el.innerHTML = %2;
  el.className = %3;
}
|]

findAndAdd
  :: [UI Element]
  -> UI.Window
  -> Text
  -> UI ()
findAndAdd els window elId =
  findAndDo window elId $ \el -> void $ element el #+ els

findAndGetValue
  :: UI.Window
  -> Text
  -> UI String
findAndGetValue window elId =
  UI.getElementById window (unpack elId) >>= \case
    Nothing -> return ""
    Just el -> get value el

findByClassAndSet
  :: (UI Element -> UI Element)
  -> UI.Window
  -> Text
  -> UI ()
findByClassAndSet doIt window className =
  UI.getElementsByClassName window (unpack className)
  >>= mapM_ (\el -> void $ element el # doIt)

findAndShow, findAndHide
  :: UI.Window -> Text -> UI ()
findAndShow = findAndSet showIt
findAndHide = findAndSet hideIt

showIt
  , showInline
  , showFlex
  , hideIt :: UI Element -> UI Element
showIt     = set style [("display", "block")]
showInline = set style [("display", "inline")]
showFlex   = set style [("display", "flex")]
hideIt     = set style [("display", "none")]

pageTitle, pageTitleNotify :: String
pageTitle       = "Cardano RTView"
pageTitleNotify = "(!) Cardano RTView"

dataTooltip :: WriteAttr Element String
dataTooltip = mkWriteAttr $ set' (attr "data-tooltip")

dataAction :: Attr Element String
dataAction = dataAttr "action"

dataParent :: Attr Element String
dataParent = dataAttr "parent"

dataState :: Attr Element String
dataState = dataAttr "state"

dataAttr :: String -> Attr Element String
dataAttr name = mkReadWriteAttr getData setData
 where
  getData   el = callFunction $ ffi "$(%1).data(%2)" el name
  setData v el = runFunction  $ ffi "$(%1).data(%2,%3)" el name v

image :: String -> String -> UI Element
image imgClass svg = UI.span #. imgClass # set html svg

shortenPath :: FilePath -> FilePath
shortenPath p =
  if length p > 20
    then take 20 p <> "..."
    else p

shortenName :: Text -> Text
shortenName n =
  if T.length n > 20
    then T.take 20 n <> "..."
    else n

setDisplayedValue
  :: NodeId
  -> DisplayedElements
  -> Text
  -> Text
  -> UI ()
setDisplayedValue nodeId displayedElements elId mValue =
  liftIO (getDisplayedValue displayedElements nodeId elId) >>= \case
    Nothing        -> setAndRemember
    Just displayed -> unless (displayed == mValue) setAndRemember
 where
  setAndRemember = do
    setTextValue elId mValue
    liftIO $ saveDisplayedValue displayedElements nodeId elId mValue

-- | The function 'delete' from 'threepenny-gui' uses 'detach' jQuery-function,
--   but if we need to remove DOM-element completely, we have to use 'remove' instead.
delete' :: Element -> UI ()
delete' el = liftJSWindow $ \w -> do
  JS.runFunction w $ ffi "$(%1).remove()" el
  Foreign.destroy $ toJSObject el

fadeInModal :: Element -> UI ()
fadeInModal modal = do
  fadeOut modal 1 Swing $ return ()
  void $ element modal #. "modal is-active"
  fadeIn modal 150 Swing $ return ()

exportErrorsToJSONFile
  :: Errors
  -> NodeId
  -> Text
  -> UI ()
exportErrorsToJSONFile nodesErrors nodeId nodeName =
  whenJustM (liftIO $ errorsToJSON nodesErrors nodeId) $ \errorsAsJSON -> do
    now <- liftIO $ systemToUTCTime <$> getSystemTime
    let nowF = formatTime defaultTimeLocale "%FT%T%z" now
        fileName = "node-" <> unpack nodeName <> "-errors-" <> nowF <> ".json"
    downloadJSONFile fileName errorsAsJSON
