module Cardano.Tracer.Handlers.RTView.UI.Utils
  ( (##)
  , dataTooltip
  , findAndDo
  , findAndSet
  , findAndHide
  , findAndShow
  , image
  , showIt
  , showInline
  , hideIt
  , pageTitle
  , pageTitleNotify
  , whatIsItImage
  ) where

import           Data.Text (Text, unpack)
import           Control.Monad (void)
import           Control.Monad.Extra (whenJustM)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons

(##) :: UI Element -> String -> UI Element
(##) el anId = el # set UI.id_ anId

findAndDo
  :: UI.Window
  -> Text
  -> (Element -> UI ())
  -> UI ()
findAndDo window elId =
  whenJustM (UI.getElementById window (unpack elId))

findAndSet
  :: (UI Element -> UI Element)
  -> UI.Window
  -> Text
  -> UI ()
findAndSet doIt window elId =
  findAndDo window elId $ \el -> void $ element el # doIt

findAndShow, findAndHide
  :: UI.Window -> Text -> UI ()
findAndShow = findAndSet showIt
findAndHide = findAndSet hideIt

showIt, showInline, hideIt :: UI Element -> UI Element
showIt     = set style [("display", "block")]
showInline = set style [("display", "inline")]
hideIt     = set style [("display", "none")]

pageTitle, pageTitleNotify :: String
pageTitle       = "Cardano RTView"
pageTitleNotify = "(!) Cardano RTView"

dataTooltip :: WriteAttr Element String
dataTooltip = mkWriteAttr $ set' (attr "data-tooltip")

image :: String -> String -> UI Element
image imgClass svg = UI.span #. imgClass # set html svg

whatIsItImage :: String -> UI Element
whatIsItImage tooltip =
  image "rt-view-what-icon has-tooltip-multiline has-tooltip-right" questionSVG
        # set dataTooltip tooltip
