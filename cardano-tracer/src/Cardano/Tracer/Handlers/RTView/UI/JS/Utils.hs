{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.UI.JS.Utils
  ( closeModalsByEscapeButton
  , copyTextToClipboard
  , downloadJSONFile
  , selectOption
  ) where

import           Data.String.QQ
import           Data.Text (Text)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

copyTextToClipboard :: String -> UI ()
copyTextToClipboard textToCopy =
  UI.runFunction $ UI.ffi copyTextToClipboard' textToCopy

copyTextToClipboard' :: String
copyTextToClipboard' = [s|
const listener = function(ev) {
  ev.preventDefault();
  ev.clipboardData.setData('text/plain', %1);
};
document.addEventListener('copy', listener);
document.execCommand('copy');
document.removeEventListener('copy', listener);
|]

downloadJSONFile
  :: FilePath
  -> Text
  -> UI ()
downloadJSONFile jsonFileName textToExport =
  UI.runFunction $ UI.ffi downloadJSONFile' jsonFileName textToExport

downloadJSONFile' :: String
downloadJSONFile' = [s|
var element = document.createElement('a');
element.setAttribute('href', 'data:application/json;charset=utf-8,' + encodeURIComponent(%2));
element.setAttribute('download', %1);
element.style.display = 'none';
document.body.appendChild(element);
element.click();
document.body.removeChild(element);
|]

selectOption
  :: String
  -> Int
  -> UI ()
selectOption selectId optionValue =
  UI.runFunction $ UI.ffi "document.getElementById(%1).value = %2;" selectId optionValue

closeModalsByEscapeButton :: UI ()
closeModalsByEscapeButton =
  UI.runFunction $ UI.ffi closeModalsByEscapeButton'

closeModalsByEscapeButton' :: String
closeModalsByEscapeButton' = [s|
$(document).keydown(function(event) {
  if (event.keyCode == 27) {
    var modals = document.getElementsByClassName("modal is-active");
    for (var i=0; i < modals.length; i++) {
      modals[i].className = "modal";
    }
  }
});
|]
