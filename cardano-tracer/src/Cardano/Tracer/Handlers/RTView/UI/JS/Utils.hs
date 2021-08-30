module Cardano.RTView.GUI.JS.Utils
    ( copyTextToClipboard
    , downloadCSVFile
    , goToTab
    ) where

copyTextToClipboard :: String
copyTextToClipboard = concat
  [ "const listener = function(ev) {"
  , "  ev.preventDefault();"
  , "  ev.clipboardData.setData('text/plain', %1);"
  , "};"
  , "document.addEventListener('copy', listener);"
  , "document.execCommand('copy');"
  , "document.removeEventListener('copy', listener);"
  ]

downloadCSVFile :: String
downloadCSVFile = concat
  [ "var element = document.createElement('a');"
  , "element.setAttribute('href', 'data:application/csv;charset=utf-8,' + %2);"
  , "element.setAttribute('download', %1);"
  , "element.style.display = 'none';"
  , "document.body.appendChild(element);"
  , "element.click();"
  , "document.body.removeChild(element);"
  ]

goToTab :: String
goToTab = concat
  [ "var element = document.getElementById(%1);"
  , "element.click();"
  ]
