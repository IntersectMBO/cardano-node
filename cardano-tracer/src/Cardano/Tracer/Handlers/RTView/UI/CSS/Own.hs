{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.UI.CSS.Own
  ( ownCSS
  ) where

import           Data.String.QQ

-- | To avoid run-time dependency from the static content, embed own CSS in the page's header.
ownCSS :: String
ownCSS = [s|
html {
  height: 100%;
}

body {
  font-family: sans-serif;
  font-size: 20px;
  color: #1b2238;
  background-color: #131325;
  min-height: 100%;
}

.pageloader {
  background: #2c2b3b !important;
  opacity: 0.95 !important;
}

.rt-view-href {
  color: #607bf7 !important;
}

.rt-view-href:hover {
  color: #889cf5 !important;
  border-bottom: 1px solid #889cf5;
}

span[data-tooltip] {
  border-bottom: none !important;
}

.rt-view-top-bar {
  background-color: #131325;
  color: whitesmoke;
  padding-top: 8px;
  padding-bottom: 2px;
  border-bottom: 1px solid #888;
}

.rt-view-own-info-box {
  background-color: #2c2b3b !important;
  color: whitesmoke !important;
}

.rt-view-cardano-logo svg {
  width: 48px;
  color: whitesmoke;
  margin-left: 5px;
}

.rt-view-name {
  color: whitesmoke;
  margin-left: 12px;
  margin-right: 6px;
  margin-bottom: 6px;
}

.rt-view-info-icon svg {
  width: 25px;
  padding-top: 2px;
  color: whitesmoke;
  cursor: pointer;
}

.rt-view-notify-icon svg {
  width: 23px;
  padding-top: 2px;
  color: whitesmoke;
  cursor: pointer;
}

.rt-view-what-icon {
  text-align: left !important;
}

.rt-view-what-icon svg {
  width: 16px;
  margin-left: 8px;
  color: #aaa;
  cursor: pointer;
}

.rt-view-overview-icon svg {
  width: 16px;
  margin-right: 10px;
  color: #1fc1c3;
}

.rt-view-node-tab-icon svg {
  width: 16px;
  margin-right: 5px;
  color: #1fc1c3;
  max-height: 16px;
  object-fit: contain;
}

.rt-view-ada-node-icon {
  color: #1fc1c3;
}

.rt-view-node-panel {
  background-color: #dedede;
}

.rt-view-node-panel-down svg {
  width: 13px;
  margin-top: 4px;
  margin-right: 3px;
  cursor: pointer;
}

.rt-view-node-panel-block {
  padding: 5px 20px 0px 20px;
  height: 300px;
  background-color: #2c2b3b;
  color: #ccc;
}

.rt-view-node-panel-head {
  background-color: #454563;
  color: whitesmoke;
}

.rt-view-node-name {
  font-weight: normal !important;
}

.rt-view-node-name-column {
  margin-top: 2px;
}

.rt-view-node-panel-tabs {
  font-size: 20px;
  background-color: #2c2b3b;
  margin-bottom: 0px !important;
}

.tabs a {
  color: #888;
}

.tabs ul {
  border-bottom-width: 0px;
}

.tabs a:hover {
  border-bottom: 1px solid white;
  color: #aaa;
}

.tabs li.is-active a {
  border-bottom: 4px solid white;
  color: white;
}

.panel-block:not(:last-child), .panel-tabs:not(:last-child) {
  border-bottom: 0px solid #ededed;
}

.rt-view-node-panel-cols {
  width: 100%;
}

.rt-view-no-nodes-icon svg {
  width: 70px;
  margin-top: 25px;
  margin-bottom: 44px;
  color: #677deb;
}

.rt-view-no-nodes-message {
  font-size: 22px;
  color: whitesmoke;
}

.rt-view-vspace-with-hr {

}

.table thead th {
  color: whitesmoke;
  font-weight: normal;
}

.rt-view-peers-table-container {
  margin-left: 15px;
  margin-right: 15px;
}

.rt-view-peers-table {
  width: 100%;
  background-color: #2c2b3b;
  color: whitesmoke;
  font-size: 19px;
}
|]
