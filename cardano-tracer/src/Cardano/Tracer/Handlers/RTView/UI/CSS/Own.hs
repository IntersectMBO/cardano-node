{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.UI.CSS.Own
  ( ownCSS
  , chartGridDark
  , chartGridLight
  , chartTextDark
  , chartTextLight
  ) where

import           Data.String.QQ

-- | To avoid run-time dependency from the static content, embed own CSS in the page's header.
ownCSS :: String
ownCSS = [s|
html {
  height: 100%;
}

body {
  height: 100%;
}

.wrapper {
  min-height: 100%;
  width: 100%;
  position: relative;
  padding-bottom: 62px;
  box-sizing: border-box;
}

.footer {
  height: 62px;
  bottom: 0;
  position: absolute;
  width: 100%;
}

code {
  color: #1d359f;
  padding: 0.11em 0.2em 0.11em;
  border-radius: 3px;
}

pre {
  color: #1d359f;
  font-size: .875em;
  overflow-x: auto;
  padding: 8px 8px 8px 8px;
  margin-top: 12px;
  margin-bottom: 12px;
  white-space: pre;
  word-wrap: normal;
  border-radius: 3px;
}

.message.is-link .message-body {
  border-color: #485fc7;
  color: #1834ae;
}

span[data-tooltip] {
  border-bottom: none !important;
}

.pageloader {
  opacity: 0.98 !important;
}

.field.is-grouped {
  display: inline-flex !important;
}

.divider {
  font-size: 16px;
  letter-spacing: .5px;
  margin: 30px 0;
}

.rt-view-show-hide-chart-group {
  margin-top: 5px;
}

.rt-view-no-nodes-icon svg {
  width: 70px;
  margin-top: 60px;
  margin-bottom: 40px;
}

.rt-view-no-nodes-info {
  max-width: 800px !important;
  margin-top: 50px;
  font-size: 97%;
}

.rt-view-chart-area {
  width: 100% !important;
}

.rt-view-main-table-description {
  min-width: 380px;
}

.rt-view-node-chart-label svg {
  width: 42px;
  height: 15px;
  margin-right: 3px;
}

.rt-view-peer-modal {
  width: 45%;
}

.rt-view-errors-modal {
  width: 65%;
  min-height: 65%;
}

.rt-view-ekg-metrics-modal {
  width: 50%;
}

.rt-view-divider {
  margin-top: 5px;
  margin-bottom: 13px;
}

@media only screen and (max-width: 1216px) {
  .rt-view-peer-modal {
    width: 60%;
  }

  .rt-view-errors-modal {
    width: 70%;
  }

  .rt-view-ekg-metrics-modal {
    width: 60%;
  }
}

@media only screen and (max-width: 1024px) {
  .rt-view-peer-modal {
    width: 70%;
  }

  .rt-view-errors-modal {
    width: 75%;
  }

  .rt-view-ekg-metrics-modal {
    width: 70%;
  }
}

@media screen and (min-width: 769px), print {
  .field-label {
    flex-grow: 1.5;
  }
}

@media only screen and (max-width: 769px) {
  .rt-view-peer-modal {
    width: 80%;
  }

  .rt-view-errors-modal {
    width: 85%;
  }

  .rt-view-ekg-metrics-modal {
    width: 80%;
  }
}

.rt-view-notifications-settings {
}

.rt-view-logs-input {
  max-width: 150px;
}

.rt-view-error-msg-input {
  max-width: 380px;
}

.rt-view-errors-timestamp {
  width: 30%;
}

.rt-view-errors-severity {
  width: 16%;
}

.rt-view-copy-icon {
  margin-top: 6px;
}

.rt-view-show-hide-pass-icon {
  margin-top: 6px;
}

.rt-view-search-errors-icon {
  margin-top: 6px;
}

.rt-view-search-errors-icon svg {
  width: 18px;
  color: whitesmoke;
}

.rt-view-notifications-errors-select-wrapper {
  padding-top: 7px;
  padding-bottom: 11px;
  float: right;
  clear: right;
}

/* Dark Theme */

.dark {
  font-family: sans-serif;
  font-size: 22px;
  background-color: #131325;
  min-height: 100%;
}

.dark .wrapper {
  background-color: #131325;
}

.dark .rt-view-href {
  color: #607bf7;
}

.dark .rt-view-href:hover {
  color: #889cf5 !important;
  border-bottom: 1px solid #889cf5;
}

.dark .navbar-link, a.navbar-item {
  font-size: 18px;
  cursor: pointer;
}

@media screen and (min-width: 1024px) {
  .dark .navbar-item.has-dropdown:hover .navbar-link {
    background-color: #434360;
  }

  .dark .navbar-dropdown {
    background-color: #434360;
    border-top: 1px solid #aaa;
  }
}

.dark .navbar-dropdown.is-right .navbar-item {
  background-color: #434360;
  color: #cecece;
}

.dark .navbar-dropdown.is-right a.navbar-item:hover {
  background-color: #545478;
}

.dark .rt-view-href-icon svg {
  width: 12px;
  margin-left: 5px;
  margin-bottom: 4px;
  color: #607bf7;
}

.dark .rt-view-top-bar {
  background-color: #282841;
  color: whitesmoke;
  padding-top: 8px;
  padding-bottom: 2px;
  border-bottom: 1px solid #555;
}

.dark .rt-view-cardano-logo svg {
  width: 48px;
  color: whitesmoke;
  margin-left: 5px;
}

.dark .rt-view-name {
  color: whitesmoke;
  margin-left: 17px;
  margin-right: 6px;
  margin-bottom: 6px;
}

.dark .rt-view-info-icon svg {
  width: 25px;
  padding-top: 2px;
  color: whitesmoke;
  cursor: pointer;
}

.dark .rt-view-theme-icon svg {
  width: 23px;
  padding-top: 2px;
  color: whitesmoke;
  cursor: pointer;
}

.dark .rt-view-copy-icon svg {
  width: 20px;
  color: whitesmoke;
  cursor: pointer;
}

.dark .rt-view-copy-icon-on-button svg {
  width: 20px;
  color: whitesmoke;
  margin-top: 5px;
}

.dark .rt-view-show-hide-pass-icon svg {
  width: 20px;
  cursor: pointer;
}

.dark .rt-view-sort-icon svg {
  width: 11px;
  margin-left: 9px;
  color: whitesmoke;
  cursor: pointer;
}

.dark .rt-view-delete-icon svg {
  width: 20px;
  color: red;
  cursor: pointer;
}

.dark .rt-view-delete-errors-icon svg {
  width: 22px;
  color: red;
  cursor: pointer;
}

.dark .rt-view-export-icon svg {
  width: 29px;
  margin-top: 5px;
  margin-right: 5px;
  color: whitesmoke;
  cursor: pointer;
}

.dark .rt-view-logs-icon svg {
  width: 23px;
  padding-top: 2px;
  color: whitesmoke;
}

.dark .rt-view-overview-icon svg {
  width: 18px;
  margin-right: 12px;
  color: #0cc9cb;
}

.dark .rt-view-no-nodes-icon svg {
  color: #677deb;
}

.dark .rt-view-no-nodes-progress {
  margin-top: 34px;
  max-width: 360px;
  margin-left: auto;
  margin-right: auto;
}

.dark .rt-view-notify-menu-icon svg {
  width: 15px;
  padding-top: 6px;
  margin-right: 9px;
  color: whitesmoke;
}

.dark .rt-view-no-nodes-message {
  font-size: 23px;
  color: whitesmoke;
  text-align: center;
}

.dark .rt-view-charts-container {
  padding-bottom: 0px;
}

.dark .rt-view-chart-container {
  background-color: #2c2b3b;
  padding-top: 10px;
  padding-bottom: 20px;
  padding-left: 20px;
  padding-right: 20px;
  margin-bottom: 24px;
  border: 1px solid #444;
  border-radius: 6px;
}

.dark .rt-view-chart-name {
  color: whitesmoke;
}

.dark .rt-view-about-title {
  color: whitesmoke;
}

.dark .rt-view-about-head {
  color: whitesmoke;
  background-color: #282841;
  border-bottom: 1px solid #555;
}

.dark .rt-view-about-body {
  color: whitesmoke;
  background-color: #131325;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.dark .rt-view-peer-title {
  color: whitesmoke;
}

.dark .rt-view-peer-head {
  color: whitesmoke;
  background-color: #282841;
  border-bottom: 1px solid #555;
}

.dark .rt-view-peer-body {
  color: whitesmoke;
  background-color: #131325;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.dark .rt-view-errors-title {
  color: whitesmoke;
}

.dark .rt-view-errors-head {
  color: whitesmoke;
  background-color: #282841;
  border-bottom: 1px solid #555;
}

.dark .rt-view-errors-body {
  color: whitesmoke;
  background-color: #131325;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.dark .rt-view-errors-foot {
  color: whitesmoke;
  background-color: #282841;
  border-top: 1px solid #555;
  display: block;
}

.dark .rt-view-notification-settings-foot {
  color: whitesmoke;
  background-color: #282841;
  border-top: 1px solid #555;
  display: block;
}

.dark .rt-view-notifications-title {
  color: whitesmoke;
}

.dark .rt-view-notifications-head {
  color: whitesmoke;
  background-color: #282841;
  border-bottom: 1px solid #555;
}

.dark .rt-view-notifications-body {
  color: whitesmoke;
  background-color: #131325;
}

.dark .rt-view-ekg-metrics-title {
  color: whitesmoke;
}

.dark .rt-view-ekg-metrics-head {
  color: whitesmoke;
  background-color: #282841;
  border-bottom: 1px solid #555;
}

.dark .rt-view-ekg-metrics-body {
  color: whitesmoke;
  background-color: #131325;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.dark .rt-view-main-table {
  background-color: #131325;
  color: whitesmoke;
}

.dark .rt-view-main-table td {
  padding-top: 17px;
  padding-bottom: 17px;
  border-bottom: 0px solid #444;
}

.dark .rt-view-main-table th {
  color: whitesmoke;
  border-bottom: 2px solid #888;
  vertical-align: middle;
}

.dark .rt-view-peer-table {
  background-color: #131325;
  color: whitesmoke;
}

.dark .rt-view-peer-table td {
  padding-top: 10px;
  padding-bottom: 10px;
  border-bottom: 0px solid #444;
}

.dark .rt-view-peer-table th {
  color: whitesmoke;
  border-bottom: 2px solid #888;
  vertical-align: middle;
}

.dark .rt-view-errors-table {
  background-color: #131325;
  color: whitesmoke;
}

.dark .rt-view-errors-table td {
  padding-top: 10px;
  padding-bottom: 10px;
  border-bottom: 0px solid #444;
}

.dark .rt-view-errors-table th {
  color: whitesmoke;
  border-bottom: 2px solid #888;
  vertical-align: middle;
}

.dark .rt-view-chart-group-title {
  color: whitesmoke;
  font-weight: bold;
  margin-left: 13px;
  font-size: 115%;
}

.dark .rt-view-events-title {
  color: whitesmoke;
  font-size: 105%;
}

.dark .rt-view-show-hide-chart-group svg {
  width: 21px;
  color: #0cc9cb;
}

.dark .rt-view-chart-icon svg {
  width: 19px;
  margin-right: 11px;
  color: #0cc9cb;
}

.dark .rt-view-footer {
  background-color: #282841;
  color: #999;
  padding-top: 16px;
  padding-bottom: 12px;
  border-top: 1px solid #555;
  font-size: 80%;
}

.dark .rt-view-footer-github svg {
  width: 23px;
  margin-top: 1px;
  color: #0cc9cb;
}

.dark .rt-view-footer-doc svg {
  width: 20px;
  margin-top: 1px;
  margin-left: 15px;
  color: #0cc9cb;
}

.dark .rt-view-percent-done {
  color: #07e949;
}

.dark .rt-view-what-icon svg {
  width: 18px;
  margin-left: 12px;
  color: #999;
}

.dark .rt-view-period-what-icon svg {
  width: 18px;
  margin-left: 8px;
  margin-top: 5px;
  color: #999;
}

.dark .rt-view-epoch-end svg {
  width: 16px;
  margin-left: 20px;
  margin-right: 5px;
  color: #0cc9cb;
}

.dark .rt-view-email-only {
  color: orange;
  font-size: 95%;
  margin-bottom: 21px;
}

.dark .rt-view-label {
  font-size: 80%;
  font-weight: 600;
  color: whitesmoke;
}

.dark .rt-view-test-status-message-ok {
  color: #48ee66;
}

.dark .rt-view-test-status-message-fail {
  color: #f7967f;
}

/********************* Light Theme *********************/

.light {
  font-family: sans-serif;
  font-size: 22px;
  background-color: #f5f5f5;
  min-height: 100%;
}

.light .wrapper {
  background-color: #f5f5f5;
}

.light .rt-view-href {
  color: #264af0;
}

.light .rt-view-href:hover {
  color: #889cf5 !important;
  border-bottom: 1px solid #889cf5;
}

.light .rt-view-href-icon svg {
  width: 12px;
  margin-left: 5px;
  margin-bottom: 4px;
  color: #264af0;
}

.light .navbar-link, a.navbar-item {
  font-size: 18px;
  cursor: pointer;
}

.light .rt-view-top-bar {
  background-color: #efefef;
  color: #131325;
  padding-top: 8px;
  padding-bottom: 2px;
  border-bottom: 1px solid #dbdbdb;
}

.light .rt-view-cardano-logo svg {
  width: 48px;
  color: #0033ad;
  margin-left: 5px;
}

.light .rt-view-name {
  color: #0033ad;
  margin-left: 17px;
  margin-right: 6px;
  margin-bottom: 6px;
}

.light .rt-view-info-icon svg {
  width: 25px;
  padding-top: 2px;
  color: #0033ad;
  cursor: pointer;
}

.light .rt-view-theme-icon svg {
  width: 23px;
  padding-top: 2px;
  color: #0033ad;
  cursor: pointer;
}

.light .rt-view-copy-icon svg {
  width: 20px;
  color: #444;
  cursor: pointer;
}

.light .rt-view-copy-icon-on-button svg {
  width: 20px;
  color: whitesmoke;
  margin-top: 5px;
}

.light .rt-view-show-hide-pass-icon svg {
  width: 20px;
  cursor: pointer;
}

.light .rt-view-sort-icon svg {
  width: 11px;
  margin-left: 9px;
  color: #444;
  cursor: pointer;
}

.light .rt-view-delete-icon svg {
  width: 20px;
  color: red;
  cursor: pointer;
}

.light .rt-view-delete-errors-icon svg {
  width: 22px;
  color: red;
  cursor: pointer;
}

.light .rt-view-export-icon svg {
  width: 29px;
  margin-top: 5px;
  margin-right: 5px;
  color: #0033ad;
  cursor: pointer;
}

.light .rt-view-logs-icon svg {
  width: 23px;
  padding-top: 2px;
  color: #0033ad;
}

.light .rt-view-overview-icon svg {
  width: 18px;
  margin-right: 12px;
  color: #038b8c;
}

.light .rt-view-no-nodes-icon svg {
  color: #0033ad;
}

.light .rt-view-no-nodes-message {
  font-size: 23px;
  color: #0033ad;
  text-align: center;
}

.light .rt-view-no-nodes-progress {
  margin-top: 34px;
  max-width: 360px;
  margin-left: auto;
  margin-right: auto;
}

.light .rt-view-charts-container {
  padding-bottom: 0px;
}

.light .rt-view-chart-container {
  background-color: #eeeeee;
  padding-top: 10px;
  padding-bottom: 20px;
  padding-left: 20px;
  padding-right: 20px;
  margin-bottom: 24px;
  border: 1px solid #dddddd;
  border-radius: 6px;
}

.light .rt-view-chart-name {
  color: #444;
}

.light .rt-view-about-title {
  color: #444;
}

.light .rt-view-about-head {
  color: whitesmoke;
  background-color: whitesmoke;
  border-bottom: 1px solid #bebebe;
}

.light .rt-view-about-body {
  color: #555;
  background-color: #eaeaea;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.light .rt-view-peer-title {
  color: #444;
}

.light .rt-view-peer-head {
  color: whitesmoke;
  background-color: whitesmoke;
  border-bottom: 1px solid #bebebe;
}

.light .rt-view-peer-body {
  color: #555;
  background-color: #eaeaea;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.light .rt-view-errors-title {
  color: #444;
}

.light .rt-view-errors-head {
  color: #555;
  background-color: whitesmoke;
  border-bottom: 1px solid #bebebe;
}

.light .rt-view-errors-body {
  color: #555;
  background-color: #eaeaea;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.light .rt-view-errors-foot {
  color: #555;
  background-color: whitesmoke;
  border-top: 1px solid #bebebe;
  display: block;
}

.light .rt-view-notification-settings-foot {
  color: #555;
  background-color: whitesmoke;
  border-top: 1px solid #bebebe;
  display: block;
}

.light .rt-view-notifications-title {
  color: #444;
}

.light .rt-view-notifications-head {
  color: whitesmoke;
  background-color: whitesmoke;
  border-bottom: 1px solid #bebebe;
}

.light .rt-view-notifications-body {
  color: #555;
  background-color: #eaeaea;
}

.light .rt-view-ekg-metrics-title {
  color: #444;
}

.light .rt-view-ekg-metrics-head {
  color: whitesmoke;
  background-color: whitesmoke;
  border-bottom: 1px solid #bebebe;
}

.light .rt-view-ekg-metrics-body {
  color: #555;
  background-color: #eaeaea;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.light .rt-view-main-table {
  background-color: #f5f5f5;
  color: #444;
}

.light .rt-view-main-table td {
  padding-top: 17px;
  padding-bottom: 17px;
  border-bottom: 0px solid #444;
}

.light .rt-view-main-table th {
  color: #444;
  border-bottom: 2px solid #cfcfcf;
  vertical-align: middle;
}

.light .rt-view-peer-table {
  background-color: #eaeaea;
  color: #444;
}

.light .rt-view-peer-table td {
  padding-top: 10px;
  padding-bottom: 10px;
  border-bottom: 0px solid #444;
}

.light .rt-view-peer-table th {
  color: #444;
  border-bottom: 2px solid #cfcfcf;
  vertical-align: middle;
}

.light .rt-view-errors-table {
  background-color: #eaeaea;
  color: #444;
}

.light .rt-view-errors-table td {
  padding-top: 10px;
  padding-bottom: 10px;
  border-bottom: 0px solid #444;
}

.light .rt-view-errors-table th {
  color: #444;
  border-bottom: 2px solid #cfcfcf;
  vertical-align: middle;
}

.light .rt-view-chart-group-title {
  color: #444;
  font-weight: bold;
  margin-left: 13px;
  font-size: 110%;
}

.light .rt-view-events-title {
  color: #555;
  font-size: 105%;
}

.light .rt-view-show-hide-chart-group svg {
  width: 21px;
  color: #038b8c;
}

.light .rt-view-chart-icon svg {
  width: 19px;
  margin-right: 11px;
  color: #038b8c;
}

.light .rt-view-footer {
  background-color: #efefef;
  color: #777;
  padding-top: 16px;
  padding-bottom: 12px;
  border-top: 1px solid #dbdbdb;
  font-size: 80%;
}

.light .rt-view-footer-github svg {
  width: 23px;
  margin-top: 1px;
  color: #038b8c;
}

.light .rt-view-footer-doc svg {
  width: 20px;
  margin-top: 1px;
  margin-left: 15px;
  color: #038b8c;
}

.light .rt-view-percent-done {
  color: #048b04;
}

.light .rt-view-what-icon svg {
  width: 18px;
  margin-left: 12px;
  color: #9a9a9a;
}

.light .rt-view-period-what-icon svg {
  width: 18px;
  margin-left: 8px;
  margin-top: 5px;
  color: #9a9a9a;
}

.light .rt-view-epoch-end svg {
  width: 16px;
  margin-left: 20px;
  margin-right: 5px;
  color: #038b8c;
}

.light .rt-view-notify-menu-icon svg {
  width: 15px;
  padding-top: 6px;
  margin-right: 9px;
  color: #0033ad;
}

.light .rt-view-email-only {
  color: #d68c04;
  font-size: 95%;
  margin-bottom: 21px;
}

.light .rt-view-label {
  font-size: 80%;
  font-weight: 600;
  color: #555;
}

.light .rt-view-test-status-message-ok {
  color: #0b9223;
}

.light .rt-view-test-status-message-fail {
  color: #e6380d;
}
|]

chartTextLight
  , chartTextDark
  , chartGridDark
  , chartGridLight :: String
chartGridDark  = "#ccc"
chartGridLight = "#555"
chartTextDark  = "#555"
chartTextLight = "#ddd"
