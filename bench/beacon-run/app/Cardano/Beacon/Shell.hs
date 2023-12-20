{-# LANGUAGE TemplateHaskell #-}

module Cardano.Beacon.Shell where

import Shh



$(load SearchPath "nix")
$(load SearchPath "curl")
$(load SearchPath "jq")


curlGitHubAPI :: _ -> _
curlGitHubAPI queryPath =
    curl 
        "-L" 
        "-H \"Accept: application/vnd.github+json\""
        "-H \"X-GitHub-Api-Version: 2022-11-28\""
        ("https://api.github.com" <> queryPath)
    |> captureTrim
