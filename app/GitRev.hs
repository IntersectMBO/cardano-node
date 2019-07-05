{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module GitRev (
      gitRev
    ) where

import           Data.FileEmbed (dummySpaceWith)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

import           GitRevFromGit (gitRevFromGit)

gitRev :: Text
gitRev | gitRevEmbed /= zeroRev = gitRevEmbed
       | T.null fromGit         = zeroRev
       | otherwise              = fromGit
    where
        -- Git revision embedded after compilation using
        -- Data.FileEmbed.injectWith. If nothing has been injected,
        -- this will be filled with 0 characters.
        gitRevEmbed :: Text
        gitRevEmbed = decodeUtf8 $(dummySpaceWith "gitrev" 40)

        -- Git revision found during compilation by running git. If
        -- git could not be run, then this will be empty.
        fromGit = T.strip (T.pack $(gitRevFromGit))

zeroRev :: Text
zeroRev = "0000000000000000000000000000000000000000"
