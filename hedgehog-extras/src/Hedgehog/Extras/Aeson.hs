{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Hedgehog.Extras.Aeson
  ( goldenTestJsonValue
  , goldenTestJsonValuePretty
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Bool
import           Data.Either
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Semigroup
import           GHC.Stack
import           Hedgehog
import           System.IO
import           Text.Show

import qualified Data.ByteString.Lazy as LBS
import qualified Hedgehog.Internal.Property as H

writeNewGoldFiles :: Bool
writeNewGoldFiles = False

strictComparison :: Bool
strictComparison = False

goldenTestJsonValue :: forall a. ()
  => Eq a
  => FromJSON a
  => Show a
  => ToJSON a
  => HasCallStack
  => a
  -> FilePath
  -> Property
goldenTestJsonValue x path = withFrozenCallStack $ withTests 1 . property $ do
  bs <- liftIO (LBS.readFile path)
  when writeNewGoldFiles $ liftIO . LBS.writeFile (path <> ".gold") $ encode x
  when strictComparison $ fmap encode (eitherDecode @a bs) === Right bs
  case eitherDecode bs of
    Left  err -> H.failWith Nothing $ "could not decode: " <> show err
    Right x'  -> x === x'

goldenTestJsonValuePretty
  :: forall a. ()
  => Eq a
  => FromJSON a
  => HasCallStack
  => Show a
  => ToJSON a
  => a
  -> FilePath
  -> Property
goldenTestJsonValuePretty x path =
  withFrozenCallStack
    $ withTests 1
    . property
    $ do
        bs <- liftIO (LBS.readFile path)
        -- Sort keys by their order of appearance in the argument list
        -- of `keyOrder`. Keys not in the argument list are moved to the
        -- end, while their order is preserved.
        let
          defConfig' = Config
            { confIndent          = Spaces 4
            , confCompare         = keyOrder ["file", "hash"]
            , confNumFormat       = Generic
            , confTrailingNewline = False
            }
        when writeNewGoldFiles $ liftIO . LBS.writeFile (path <> ".gold") $ encodePretty' defConfig' x
        when strictComparison $ fmap (encodePretty' defConfig') (eitherDecode @a bs) === Right bs
        case eitherDecode bs of
          Left  err -> H.failWith Nothing $ "could not decode: " <> show err
          Right x'  -> x === x'
