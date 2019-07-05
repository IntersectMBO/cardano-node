{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.CanonicalJSON
    ( canonicalEncPre, canonicalDecPre
    ) where

import qualified Data.ByteString.Lazy as LB
import qualified Text.JSON.Canonical as CanonicalJSON

import           Cardano.Prelude

{-------------------------------------------------------------------------------
  Stolen from: cardano-prelude/test/Test/Cardano/Prelude/Tripping.hs
-------------------------------------------------------------------------------}
canonicalEncPre
  :: forall a . CanonicalJSON.ToJSON Identity a => a -> LB.ByteString
canonicalEncPre x =
  LB.fromStrict
    . encodeUtf8
    . toS
    $ CanonicalJSON.prettyCanonicalJSON
    $ runIdentity
    $ CanonicalJSON.toJSON x

canonicalDecPre
  :: forall a
   . CanonicalJSON.FromJSON (Either SchemaError) a => LB.ByteString -> Either Text a
canonicalDecPre bs = do
  eVal <- first toS (CanonicalJSON.parseCanonicalJSON bs)
  first show (CanonicalJSON.fromJSON eVal :: Either SchemaError a)
