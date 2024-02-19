{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracer.Test.TestSetup
  ( module Cardano.Tracer.Test.TestSetup
  , module Ouroboros.Network.Magic
  )
where

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Control.Monad (join)
import           Data.Functor ((<&>))
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics (Generic)
import           Options.Applicative

import           Generic.Data (gmappend)


data TestSetup a
  = TestSetup
  { tsTime         :: !(a Double)
  , tsThreads      :: !(a Int)
  , tsMessages     :: !(a (Maybe Int))
  , tsSockInternal :: !(a FilePath)
  , tsSockExternal :: !(a FilePath)
  , tsNetworkMagic :: !(a NetworkMagic)
  , tsWorkDir      :: !(a FilePath)
  } deriving (Generic)
instance Semigroup (TestSetup Last) where
  (<>) = gmappend

deriving instance Show (TestSetup Identity)

parseTestSetup :: Parser (TestSetup Last)
parseTestSetup =
  TestSetup
  <$> (Last <$> optional (option auto (long "time"          <> metavar "SEC")))
  <*> (Last <$> optional (option auto (long "threads"       <> metavar "THRDS")))
  <*> (Last <$> optional (option auto (long "messages"      <> metavar "MSGS")))
  <*> (Last <$> optional (option auto (long "sock-internal" <> metavar "FILE")))
  <*> (Last <$> optional (option auto (long "sock-external" <> metavar "FILE")))
  <*> (Last <$> optional (option (NetworkMagic <$> auto)
                           (long "network-magic" <> metavar "INT")))
  <*> (Last <$> optional (option auto (long "workdir"       <> metavar "DIR")))

mergeTestSetup :: TestSetup Last -> TestSetup Identity
mergeTestSetup TestSetup{..} =
  TestSetup
  { tsTime         = get "Missing tsTime"         tsTime
  , tsThreads      = get "Missing tsThreads"      tsThreads
  , tsMessages     = Identity . join $ getLast    tsMessages
  , tsSockInternal = get "Missing tsSockInternal" tsSockInternal
  , tsSockExternal = get "Missing tsSockExternal" tsSockExternal
  , tsNetworkMagic = get "Missing tsNetworkMagic" tsNetworkMagic
  , tsWorkDir      = get "Missing tsWorkDir"      tsWorkDir
  }
 where
  get desc = Identity . fromMaybe (error $ "Missing " <> desc) . getLast

getTestSetup :: TestSetup Last -> IO (TestSetup Identity)
getTestSetup def =
  customExecParser
    (prefs showHelpOnEmpty)
    (info parseTestSetup mempty)
    <&> (def <>)
    <&> mergeTestSetup
