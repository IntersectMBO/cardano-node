{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hedgehog.Extras.Test.MonadAssertion
  ( MonadAssertion(..)
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Either
import           Data.Function
import           Data.Monoid (mempty)

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Resource as IO
import qualified Control.Monad.Trans.Resource.Internal as IO
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H

class Monad m => MonadAssertion m where
  throwAssertion :: H.Failure -> m a
  catchAssertion :: m a -> (H.Failure -> m a) -> m a

instance Monad m => MonadAssertion (H.TestT m) where
  throwAssertion f = H.liftTest $ H.mkTest (Left f, mempty)
  catchAssertion g h = H.TestT $ E.catchE (H.unTest g) (H.unTest . h)

instance MonadAssertion m => MonadAssertion (IO.ResourceT m) where
  throwAssertion = lift . throwAssertion
  catchAssertion r h = IO.ResourceT $ \i -> IO.unResourceT r i `catchAssertion` \e -> IO.unResourceT (h e) i

deriving instance Monad m => MonadAssertion (H.PropertyT m)
