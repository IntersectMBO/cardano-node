{-# LANGUAGE ImportQualifiedPost #-}

module Cardano.Api.Memo where

import Prelude
import Data.IORef
import Data.Map qualified as Map
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE memoise #-}
memoise :: Ord a => (a -> b) -> (a -> b)
memoise f = unsafePerformIO $ do
  ref <- newIORef Map.empty
  pure $ \x -> unsafePerformIO $ do
    m <- readIORef ref
    case Map.lookup x m of
      Just y  -> pure y
      Nothing -> do
        let y = f x
        writeIORef ref (Map.insert x y m)
        pure y
