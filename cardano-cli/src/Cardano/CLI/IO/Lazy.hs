{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.IO.Lazy
  ( replicateM
  , sequenceM
  , traverseM
  , traverseStateM
  , forM
  , forStateM
  ) where

import           Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO, UnliftIO (unliftIO),
                   askUnliftIO)

import qualified Data.List as L
import qualified System.IO.Unsafe as IO

replicateM :: MonadUnliftIO m => Int -> m a -> m [a]
replicateM n f = sequenceM (L.replicate n f)

sequenceM :: MonadUnliftIO m => [m a] -> m [a]
sequenceM as = do
  f <- askUnliftIO
  liftIO $ sequenceIO (L.map (unliftIO f) as)

-- | Traverses the function over the list and produces a lazy list in a
-- monadic context.
--
-- It is intended to be like the "standard" 'traverse' except
-- that the list is generated lazily.
traverseM :: MonadUnliftIO m => (a -> m b) -> [a] -> m [b]
traverseM f as = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go u as)
  where
    go _ [] = pure []
    go !u (v:vs) = do
      !res <- unliftIO u (f v)
      rest <- IO.unsafeInterleaveIO (go u vs)
      pure (res:rest)

traverseStateM :: forall m s a b. MonadUnliftIO m => s -> (s -> a -> m (s, b)) -> [a] -> m [b]
traverseStateM s f as = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go s u as)
  where
    go :: s -> UnliftIO m -> [a] -> IO [b]
    go _ _ [] = pure []
    go t !u (v:vs) = do
      (t', !res) <- unliftIO u (f t v)
      rest <- IO.unsafeInterleaveIO (go t' u vs)
      pure (res:rest)

forM :: MonadUnliftIO m => [a] -> (a -> m b) -> m [b]
forM = flip traverseM

forStateM :: MonadUnliftIO m => s -> [a] -> (s -> a -> m (s, b)) -> m [b]
forStateM s as f = traverseStateM s f as

-- Internal
sequenceIO :: [IO a] -> IO [a]
sequenceIO = IO.unsafeInterleaveIO . go
  where go :: [IO a] -> IO [a]
        go []       = return []
        go (fa:fas) = (:) <$> fa <*> IO.unsafeInterleaveIO (go fas)
