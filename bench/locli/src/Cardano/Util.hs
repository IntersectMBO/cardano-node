module Cardano.Util
  ( module Cardano.Util
  , module Control.Arrow
  , module Control.Concurrent.Async
  , module Control.Monad.Trans.Except.Extra
  , module Text.Printf
  )
where

import Prelude                          (String)
import Cardano.Prelude

import Control.Arrow                    ((&&&), (***))
import Control.Concurrent.Async         (forConcurrently, forConcurrently_, mapConcurrently, mapConcurrently_)
import Control.DeepSeq                  qualified as DS
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Data.Aeson                       (FromJSON, ToJSON, encode, eitherDecode)
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        qualified as T
import Text.Printf                      (printf)

import System.FilePath                  qualified as F

import Cardano.Analysis.Ground


mapConcurrentlyPure :: NFData b => (a -> b) -> [a] -> IO [b]
mapConcurrentlyPure f xs =
  mapConcurrently
    (evaluate . DS.force . f)
    xs

mapAndUnzip :: (a -> (b, c)) -> [a] -> ([b], [c])
mapAndUnzip _ [] = ([], [])
mapAndUnzip f (x:xs)
  = let (r1,  r2)  = f x
        (rs1, rs2) = mapAndUnzip f xs
    in
    (r1:rs1, r2:rs2)

redistribute :: (a, (b, c)) -> ((a, b), (a, c))
redistribute    (a, (b, c))  = ((a, b), (a, c))

data F
  = R String
  | Q String
  | L [String]
  | forall a. ToJSON a => J a

progress :: MonadIO m => String -> F -> m ()
progress key = putStrLn . T.pack . \case
  R x  -> printf "{ \"%s\":  %s }"    key x
  Q x  -> printf "{ \"%s\": \"%s\" }" key x
  L xs -> printf "{ \"%s\": \"%s\" }" key (Cardano.Prelude.intercalate "\", \"" xs)
  J x  -> printf "{ \"%s\": %s }" key (LBS.unpack $ encode x)

-- Dumping to files
--
replaceExtension :: FilePath -> String -> FilePath
replaceExtension f new = F.dropExtension f <> "." <> new

dumpObject :: ToJSON a => String -> a -> JsonOutputFile -> ExceptT Text IO ()
dumpObject ident x (JsonOutputFile f) = liftIO $ do
  progress ident (Q f)
  withFile f WriteMode $ \hnd -> LBS.hPutStrLn hnd $ encode x

dumpObjects :: ToJSON a => String -> [a] -> JsonOutputFile -> ExceptT Text IO ()
dumpObjects ident xs (JsonOutputFile f) = liftIO $ do
  progress ident (Q f)
  withFile f WriteMode $ \hnd -> do
    forM_ xs $ LBS.hPutStrLn hnd . encode

dumpAssociatedObjects :: ToJSON a => String -> [(JsonLogfile, a)] -> ExceptT Text IO ()
dumpAssociatedObjects ident xs = liftIO $
  flip mapConcurrently_ xs $
    \(JsonLogfile f, x) -> do
        progress ident (Q f)
        withFile (replaceExtension f $ ident <> ".json") WriteMode $ \hnd ->
          LBS.hPutStrLn hnd $ encode x

readAssociatedObjects :: forall a.
  FromJSON a => String -> [JsonLogfile] -> ExceptT Text IO [(JsonLogfile, a)]
readAssociatedObjects ident fs = firstExceptT T.pack . newExceptT . fmap sequence . fmap (fmap sequence) $
  flip mapConcurrently fs $
    \jf@(JsonLogfile f) -> do
        x <- eitherDecode @a <$> LBS.readFile (replaceExtension f $ ident <> ".json")
        progress ident (Q f)
        pure (jf, x)

dumpAssociatedObjectStreams :: ToJSON a => String -> [(JsonLogfile, [a])] -> ExceptT Text IO ()
dumpAssociatedObjectStreams ident xss = liftIO $
  flip mapConcurrently_ xss $
    \(JsonLogfile f, xs) -> do
        progress ident (Q f)
        withFile (replaceExtension f $ ident <> ".json") WriteMode $ \hnd -> do
          forM_ xs $ LBS.hPutStrLn hnd . encode

dumpText :: String -> [Text] -> TextOutputFile -> ExceptT Text IO ()
dumpText ident xs (TextOutputFile f) = liftIO $ do
  progress ident (Q f)
  withFile f WriteMode $ \hnd -> do
    forM_ xs $ hPutStrLn hnd

dumpAssociatedTextStreams :: String -> [(JsonLogfile, [Text])] -> ExceptT Text IO ()
dumpAssociatedTextStreams ident xss = liftIO $
  flip mapConcurrently_ xss $
    \(JsonLogfile f, xs) -> do
        progress ident (Q f)
        withFile (replaceExtension f $ ident <> ".txt") WriteMode $ \hnd -> do
          forM_ xs $ hPutStrLn hnd
