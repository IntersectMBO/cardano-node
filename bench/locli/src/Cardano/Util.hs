module Cardano.Util
  ( module Prelude
  , module Cardano.Util
  , module Cardano.Ledger.BaseTypes
  , module Control.Arrow
  , module Control.Applicative
  , module Control.Concurrent.Async
  , module Control.Monad.Trans.Except.Extra
  , module Text.Printf
  )
where

import Prelude                          (String)
import Cardano.Prelude

import Control.Arrow                    ((&&&), (***))
import Control.Applicative              ((<|>))
import Control.Concurrent.Async         (forConcurrently, forConcurrently_, mapConcurrently, mapConcurrently_)
import Control.DeepSeq                  qualified as DS
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import Data.Aeson                       (FromJSON, ToJSON, encode, eitherDecode)
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.List                        (span)
import Data.Text                        qualified as T
import Data.Text.Short                  (fromText)
import Data.Vector                      (Vector)
import Data.Vector                      qualified as Vec
import GHC.Base                         (build)
import Text.Printf                      (printf)

import System.FilePath                  qualified as F

import Cardano.Analysis.Ground
import Cardano.Ledger.BaseTypes         (StrictMaybe (..), fromSMaybe)


smaybe :: b -> (a -> b) -> StrictMaybe a -> b
smaybe x _  SNothing = x
smaybe _ f (SJust x) = f x

mapSMaybe          :: (a -> StrictMaybe b) -> [a] -> [b]
mapSMaybe _ []     = []
mapSMaybe f (x:xs) =
 let rs = mapSMaybe f xs in
 case f x of
  SNothing -> rs
  SJust r  -> r:rs
{-# NOINLINE [1] mapSMaybe #-}

{-# RULES
"mapSMaybe"     [~1] forall f xs. mapSMaybe f xs
                     = build (\c n -> foldr (mapSMaybeFB c f) n xs)
  #-}

{-# INLINE [0] mapSMaybeFB #-} -- See Note [Inline FB functions] in GHC.List
mapSMaybeFB :: (b -> r -> r) -> (a -> StrictMaybe b) -> a -> r -> r
mapSMaybeFB cons f x next = case f x of
  SNothing -> next
  SJust r -> cons r next

mapConcurrentlyPure :: NFData b => (a -> b) -> [a] -> IO [b]
mapConcurrentlyPure f =
  mapConcurrently
    (evaluate . DS.force . f)

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

-- /path/to/logs-HOSTNAME.some.ext -> HOSTNAME
hostFromLogfilename :: JsonLogfile -> Host
hostFromLogfilename (JsonLogfile f) =
  Host $ fromText . stripPrefixHard "logs-" . T.pack . F.dropExtensions . F.takeFileName $ f
 where
   stripPrefixHard :: Text -> Text -> Text
   stripPrefixHard p s = fromMaybe s $ T.stripPrefix p s

hostDeduction :: HostDeduction -> (JsonLogfile -> Host)
hostDeduction = \case
  HostFromLogfilename -> hostFromLogfilename

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

spans :: forall a. (a -> Bool) -> [a] -> [Vector a]
spans f = go []
 where
   go :: [Vector a] -> [a] -> [Vector a]
   go acc [] = reverse acc
   go acc xs =
     case span f $ dropWhile (not . f) xs of
       ([], rest) -> go acc rest
       (ac, rest) ->
         go (Vec.fromList ac:acc) rest
