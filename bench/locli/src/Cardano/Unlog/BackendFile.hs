
module Cardano.Unlog.BackendFile where

import           Cardano.Analysis.API.Ground
import           Cardano.Prelude hiding (Text, show, toText)
import           Cardano.Unlog.LogObject
import           Cardano.Util

import           Prelude (id, show)

import qualified Data.Aeson as AE (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as TS
import qualified Data.Text.Short as Text
import           GHC.Conc (numCapabilities)


runLiftLogObjects :: RunLogs () -> Bool -> Maybe [LOAnyType]
                  -> ExceptT TS.Text IO (RunLogs [LogObject])
runLiftLogObjects rl@RunLogs{..} okDErr loAnyLimit = liftIO $
 go Map.empty 0 simultaneousReads
 where
   go (force -> !acc) batchBase = \case
     []    -> pure $ rl{ rlHostLogs = acc }
     c:cs  -> do
       let batchBase' = batchBase + length c
       when (length c > 1) $
         progress "logs" (Q $ printf "processing batch %d - %d" batchBase (batchBase' - 1))
       hlsMap <- readHostLogChunk c
       go (acc `Map.union` hlsMap) batchBase' cs

   simultaneousReads = chunksOf numCapabilities (Map.toList rlHostLogs)

   readHostLogChunk :: [(Host, HostLogs ())] -> IO (Map Host (HostLogs [LogObject]))
   readHostLogChunk hls =
     Map.fromList <$> forConcurrently hls (uncurry readHostLogs)

   readHostLogs :: Host -> HostLogs () -> IO (Host, HostLogs [LogObject])
   readHostLogs h hl@HostLogs{..} =
     readLogObjectStream (unJsonLogfile $ fst hlLogs) okDErr loAnyLimit
     <&> (h,) . setLogs hl . fmap (setLOhost h)

   setLogs :: HostLogs a -> b -> HostLogs b
   setLogs hl x = hl { hlLogs = (fst $ hlLogs hl, x) }
   setLOhost :: Host -> LogObject -> LogObject
   setLOhost h lo = lo { loHost = h }

readLogObjectStream :: FilePath -> Bool -> Maybe [LOAnyType] -> IO [LogObject]
readLogObjectStream f okDErr loAnyLimit =
  LBS.readFile f
    <&>
    (if okDErr then id else
        filter ((\case
                    LODecodeError input err -> error
                      (printf "Decode error while parsing %s:\n%s\non input:\n>>>  %s" f (Text.toString err) (Text.toString input))
                    _ -> True)
               . loBody)) .
    filter ((case loAnyLimit of
              Nothing -> \case
                LOAny{} -> False
                _       -> True
              Just constraint -> \case
                LOAny laty obj ->
                  elem laty constraint
                  || error (printf "Unexpected LOAny while parsing %s -- %s: %s"
                                   f (show laty) (show obj))
                _ -> True)
             . loBody) .
    filter (not . isDecodeError "Error in $: not enough input" . loBody) .
    fmap (\bs ->
            AE.eitherDecode bs &
            either
            (LogObject zeroUTCTime "Cardano.Analysis.DecodeError" "DecodeError" "" (TId "0")
             . LODecodeError (Text.fromByteString (LBS.toStrict bs)
                               & fromMaybe "#<ERROR decoding input fromByteString>")
              . Text.fromText
              . TS.pack)
            id)
    . filter (not . LBS.null)
    . LBS.split (fromIntegral $ fromEnum '\n')
 where
   isDecodeError x = \case
     LODecodeError _ x' -> x == x'
     _ -> False
