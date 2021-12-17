{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Analyze.Report
  ( report
  ) where

import           Control.Monad (when)
import qualified Data.Aeson as AE
import           Data.ByteString.Lazy (toStrict)
import           Data.List (nub, (\\))
import qualified Data.Map as Map
import           Data.Text (Text, drop, isPrefixOf, unpack)
import           Data.Text.Encoding (decodeUtf8)

import           Cardano.Tracer.Analyze.Process (extractNamespace)
import           Cardano.Tracer.Analyze.Types

report :: FileDBM -> FileDBM -> IO ()
report fdbm1 fdbm2 = do
    putStrLn "Trace analyzer report:"
    putStrLn $ "\nTotal messages db1 " <>
                show (fdbLengthTotal fdbm1)
    putStrLn $ "Different messages db1 " <>
                show (length (Map.toList (fdbKindMap fdbm1)))
    putStrLn $ "\nTotal messages db2 " <>
                show (fdbLengthTotal fdbm2)
    putStrLn $ "Different messages db2 " <>
                show (length (Map.toList (fdbKindMap fdbm2)))
    putStrLn ""

    reportParseErrors "db1" fdbm1
    reportParseErrors "db2" fdbm2

    _ <- reportUnclassified "db1" fdbm1
    _ <- reportUnclassified "db2" fdbm2

    reportClassifiedByNS "db1" fdbm1
    reportClassifiedByNS "db2" fdbm2

    _ <- reportMissingEntriesOf "db1" fdbm1 "db2" fdbm2
    count1 <- reportMissingEntriesOf "db2" fdbm2 "db1" fdbm1

    count2 <- reportCommonEntries fdbm1 fdbm2
    putStrLn $ "\n\nTotal number of different new messages db2 "
      <>  show (count1 + count2)



reportParseErrors :: Text -> FileDBM -> IO ()
reportParseErrors txt fdbm =
    case fdbErrors fdbm of
      [] -> putStrLn $ "No parse errors in " <> unpack txt
      l  -> do
              putStrLn $ "Parse errors in " <> unpack txt
              mapM_ printParseError l
  where
    printParseError :: (LineNumber, String) -> IO ()
    printParseError (ln,val) = putStrLn $ show ln <> " " <> show val

reportUnclassified :: Text -> FileDBM -> IO Int
reportUnclassified txt fdbm =
    case Map.lookup "Unclassified" (fdbKindMap fdbm) of
      Nothing -> do
        putStrLn $ "\nNo unclassifieds in " <> unpack txt
        pure 0
      Just l -> do
                    putStrLn $ "\nUnclassifieds in " <> unpack txt
                    mapM_ printUnclassifiedError l
                    pure (length l)
  where
    printUnclassifiedError :: (LineNumber, AE.Value) -> IO ()
    printUnclassifiedError (ln,val) =
      putStrLn $ show ln <> " " <> unpack (decodeUtf8 (toStrict (AE.encode val)))

reportClassifiedByNS :: Text -> FileDBM -> IO ()
reportClassifiedByNS txt fdbm =
    case filter (isPrefixOf "NS:") (Map.keys (fdbKindMap fdbm)) of
      [] -> putStrLn $ "\nNo classified by Namespace in " <> unpack txt
      l  -> do
              putStrLn $ "\nClassified by Namespace in " <> unpack txt
              mapM_ printClassifiedByNSError l
  where
    printClassifiedByNSError :: Text -> IO ()
    printClassifiedByNSError txt' =
      case Map.lookup txt' (fdbKindMap fdbm) of
        Nothing -> putStrLn $ "Can't find entry for " <> show txt'
        Just l ->  mapM_ (\(ln, val) ->
                      putStrLn $ show ln <> " " <>
                        unpack (decodeUtf8 (toStrict (AE.encode val)))) l

reportMissingEntriesOf :: Text -> FileDBM -> Text -> FileDBM -> IO Int
reportMissingEntriesOf txt1 fdbm1 txt2 fdbm2 =
    let keys1 = map canonicalizeKinds (Map.keys (fdbKindMap fdbm1))
        keys2 = map canonicalizeKinds (Map.keys (fdbKindMap fdbm2))
    in case keys1 \\ keys2 of
      [] -> do
        putStrLn $ "\nNo entries of " <> unpack txt1 <> " are missing in " <> unpack txt2
        pure 0
      l -> do
        putStrLn $ "\nThese entries of " <> unpack txt1 <> " are missing in " <> unpack txt2
        mapM_ printMissingError l
        pure (length l)
  where
    printMissingError :: Text -> IO ()
    printMissingError txt =
      case Map.lookup txt (fdbKindMap fdbm1) of
        Nothing -> case Map.lookup ("Trace" <> txt) (fdbKindMap fdbm1) of
                      Just l ->  putStrLn $ unpack ("Trace" <> txt) <> " number of entries "
                                          <> show (length l)
                      Nothing -> putStrLn $ "Can't find entry for " <> unpack txt
        Just l ->  putStrLn $ unpack txt <> " number of entries "
                            <> show (length l)

canonicalizeKinds :: Text -> Text
canonicalizeKinds "ChainSyncClientEvent.TraceDownloadedHeader" = "DownloadedHeader"
canonicalizeKinds "ChainSyncClientEvent.TraceFoundIntersection" = "FoundIntersection"
canonicalizeKinds "ChainSyncClientEvent.TraceRolledBack" = "RolledBack"
canonicalizeKinds "NS:cardano.node.BlockFetchDecision" = "NS:Cardano.Node.BlockFetchDecision"
canonicalizeKinds "TraceImmutableDBEvent.ValidatedLastLocation" = "ImmDbEvent.ValidatedLastLocation"
canonicalizeKinds "TraceImmutableDBEvent.ValidatingChunk" = "ImmDbEvent.ValidatingChunk"
canonicalizeKinds "TraceInitChainSelEvent.ValidCandidate" = "InitChainSelEvent.ValidCandidate"
canonicalizeKinds key = if "Trace" `isPrefixOf` key
                      then Data.Text.drop 5 key
                      else key

reportCommonEntries :: FileDBM -> FileDBM -> IO Int
reportCommonEntries fdbm1 fdbm2 =
    let keys = Map.keys (fdbKindMap fdbm1)
        keysCanonicalized = zip keys (map canonicalizeKinds keys)
    in do
      putStrLn "\n\nReporting common entries"
      l <- mapM (reportCommonEntry fdbm1 fdbm2) keysCanonicalized
      pure (sum l)

reportCommonEntry :: FileDBM -> FileDBM -> (KindName, KindName) -> IO Int
reportCommonEntry fdbm1 fdbm2 (kindName, kindNameCanonicalized) =
    case Map.lookup kindName (fdbKindMap fdbm1) of
      Just valuelist1 ->
        case Map.lookup kindNameCanonicalized (fdbKindMap fdbm2) of
          Just valuelist2 -> do
            putStrLn $ "\n\nReporting on " <> unpack kindName
                          <> " old occurences " <> show (length valuelist1)
                          <> " new occurences " <> show (length valuelist2)
            when (kindName /= kindNameCanonicalized) $
              putStrLn $ "\nCanonicalized as " <> unpack kindNameCanonicalized
            let oldNamespaces = nub $ map (extractNamespace . snd) valuelist1
            let newNamespaces = nub $ map (extractNamespace . snd) valuelist2
            case length oldNamespaces of
              0 -> putStrLn "\n\tNo old namespace!"
              1 -> putStrLn $ "\n\tOld namespace " <> show (head oldNamespaces)
              _ -> putStrLn $ "\n\tOld namespaces " <> show oldNamespaces
            case length newNamespaces of
              0 -> putStrLn "\n\tWARNING No new namespace!"
              1 -> putStrLn $ "\n\tNew namespace " <> show (head newNamespaces)
              _ -> putStrLn $ "\n\tWARNING New namespaces " <> show newNamespaces
            pure (length newNamespaces)
          Nothing -> pure 0
      Nothing -> pure 0
