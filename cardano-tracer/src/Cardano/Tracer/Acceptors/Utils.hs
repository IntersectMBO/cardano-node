{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-error=unused-imports
                -Wno-error=unused-local-binds
                -Wno-error=unused-matches
                -Wno-error=partial-type-signatures
                -Wno-error=unused-top-binds #-}

module Cardano.Tracer.Acceptors.Utils
  ( cmpFileOffsetsSDU
  , fileOffsetsSDU
  , handshakeCodec
  , notifyAboutNodeDisconnected
  , parseHandshakeLog
  , parseSDU
  , parseFileSDUs
  , parseSDUatOffset
  , prepareDataPointRequestor
  , prepareMetricsStores
  , printSDU
  , printSDUallOffsets
  , removeDisconnectedNode
  , decodeConfirm
  , decodeDone
  , decodeHandshake
  , decodePropose
  , decodeToken'
  ) where

import           Prelude hiding (unzip)
#if RTVIEW
import           Cardano.Logging (SeverityS (..))
import           Cardano.Tracer.Handlers.Notifications.Types
import           Cardano.Tracer.Handlers.Notifications.Utils
#endif
import           Cardano.Logging.Version (ForwardingVersion, forwardingVersionCodec)
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils
import qualified Network.Mux.Codec as Mux (decodeSDU)
import qualified Network.Mux.Trace as Mux (Error (..))
import qualified Network.Mux.Types as Mux (MiniProtocolNum (..), RemoteClockModel (..), SDU (..), SDUHeader (..))
import           Network.TypedProtocol.Codec (Codec (..), DecodeStep (..), SomeMessage (..), runDecoder)
import           Network.TypedProtocol.Core (IsActiveState (..), Protocol (..))
import           Ouroboros.Network.Protocol.Handshake.Codec (codecHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake (..), SingHandshake (..))
import           Ouroboros.Network.Snocket (LocalAddress)
import           Ouroboros.Network.Socket (ConnectionId (..))

import qualified Codec.CBOR.Read as CBOR (DeserialiseFailure)
import qualified Codec.CBOR.Term as CBOR (Term)

import           Control.Arrow (ArrowChoice (..))
-- This as-of-yet unused import reflects a goal to generalize from
-- monomorphic use of the IO monad to future monad-polymorphic
-- potentially pure use in the decoding functions.
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO)
import           Control.Exception (throw)
import           Control.Monad (forM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT, except, runExceptT, tryE)
import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Bimap as BM
import qualified Data.ByteString.Lazy as LBS (ByteString, hGet, readFile, splitAt)
import           Data.Either.Extra (eitherToMaybe, fromEither)
import           Data.Function (on)
import           Data.Functor ((<&>))
import           Data.Kind (Type)
import           Data.List.Extra (unsnoc)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (delete, fromList, insert)
import qualified Data.Map.Merge.Strict as Map (mapMissing, merge, zipWithMatched)
import           Data.Maybe (fromJust)
import qualified Data.Set as S
import           Data.These (These (..))
import qualified Data.These as These ()
import           Data.Time.Clock.POSIX (getPOSIXTime)
#if RTVIEW
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
#endif
import           Data.Tuple.Extra (both, swap)
import           GHC.Generics (Generic (..))
import           Numeric (showHex)
import qualified System.FilePath as FilePath (splitExtension)
import qualified System.Metrics as EKG
import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)
import           System.IO (Handle, IOMode (..), SeekMode (..))
import qualified System.IO as IO (hSeek, hTell, openFile)

import           Trace.Forward.Utils.DataPoint (DataPointRequestor, initDataPointRequestor)

handshakeCodec :: forall
      (error :: Type)
      (handshake :: Type)
      (monad :: Type -> Type)
      (string :: Type)
      (term :: Type)
      . ()
  => error ~ CBOR.DeserialiseFailure
  => handshake ~ Handshake ForwardingVersion term
  => monad ~ IO
  => string ~ LBS.ByteString
  => term ~ CBOR.Term
  => Codec handshake error monad string
handshakeCodec = codecHandshake forwardingVersionCodec

parseSDU :: LBS.ByteString -> Either Mux.Error (Mux.SDU, LBS.ByteString)
parseSDU (LBS.splitAt 8 -> (sduBS, bs))
  = right (, bs) $ Mux.decodeSDU sduBS

deriving instance Show Mux.RemoteClockModel
deriving instance Show Mux.SDUHeader
deriving instance Show Mux.SDU

decodeHandshake :: forall (error :: Type)
                          (vNumber :: Type)
                          (vParams :: Type)
                          (st :: Handshake vNumber vParams)
                          (string :: Type)
                        . ()
  => error ~ CBOR.DeserialiseFailure
  => vNumber ~ ForwardingVersion
  => vParams ~ CBOR.Term
  => string ~ LBS.ByteString
  => IsActiveState st (StateAgency st)
  => SingHandshake st
       -> IO (DecodeStep string error IO (SomeMessage st))
decodeHandshake = decode handshakeCodec

decodePropose :: forall (error :: Type)
                        (vNumber :: Type)
                        (vParams :: Type)
                        (stPropose :: Handshake vNumber vParams)
                        (string :: Type)
                      . ()
  => error ~ CBOR.DeserialiseFailure
  => vNumber ~ ForwardingVersion
  => vParams ~ CBOR.Term
  => stPropose ~ 'StPropose
  => string ~ LBS.ByteString
  => IsActiveState stPropose (StateAgency stPropose)
  => IO (DecodeStep string error IO (SomeMessage stPropose))
decodePropose = decodeHandshake SingPropose

decodeConfirm :: forall (error :: Type)
                        (vNumber :: Type)
                        (vParams :: Type)
                        (stConfirm :: Handshake vNumber vParams)
                        (string :: Type)
                      . ()
  => error ~ CBOR.DeserialiseFailure
  => vNumber ~ ForwardingVersion
  => vParams ~ CBOR.Term
  => stConfirm ~ 'StConfirm
  => string ~ LBS.ByteString
  => IsActiveState stConfirm (StateAgency stConfirm)
  => IO (DecodeStep string error IO (SomeMessage stConfirm))
decodeConfirm = decodeHandshake SingConfirm

decodeDone :: forall (error :: Type)
                     (vNumber :: Type)
                     (vParams :: Type)
                     (stDone :: Handshake vNumber vParams)
                     (string :: Type)
                   . ()
  => error ~ CBOR.DeserialiseFailure
  => vNumber ~ ForwardingVersion
  => vParams ~ CBOR.Term
  => stDone ~ 'StDone
  => string ~ LBS.ByteString
  => IsActiveState stDone (StateAgency stDone)
  => IO (DecodeStep string error IO (SomeMessage stDone))
decodeDone = decodeHandshake SingDone

data Either3 t t' t''
  = Left3 t
  | Middle3 t'
  | Right3 t''
  deriving (Eq, Foldable, FromJSON, ToJSON, Generic, Ord, Read, Show)

decodeToken' :: forall (error :: Type)
                       (vNumber :: Type)
                       (vParams :: Type)
                       (stConfirm :: Handshake vNumber vParams)
                       (stDone :: Handshake vNumber vParams)
                       (stPropose :: Handshake vNumber vParams)
                       (string :: Type)
                     . ()
  => error ~ CBOR.DeserialiseFailure
  => vNumber ~ ForwardingVersion
  => vParams ~ CBOR.Term
  => stConfirm ~ 'StConfirm
  => IsActiveState stConfirm (StateAgency stConfirm)
  => stDone ~ 'StDone
  => IsActiveState stDone (StateAgency stDone)
  => stPropose ~ 'StPropose
  => IsActiveState stPropose (StateAgency stPropose)
  => string ~ LBS.ByteString
  => [string]
     -> IO ( Either error (SomeMessage stConfirm)
           , Either error (SomeMessage stDone)
           , Either error (SomeMessage stPropose))
decodeToken' bytes = do
  confirm <- runDecoder bytes =<< decodeConfirm
  done    <- runDecoder bytes =<< decodeDone
  propose <- runDecoder bytes =<< decodePropose
  pure (confirm, done, propose)

parseHandshakeLog :: FilePath -> IO ()
parseHandshakeLog logFile = parseSDU <$> LBS.readFile logFile >>= \case
  Left msg -> print msg
  Right (sdu, cborBS) -> do
    print sdu
    print . take 1024 $ show cborBS
    pure ()

preadLBS :: Handle -> Integer -> Int -> IO LBS.ByteString
preadLBS handle offset count = do
  savedOffset <- IO.hTell handle
  IO.hSeek handle AbsoluteSeek offset
  byteString <- LBS.hGet handle count
  IO.hSeek handle AbsoluteSeek savedOffset
  pure byteString

parseSDUatOffset :: Handle -> Integer -> Int -> ExceptT Mux.Error IO Mux.SDU
parseSDUatOffset handle offset count = do
  byteString <- liftIO do preadLBS handle offset count
  except $ fst <$> parseSDU byteString

-- | A monadic unfold.
unfoldM :: Monad m => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldM f s = do
    f s >>= maybe (pure []) \(a, s') -> (a :) <$> unfoldM f s'

parseFileSDUs :: FilePath -> ExceptT Mux.Error IO [(Mux.SDUHeader, Integer)]
parseFileSDUs filePath = do
  handle <- liftIO do IO.openFile filePath ReadMode
  flip unfoldM 0 \(offset :: Integer) -> do
    maybeSDUH <- fmap eitherToMaybe . tryE $
      Mux.msHeader <$> parseSDUatOffset handle offset 8
    pure $ maybeSDUH <&> \sduH@Mux.SDUHeader {..} ->
      let newOffset = offset + fromIntegral mhLength
       in ((sduH, offset), newOffset)

infixr 8 <$$>
(<$$>) :: forall (f :: Type -> Type) (g :: Type -> Type) (t :: Type) (t' :: Type) . ()
  => Functor f
  => Functor g
  => (t -> t') -> f (g t) -> f (g t')
f <$$> xss = fmap f <$> xss

fileOffsetsSDU :: FilePath -> ExceptT Mux.Error IO [Integer]
fileOffsetsSDU filePath = snd <$$> parseFileSDUs filePath

showHex' :: Integral int => int -> String
showHex' int = "0x" <> showHex int ""

unzip :: Functor functor => functor (t, t') -> (functor t, functor t')
unzip structure = (fst <$> structure, snd <$> structure)

bothFoldr1 :: forall (fold :: Type -> Type) (t :: Type) . ()
  => Foldable fold
  => Functor fold
  => (t -> t -> t) -> fold (t, t) -> (t, t)
bothFoldr1 op struct = (foldr1 op leftStruct, foldr1 op rightStruct) where
  (leftStruct, rightStruct) :: (fold t, fold t) = unzip struct

pad :: Int -> String -> String
pad w s = replicate (w - length s) ' ' <> s

showOffsetPairs :: forall (offset :: Type) . ()
  => Integral offset
  => String -> String -> [(offset, offset)] -> [String]
showOffsetPairs ((<>":")->labelA) ((<>":")->labelB) pairList = strings where
  pairList' :: [(String, String)] = both showHex' <$> pairList
  (maxA, maxB) :: (Int, Int) = bothFoldr1 max $ both length <$> pairList'
  strings = forM pairList' \(pad maxA -> a, pad maxB -> b) -> do
    unwords [labelA, a, labelB, b]

zipDropEqOnM :: (Monad monad, Eq t')
  => (t -> monad [t']) -> t -> t -> monad [(t', t')]
zipDropEqOnM f x y = dropWhile (uncurry (==)) <$> liftA2 zip (f x) (f y)

cmpFileOffsetsSDU :: FilePath -> FilePath -> ExceptT Mux.Error IO ()
cmpFileOffsetsSDU filePath1 filePath2
  | (_, label1) <- FilePath.splitExtension filePath1
  , (_, label2) <- FilePath.splitExtension filePath2
  = liftIO . mapM_ putStrLn . showOffsetPairs label1 label2
            =<< zipDropEqOnM fileOffsetsSDU filePath1 filePath2

cmpFileSDUs :: FilePath -> FilePath -> ExceptT Mux.Error IO (Map Integer (These Mux.SDUHeader Mux.SDUHeader))
cmpFileSDUs = liftA2 merge' `on` mapOfFile where
      merge' = Map.merge this' that' these'
      this'  = Map.mapMissing . const $ This
      that'  = Map.mapMissing . const $ That
      these' = Map.zipWithMatched . const $ These
      mapOfFile f = Map.fromList <$> swap <$$> parseFileSDUs f

deriving instance Eq Mux.SDUHeader

cmpShowSDUs :: String -> String -> Integer -> These Mux.SDUHeader Mux.SDUHeader -> [String]
cmpShowSDUs label1 label2 offset = \case
  This sdu -> ((label1 <> ": ") <>) <$> lines (printSDU sdu offset)
  That sdu -> ((label2 <> ": ") <>) <$> lines (printSDU sdu offset)
  These sdu1 sdu2
    | sdu1 /= sdu2 -> undefined
    | otherwise    -> undefined

diffFileSDUs :: FilePath -> FilePath -> ExceptT Mux.Error IO ()
diffFileSDUs filePath1 filePath2
  | (_, label1) <- FilePath.splitExtension filePath1
  , (_, label2) <- FilePath.splitExtension filePath2
  = undefined

printSDU :: Mux.SDUHeader -> Integer -> String
printSDU Mux.SDUHeader {..} offset = unlines is''' where
  Mux.MiniProtocolNum mhNum' = mhNum
  is''' :: [String]
  is''' = [ "SDU header at off=" <> showHex' offset ] <> is''
  is'' :: [String]
  is'' | (frontList, backElt) <- fromJust $ unsnoc is'
       = [ "struct sdu {" ] <> frontList <> [ backElt <> " };" ]
  is'  :: [String]
  is'  = ("\t" <>) . (<> ";") . unwords <$> is
  is   :: [[String]]
  is   = [ [ "uint32_t", "sdu_xmit", "="
             , showHex' $ Mux.unRemoteClockModel mhTimestamp ]
        , [ "uint16_t sdu_proto_num", "="
             , show mhNum', "(" <> showHex' mhNum' <> ")" ]
        , [ "uint16_t sdu_len", "="
             , show mhLength, "(" <> showHex' mhLength <> ")" ]
        , [ "bool", "sdu_init_or_resp", "=", show mhDir ]
        , [ "const", "char", "*sdu_data", "=", "(nil)" ] ]

printSDUallOffsets :: FilePath -> IO ()
printSDUallOffsets filePath = fromEither . left throw <$> runExceptT monad where
  monad = printSDU' <$$> parseFileSDUs filePath >>= mapM_ (liftIO . putStrLn)
  printSDU' = head . lines . uncurry printSDU

prepareDataPointRequestor
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO DataPointRequestor
prepareDataPointRequestor TracerEnv{teConnectedNodes, teDPRequestors} connId = do
  addConnectedNode teConnectedNodes connId
  dpRequestor <- initDataPointRequestor
  atomically $
    modifyTVar' teDPRequestors $ Map.insert (connIdToNodeId connId) dpRequestor
  return dpRequestor

prepareMetricsStores
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO (EKG.Store, TVar MetricsLocalStore)
prepareMetricsStores TracerEnv{teConnectedNodes, teAcceptedMetrics} connId = do
  addConnectedNode teConnectedNodes connId
  store <- EKG.newStore

  EKG.registerCounter "ekg.server_timestamp_ms" getTimeMs store
  storesForNewNode <- (store ,) <$> newTVarIO emptyMetricsLocalStore

  atomically do
    modifyTVar' teAcceptedMetrics do
      Map.insert (connIdToNodeId connId) storesForNewNode

  return storesForNewNode

  where
    -- forkServer definition of `getTimeMs'. The ekg frontend relies
    -- on the "ekg.server_timestamp_ms" metric being in every
    -- store. While forkServer adds that that automatically we must
    -- manually add it.
    -- url
    --  + https://github.com/tvh/ekg-wai/blob/master/System/Remote/Monitoring/Wai.hs#L237-L238
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

addConnectedNode
  :: ConnectedNodes
  -> ConnectionId LocalAddress
  -> IO ()
addConnectedNode connectedNodes connId = atomically $
  modifyTVar' connectedNodes $ S.insert (connIdToNodeId connId)

-- | This handler is called when 'runPeer' function throws an exception,
--   which means that there is a problem with network connection.
removeDisconnectedNode
  :: TracerEnv
  -> ConnectionId LocalAddress
  -> IO ()
removeDisconnectedNode tracerEnv connId =
  -- Remove all the stuff related to disconnected node.
  atomically $ do
    modifyTVar' teConnectedNodes      $ S.delete  nodeId
    modifyTVar' teConnectedNodesNames $ BM.delete nodeId
    modifyTVar' teAcceptedMetrics     $ Map.delete  nodeId
    modifyTVar' teDPRequestors        $ Map.delete  nodeId
 where
  TracerEnv{teConnectedNodes, teConnectedNodesNames, teAcceptedMetrics, teDPRequestors} = tracerEnv
  nodeId = connIdToNodeId connId

notifyAboutNodeDisconnected
  :: TracerEnvRTView
  -> ConnectionId LocalAddress
  -> IO ()
#if RTVIEW
notifyAboutNodeDisconnected TracerEnvRTView{teEventsQueues} connId = do
  now <- systemToUTCTime <$> getSystemTime
  addNewEvent teEventsQueues EventNodeDisconnected $ Event nodeId now Warning msg
 where
  nodeId = connIdToNodeId connId
  msg = "Node is disconnected"
#else
notifyAboutNodeDisconnected _ _ = pure ()
#endif
