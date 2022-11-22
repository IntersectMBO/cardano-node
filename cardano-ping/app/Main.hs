{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Control.Exception
import           Control.Monad (replicateM, unless, when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer hiding (timeout)
import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Data.Aeson hiding (Options, json)
import           Data.Bits (clearBit, setBit, testBit)
import qualified Data.ByteString.Char8 as BS.Char
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL.Char (pack, putStr)
import           Data.List (foldl')
import           Data.Maybe (fromMaybe, isNothing)
import           Data.TDigest
import           Data.Text (unpack)
import           Data.Word
import           Network.Socket (AddrInfo, StructLinger (..))
import qualified Network.Socket as Socket
import           System.Console.GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit
import           System.IO (hFlush, hPrint, stderr, stdout)
import           Text.Printf

import           Network.Mux.Bearer.Socket
import           Network.Mux.Timeout
import           Network.Mux.Types

mainnetMagic :: Word32
mainnetMagic = 764824073

handshakeNum ::  MiniProtocolNum
handshakeNum = MiniProtocolNum 0

keepaliveNum :: MiniProtocolNum
keepaliveNum = MiniProtocolNum 8

nodeToClientVersionBit :: Int
nodeToClientVersionBit = 15


data Flag = CountF String | HelpF | HostF String | PortF String | MagicF String | QuietF | JsonF | UnixF String deriving Show

optionDescriptions :: [OptDescr Flag]
optionDescriptions = [
    Option "c" ["count"]  (ReqArg CountF "count")  "number of pings to send",
    Option "" ["help"]  (NoArg HelpF)  "print help",
    Option "h" ["host"]  (ReqArg HostF "host")  "hostname/ip, e.g relay.iohk.example",
    Option "u" ["unixsock"] (ReqArg UnixF "unixsock") "unix socket, e.g file.socket",
    Option "m" ["magic"] (ReqArg MagicF "magic") ("magic, defaults to " ++ show mainnetMagic),
    Option "j" ["json"]  (NoArg JsonF ) "json output flag",
    Option "p" ["port"]  (ReqArg PortF "port") "portnumber, e.g 1234",
    Option "q" ["quiet"] (NoArg QuietF ) "quiet flag, csv/json only output."
  ]

data Options = Options {
      maxCount :: Word32
    , host     :: Maybe String
    , unixSock :: Maybe String
    , port     :: String
    , magic    :: Word32
    , json     :: Bool
    , quiet    :: Bool
    , help     :: Bool
    } deriving Show

defaultOpts :: Options
defaultOpts = Options {
      maxCount = maxBound
    , host     = Nothing
    , unixSock = Nothing
    , port     = "3001"
    , json     = False
    , quiet    = False
    , magic    = mainnetMagic
    , help     = False
    }

buildOptions ::  Flag -> Options -> Options
buildOptions (CountF c)   opt = opt { maxCount = Prelude.read c }
buildOptions HelpF        opt = opt { help = True }
buildOptions (HostF host) opt = opt { host = Just host }
buildOptions (PortF port) opt = opt { port = port }
buildOptions (MagicF m)   opt = opt { magic = Prelude.read m }
buildOptions JsonF        opt = opt { json = True }
buildOptions QuietF       opt = opt { quiet = True }
buildOptions (UnixF file) opt = opt { unixSock = Just file }

data LogMsg = LogMsg ByteString
            | LogEnd

logger :: StrictTMVar IO LogMsg -> Bool -> IO ()
logger msgQueue json = go True
  where
    go first = do
        msg <- atomically $ takeTMVar  msgQueue
        case msg of
             LogMsg bs -> do
                 let bs' = case (json, first) of
                                (True, False)  -> BL.Char.pack ",\n" <> bs
                                (True, True)   -> BL.Char.pack "{ \"pongs\": [ " <> bs
                                (False, True)  -> BL.Char.pack "   timestamp,                         host,                          cookie,  sample,  median,     p90,    mean,     min,     max,     std\n" <> bs
                                (False, False) -> bs

                 BL.Char.putStr bs'
                 go False
             LogEnd ->
                 when json $ putStrLn "] }"

supportedNodeToNodeVersions :: Word32 -> [NodeVersion]
supportedNodeToNodeVersions magic =
  [ NodeToNodeVersionV7  magic False
  , NodeToNodeVersionV8  magic False
  , NodeToNodeVersionV9  magic False
  , NodeToNodeVersionV10 magic False
  ]

supportedNodeToClientVersions :: Word32 -> [NodeVersion]
supportedNodeToClientVersions magic =
  [ NodeToClientVersionV9  magic
  , NodeToClientVersionV10 magic
  , NodeToClientVersionV11 magic
  , NodeToClientVersionV12 magic
  , NodeToClientVersionV13 magic
  , NodeToClientVersionV14 magic
  ]

main :: IO ()
main = do
    args <- getArgs
    let (flags, _, _ ) = getOpt RequireOrder optionDescriptions args
        options = foldr buildOptions defaultOpts flags
        hints = Socket.defaultHints { Socket.addrSocketType = Socket.Stream }

    when (help options) $ do
        progName <- getProgName
        putStrLn $ usageInfo progName optionDescriptions
        exitSuccess

    msgQueue <- newEmptyTMVarIO

    when (isNothing (host options) && isNothing (unixSock options) ) $ do
        putStrLn "Specify host/ip with '-h <hostname>' or a unix socket with -u <file name>"
        exitWith (ExitFailure 1)

    (addresses, versions) <- case unixSock options of
        Nothing -> do
          addrs <- Socket.getAddrInfo (Just hints) (host options)
                       (Just $ port options)
          return (addrs, supportedNodeToNodeVersions $ magic options)
        Just fname ->
          return ([ Socket.AddrInfo [] Socket.AF_UNIX Socket.Stream
                    Socket.defaultProtocol (Socket.SockAddrUnix fname)
                    Nothing ]
                 , supportedNodeToClientVersions $ magic options)

    laid <- async $ logger msgQueue $ json options
    caids <- mapM (async . pingClient (Tracer $ doLog msgQueue) options
                   versions) addresses
    res <- zip addresses <$> mapM waitCatch caids
    doLog msgQueue LogEnd
    wait laid
    case foldl' partition ([],[]) res of
         ([], _) -> do
             exitSuccess
         (es, []) -> do
             mapM_ (hPrint stderr) es
             exitWith (ExitFailure 1)
         (es, _) -> do
             unless (quiet options) $
               mapM_ (hPrint stderr) es
             exitSuccess

  where

    partition :: ([(AddrInfo, SomeException)], [AddrInfo])
              -> (AddrInfo, Either SomeException ())
              -> ([(AddrInfo, SomeException)], [AddrInfo])
    partition (es, as) (a, Left e)  = ((a, e) : es, as)
    partition (es, as) (a, Right _) = (es, a : as)

    doLog :: StrictTMVar IO LogMsg -> LogMsg -> IO ()
    doLog msgQueue msg = atomically $ putTMVar msgQueue msg

data NodeVersion =       NodeToClientVersionV9  Word32
                       | NodeToClientVersionV10 Word32
                       | NodeToClientVersionV11 Word32
                       | NodeToClientVersionV12 Word32
                       | NodeToClientVersionV13 Word32
                       | NodeToClientVersionV14 Word32
                       | NodeToNodeVersionV1   Word32
                       | NodeToNodeVersionV2   Word32
                       | NodeToNodeVersionV3   Word32
                       | NodeToNodeVersionV4   Word32 Bool
                       | NodeToNodeVersionV5   Word32 Bool
                       | NodeToNodeVersionV6   Word32 Bool
                       | NodeToNodeVersionV7   Word32 Bool
                       | NodeToNodeVersionV8   Word32 Bool
                       | NodeToNodeVersionV9   Word32 Bool
                       | NodeToNodeVersionV10  Word32 Bool
                       deriving (Eq, Ord, Show)

keepAliveReqEnc :: NodeVersion -> Word16 -> CBOR.Encoding
keepAliveReqEnc v cookie | v >= NodeToNodeVersionV7 minBound minBound =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 0
    <> CBOR.encodeWord16 cookie
keepAliveReqEnc _ cookie =
       CBOR.encodeWord 0
    <> CBOR.encodeWord16 cookie

keepAliveReq :: NodeVersion -> Word16 -> ByteString
keepAliveReq v c = CBOR.toLazyByteString $ keepAliveReqEnc v c

keepAliveDone :: NodeVersion -> ByteString
keepAliveDone v | v >= NodeToNodeVersionV7 minBound minBound =
    CBOR.toLazyByteString $
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 2
keepAliveDone _ =
    CBOR.toLazyByteString $
      CBOR.encodeWord 2


handshakeReqEnc :: [NodeVersion] -> CBOR.Encoding
handshakeReqEnc [] = error "null version list"
handshakeReqEnc versions =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord 0
    <> CBOR.encodeMapLen (fromIntegral $ length versions)
    <> mconcat [ encodeVersion v
               | v <- versions
               ]
  where
    encodeVersion :: NodeVersion -> CBOR.Encoding
    encodeVersion (NodeToClientVersionV9 magic) =
          CBOR.encodeWord (9 `setBit` nodeToClientVersionBit)
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV10 magic) =
          CBOR.encodeWord (10 `setBit` nodeToClientVersionBit)
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV11 magic) =
          CBOR.encodeWord (11 `setBit` nodeToClientVersionBit)
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV12 magic) =
          CBOR.encodeWord (12 `setBit` nodeToClientVersionBit)
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV13 magic) =
          CBOR.encodeWord (13 `setBit` nodeToClientVersionBit)
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToClientVersionV14 magic) =
          CBOR.encodeWord (14 `setBit` nodeToClientVersionBit)
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV1 magic) =
          CBOR.encodeWord 1
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV2 magic) =
          CBOR.encodeWord 2
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV3 magic) =
          CBOR.encodeWord 3
       <> CBOR.encodeInt (fromIntegral magic)
    encodeVersion (NodeToNodeVersionV4 magic mode) = encodeWithMode 4 magic mode
    encodeVersion (NodeToNodeVersionV5 magic mode) = encodeWithMode 5 magic mode
    encodeVersion (NodeToNodeVersionV6 magic mode) = encodeWithMode 6 magic mode
    encodeVersion (NodeToNodeVersionV7 magic mode) = encodeWithMode 7 magic mode
    encodeVersion (NodeToNodeVersionV8 magic mode) = encodeWithMode 8 magic mode
    encodeVersion (NodeToNodeVersionV9 magic mode) = encodeWithMode 9 magic mode
    encodeVersion (NodeToNodeVersionV10 magic mode) = encodeWithMode 10 magic mode


    encodeWithMode :: Word -> Word32 -> Bool -> CBOR.Encoding
    encodeWithMode vn magic mode =
          CBOR.encodeWord vn
       <> CBOR.encodeListLen 2
       <> CBOR.encodeInt (fromIntegral magic)
       <> CBOR.encodeBool mode

handshakeReq :: [NodeVersion] -> ByteString
handshakeReq []       = BL.empty
handshakeReq versions = CBOR.toLazyByteString $ handshakeReqEnc versions

data HandshakeFailure = UnknownVersionInRsp Word
                      | UnknownKey Word
                      | UnknownTag Word
                      | VersionMissmath [Word]
                      | DecodeError Word String
                      | Refused Word String
                      deriving Show

newtype KeepAliveFailure = KeepAliveFailureKey Word deriving Show

keepAliveRspDec :: NodeVersion
                -> CBOR.Decoder s (Either KeepAliveFailure Word16)
keepAliveRspDec v | v >= (NodeToNodeVersionV7 minBound minBound)  = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (len, key) of
         (2, 1) -> Right <$> CBOR.decodeWord16
         (_, k) -> return $ Left $ KeepAliveFailureKey k
keepAliveRspDec _ = do
    key <- CBOR.decodeWord
    case key of
         1 -> Right <$> CBOR.decodeWord16
         k -> return $ Left $ KeepAliveFailureKey k

handshakeDec :: CBOR.Decoder s (Either HandshakeFailure NodeVersion)
handshakeDec = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case key of
         1 -> do
             decodeVersion
         2 -> do
             _ <- CBOR.decodeListLen
             tag <- CBOR.decodeWord
             case tag of
                  0 -> do -- VersionMismatch
                      len <- CBOR.decodeListLen
                      x <- replicateM len CBOR.decodeWord
                      return $ Left $ VersionMissmath x
                  1 -> do -- HandshakeDecodeError
                      vn <- CBOR.decodeWord
                      msg <- unpack <$> CBOR.decodeString
                      return $ Left $ DecodeError vn msg
                  2 -> do -- Refused
                      vn <- CBOR.decodeWord
                      msg <- unpack <$> CBOR.decodeString
                      return $ Left $ Refused vn msg
                  _ -> return $ Left $ UnknownTag tag

         k -> return $ Left $ UnknownKey k
  where

    decodeVersion :: CBOR.Decoder s (Either HandshakeFailure NodeVersion)
    decodeVersion = do
        version <- CBOR.decodeWord
        case ( version `clearBit` nodeToClientVersionBit
             , version `testBit`  nodeToClientVersionBit ) of
             (7,  False) -> decodeWithMode NodeToNodeVersionV7
             (8,  False) -> decodeWithMode NodeToNodeVersionV8
             (9,  False) -> decodeWithMode NodeToNodeVersionV9
             (10, False) -> decodeWithMode NodeToNodeVersionV10
             (9,  True)  -> Right . NodeToClientVersionV9 <$> CBOR.decodeWord32
             (10, True)  -> Right . NodeToClientVersionV10 <$> CBOR.decodeWord32
             (11, True)  -> Right . NodeToClientVersionV11 <$> CBOR.decodeWord32
             (12, True)  -> Right . NodeToClientVersionV12 <$> CBOR.decodeWord32
             (13, True)  -> Right . NodeToClientVersionV13 <$> CBOR.decodeWord32
             (14, True)  -> Right . NodeToClientVersionV14 <$> CBOR.decodeWord32
             _           -> return $ Left $ UnknownVersionInRsp version

    decodeWithMode :: (Word32 -> Bool -> NodeVersion)
                   -> CBOR.Decoder s (Either HandshakeFailure NodeVersion)
    decodeWithMode vnFun = do
        _ <- CBOR.decodeListLen
        magic <- CBOR.decodeWord32
        Right . vnFun magic <$> CBOR.decodeBool

wrap :: MiniProtocolNum -> MiniProtocolDir -> BL.ByteString -> MuxSDU
wrap ptclNum ptclDir blob = MuxSDU {
      msHeader = MuxSDUHeader {
                mhTimestamp = RemoteClockModel 0,
                mhNum       = ptclNum,
                mhDir       = ptclDir,
                mhLength    = fromIntegral $ BL.length blob
              }
    , msBlob = blob
    }


data StatPoint = StatPoint {
      spTimestamp :: !UTCTime
    , spHost      :: !String
    , spCookie    :: !Word16
    , spSample    :: !Double
    , spMedian    :: !Double
    , spP90       :: !Double
    , spMean      :: !Double
    , spMin       :: !Double
    , spMax       :: !Double
    , spStd       :: !Double
    }

instance Show StatPoint where
    show StatPoint {..} =
        printf "%36s, %-28s, %7d, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f, %7.3f"
            (show spTimestamp) spHost spCookie spSample spMedian spP90 spMean spMin spMax spStd

instance ToJSON StatPoint where
  toJSON StatPoint {..} =
      object [ "timestamp" .= spTimestamp
             , "host"      .=  spHost
             , "cookie"    .= spCookie
             , "sample"    .= spSample
             , "median"    .= spMedian
             , "p90"       .= spP90
             , "mean"      .= spMean
             , "min"       .= spMin
             , "max"       .= spMax
             ]

toStatPoint :: UTCTime -> String -> Word16 -> Double -> TDigest 5 -> StatPoint
toStatPoint ts host cookie sample td =
    StatPoint {
          spTimestamp = ts
        , spHost      = host
        , spCookie    = cookie
        , spSample    = sample
        , spMedian    = quantile' 0.5
        , spP90       = quantile' 0.9
        , spMean      = mean'
        , spMin       = minimumValue td
        , spMax       = maximumValue td
        , spStd       = stddev'
        }
  where
    quantile' :: Double -> Double
    quantile' q = fromMaybe 0 (quantile q td)

    mean' :: Double
    mean' = fromMaybe 0 (mean td)

    stddev' :: Double
    stddev' = fromMaybe 0 (stddev td)


pingClient :: Tracer IO LogMsg -> Options -> [NodeVersion] -> AddrInfo -> IO ()
pingClient tracer Options{quiet, json, maxCount} versions peer = bracket
    (Socket.socket (Socket.addrFamily peer) Socket.Stream Socket.defaultProtocol)
    Socket.close
    (\sd -> withTimeoutSerial $ \timeoutfn -> do
        when (Socket.addrFamily peer /= Socket.AF_UNIX) $ do
            Socket.setSocketOption sd Socket.NoDelay 1
            Socket.setSockOpt sd Socket.Linger
                (StructLinger { sl_onoff  = 1
                              , sl_linger = 0 })

        !t0_s <- getMonotonicTime
        Socket.connect sd (Socket.addrAddress peer)
        !t0_e <- getMonotonicTime
        peerStr <- peerString
        unless quiet $ printf "%s network rtt: %.3f\n" peerStr $ toSample t0_e t0_s
        let timeout = 30
            bearer = socketAsMuxBearer timeout nullTracer sd

        !t1_s <- write bearer timeoutfn $ wrap handshakeNum InitiatorDir
                    (handshakeReq versions)
        (msg, !t1_e) <- nextMsg bearer timeoutfn handshakeNum
        unless quiet $ printf "%s handshake rtt: %s\n" peerStr (show $ diffTime t1_e t1_s)
        case CBOR.deserialiseFromBytes handshakeDec msg of
             Left err -> do
                 eprint $ printf "%s Decoding error %s\n" peerStr (show err)
             Right (_, Left err) -> do
                 eprint $ printf "%s Protocol error %s\n" peerStr (show err)
             Right (_, Right version) -> do
                unless quiet $ printf "%s Negotiated version %s\n" peerStr (show version)
                keepAlive bearer timeoutfn peerStr version (tdigest []) 0
                -- send terminating message
                _ <- write bearer timeoutfn $
                        wrap keepaliveNum InitiatorDir (keepAliveDone version)
                -- protocol idle timeout
                threadDelay 5

    )
  where

    peerString :: IO String
    peerString =
        case Socket.addrFamily peer of
             Socket.AF_UNIX -> return $ show $ Socket.addrAddress peer
             _ -> do
                  (Just host, Just port) <-
                    Socket.getNameInfo
                      [ Socket.NI_NUMERICHOST, Socket.NI_NUMERICSERV ]
                      True True (Socket.addrAddress peer)
                  return $ host ++ ":" ++ port

    toSample :: Time -> Time -> Double
    toSample t_e t_s = realToFrac $ diffTime t_e t_s

    eprint :: String -> IO ()
    eprint = BS.Char.hPutStr stderr . BS.Char.pack

    nextMsg ::  MuxBearer IO -> TimeoutFn IO -> MiniProtocolNum -> IO (BL.ByteString, Time)
    nextMsg bearer timeoutfn ptclNum = do
        (sdu, t_e) <- Network.Mux.Types.read bearer timeoutfn
        if mhNum (msHeader sdu) == ptclNum
           then return (msBlob sdu, t_e)
           else nextMsg bearer timeoutfn ptclNum

    keepAlive :: MuxBearer IO
              -> TimeoutFn IO
              -> String
              -> NodeVersion
              -> TDigest 5
              -> Word32
              -> IO ()
    keepAlive _ _ _ _ _ cookie | cookie == maxCount = return ()
    keepAlive bearer timeoutfn peerStr version td !cookie = do
        let cookie16 = fromIntegral cookie
        !t_s <- write bearer timeoutfn $ wrap keepaliveNum InitiatorDir (keepAliveReq version cookie16)
        (!msg, !t_e) <- nextMsg bearer timeoutfn keepaliveNum
        let rtt = toSample t_e t_s
            td' = insert rtt td
        case CBOR.deserialiseFromBytes (keepAliveRspDec version) msg of
             Left err -> eprint $ printf "%s keepalive decoding error %s\n" peerStr (show err)
             Right (_, Left err) -> eprint $ printf "%s keepalive protocol error %s\n" peerStr (show err)
             Right (_, Right cookie') -> do
                 when (cookie' /= cookie16) $ eprint $ printf "%s cookie missmatch %d /= %d\n"
                     peerStr cookie' cookie

                 now <- getCurrentTime
                 let point = toStatPoint now peerStr cookie16 rtt td'
                 if json
                    then traceWith tracer $ LogMsg (encode point)
                    else traceWith tracer $ LogMsg $ BL.Char.pack $ show point <> "\n"
                 hFlush stdout
                 threadDelay 1
                 keepAlive bearer timeoutfn peerStr version td' (cookie + 1)
