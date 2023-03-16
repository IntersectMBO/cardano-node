{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT "Avoid $" -}

module Cardano.CLI.Ping
  ( PingCmd(..)
  , PingClientCmdError(..)
  , renderPingClientCmdError
  , runPingCmd
  , parsePingCmd
  ) where

import           Control.Applicative (optional)
import           Control.Concurrent.Class.MonadSTM.Strict (StrictTMVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as STM
import           Control.Exception (SomeException)
import           Control.Monad (forM, unless, when)
import           Control.Monad.Class.MonadAsync (MonadAsync (async, wait, waitCatch))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)
import           Control.Tracer (Tracer (..))
import           Data.List (foldl')
import qualified Data.List as L
import           Data.Maybe (isNothing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word32)
import           Network.Socket (AddrInfo)
import qualified Network.Socket as Socket
import qualified Options.Applicative as Opt
import qualified Prettyprinter as PP
import qualified System.Exit as IO
import qualified System.IO as IO

import qualified Cardano.Network.Ping as CNP

data PingClientCmdError
  = PingClientCmdErrorOfInvalidHostIp
  | PingClientCmdErrorOfExceptions ![(AddrInfo, SomeException)]


data PingCmd = PingCmd
  { pingCmdCount    :: !Word32
  , pingCmdHost     :: !(Maybe String)
  , pingCmdUnixSock :: !(Maybe String)
  , pingCmdPort     :: !String
  , pingCmdMagic    :: !Word32
  , pingCmdJson     :: !Bool
  , pingCmdQuiet    :: !Bool
  } deriving (Eq, Show)

pingClient :: Tracer IO CNP.LogMsg -> Tracer IO String -> PingCmd -> [CNP.NodeVersion] -> AddrInfo -> IO ()
pingClient stdout stderr cmd = CNP.pingClient stdout stderr opts
  where opts = CNP.PingOpts
          { CNP.pingOptsQuiet     = pingCmdQuiet cmd
          , CNP.pingOptsJson      = pingCmdJson cmd
          , CNP.pingOptsCount     = pingCmdCount cmd
          , CNP.pingOptsHost      = pingCmdHost cmd
          , CNP.pingOptsUnixSock  = pingCmdUnixSock cmd
          , CNP.pingOptsPort      = pingCmdPort cmd
          , CNP.pingOptsMagic     = pingCmdMagic cmd
          }

runPingCmd :: PingCmd -> ExceptT PingClientCmdError IO ()
runPingCmd options = do
  let hints = Socket.defaultHints { Socket.addrSocketType = Socket.Stream }

  msgQueue <- liftIO STM.newEmptyTMVarIO

  when (isNothing (pingCmdHost options) && isNothing (pingCmdUnixSock options)) $
    left PingClientCmdErrorOfInvalidHostIp

  (addresses, versions) <- case pingCmdUnixSock options of
    Nothing -> do
      addrs <- liftIO $ Socket.getAddrInfo (Just hints) (pingCmdHost options) (Just (pingCmdPort options))
      return (addrs, CNP.supportedNodeToNodeVersions $ pingCmdMagic options)
    Just fname ->
      return
        ( [ Socket.AddrInfo [] Socket.AF_UNIX Socket.Stream
              Socket.defaultProtocol (Socket.SockAddrUnix fname)
              Nothing
          ]
        , CNP.supportedNodeToClientVersions $ pingCmdMagic options
        )

  laid <- liftIO . async $ CNP.logger msgQueue $ pingCmdJson options
  caids <- forM addresses $ liftIO . async . pingClient (Tracer $ doLog msgQueue) (Tracer doErrLog) options versions
  res <- L.zip addresses <$> mapM (liftIO . waitCatch) caids
  liftIO $ doLog msgQueue CNP.LogEnd
  liftIO $ wait laid
  case foldl' partition ([],[]) res of
    ([], _) -> liftIO IO.exitSuccess
    (es, []) -> left $ PingClientCmdErrorOfExceptions es
    (es, _) -> do
      unless (pingCmdQuiet options) $ mapM_ (liftIO . IO.hPrint IO.stderr) es
      liftIO IO.exitSuccess

  where
    partition :: ([(AddrInfo, SomeException)], [AddrInfo])
              -> (AddrInfo, Either SomeException ())
              -> ([(AddrInfo, SomeException)], [AddrInfo])
    partition (es, as) (a, Left e)  = ((a, e) : es, as)
    partition (es, as) (a, Right _) = (es, a : as)

    doLog :: StrictTMVar IO CNP.LogMsg -> CNP.LogMsg -> IO ()
    doLog msgQueue msg = STM.atomically $ STM.putTMVar msgQueue msg

    doErrLog :: String -> IO ()
    doErrLog = IO.hPutStrLn IO.stderr

renderPingClientCmdError :: PingClientCmdError -> Text
renderPingClientCmdError = \case
  PingClientCmdErrorOfInvalidHostIp -> "Specify host/ip with '-h <hostname>' or a unix socket with -u <file name>"
  PingClientCmdErrorOfExceptions es -> T.intercalate "\n" $ T.pack . show <$> es

parsePingCmd :: Opt.Parser PingCmd
parsePingCmd = Opt.hsubparser $ mconcat
  [ Opt.metavar "ping"
  , Opt.command "ping" $ Opt.info pPing $ Opt.progDescDoc $ Just $ mconcat
    [ PP.pretty @String "Ping a cardano node either using node-to-node or node-to-client protocol. "
    , PP.pretty @String "It negotiates a handshake and keeps sending keep alive messages."
    ]
  ]

pPing :: Opt.Parser PingCmd
pPing = PingCmd
  <$> ( Opt.option Opt.auto $ mconcat
        [ Opt.long "count"
        , Opt.short 'c'
        , Opt.metavar "COUNT"
        , Opt.help $ mconcat
          [ "Stop after sending count requests and receiving count responses.  "
          , "If this option is not specified, ping will operate until interrupted.  "
          ]
        , Opt.value maxBound
        ]
      )
  <*> ( optional $ Opt.strOption $ mconcat
        [ Opt.long "host"
        , Opt.short 'h'
        , Opt.metavar "HOST"
        , Opt.help "Hostname/IP, e.g. relay.iohk.example."
        ]
      )
  <*> ( optional $ Opt.strOption $ mconcat
        [ Opt.long "unixsock"
        , Opt.short 'u'
        , Opt.metavar "SOCKET"
        , Opt.help "Unix socket, e.g. file.socket."
        ]
      )
  <*> ( Opt.strOption $ mconcat
        [ Opt.long "port"
        , Opt.short 'p'
        , Opt.metavar "PORT"
        , Opt.help "Port number, e.g. 1234."
        , Opt.value "3001"
        ]
      )
  <*> ( Opt.option Opt.auto $ mconcat
        [ Opt.long "magic"
        , Opt.short 'm'
        , Opt.metavar "MAGIC"
        , Opt.help "Network magic."
        , Opt.value CNP.mainnetMagic
        ]
      )
  <*> ( Opt.switch $ mconcat
        [ Opt.long "json"
        , Opt.short 'j'
        , Opt.help "JSON output flag."
        ]
      )
  <*> ( Opt.switch $ mconcat
        [ Opt.long "quiet"
        , Opt.short 'q'
        , Opt.help "Quiet flag, CSV/JSON only output"
        ]
      )
