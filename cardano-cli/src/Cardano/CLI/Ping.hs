{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Ping
  ( PingCmd(..)
  , PingCmdError(..)
  , parsePingCmd
  , runPingCmd
  , renderPingCmdError
  ) where

import           Cardano.Prelude
import           Data.String (String)

import qualified Prettyprinter as PP

import qualified Options.Applicative as Opt

data PingCmd = PingCmd
  { pingCmdCount  :: Int
  , host          :: Maybe String
  , unixSock      :: Maybe String
  , port          :: String
  , magic         :: Word32
  , json          :: Bool
  } deriving (Eq, Show)

data PingCmdError = PingCmdError

mainnetMagic :: Word32
mainnetMagic = 764824073

runPingCmd :: PingCmd -> ExceptT PingCmdError IO ()
runPingCmd _ = return ()

renderPingCmdError :: PingCmdError -> Text
renderPingCmdError _err = "TODO"

parsePingCmd :: Opt.Parser PingCmd
parsePingCmd = Opt.hsubparser $ mconcat
  [ Opt.metavar "ping"
  , Opt.command "ping" $ Opt.info pPing $ Opt.progDescDoc $ Just $ mconcat
    [ PP.pretty @String "Ping command description"
    ]
  ]

pPing :: Opt.Parser PingCmd
pPing = PingCmd
  <$> Opt.option Opt.auto
      (   Opt.long "count"
      <>  Opt.short 'c'
      <>  Opt.metavar "COUNT"
      <>  Opt.help "Number of pings to send."
      <>  Opt.value maxBound
      )
  <*> optional
      ( Opt.option Opt.auto
        (   Opt.long "host"
        <>  Opt.short 'h'
        <>  Opt.metavar "HOST"
        <>  Opt.help "Hostname/IP, e.g. relay.iohk.example."
        )
      )
  <*> optional
      ( Opt.option Opt.auto
        (   Opt.long "unixsock"
        <>  Opt.short 'u'
        <>  Opt.metavar "SOCKET"
        <>  Opt.help "Unix socket, e.g. file.socket."
        )
      )
  <*> Opt.option Opt.auto
      (   Opt.long "port"
      <>  Opt.short 'p'
      <>  Opt.metavar "PORT"
      <>  Opt.help "Port number, e.g. 1234."
      <>  Opt.value "3001"
      )
  <*> Opt.option Opt.auto
      (   Opt.long "magic"
      <>  Opt.short 'm'
      <>  Opt.metavar "MAGIC"
      <>  Opt.help "Network magic"
      <>  Opt.value mainnetMagic
      )
  <*> Opt.switch
      (   Opt.long "json"
      <>  Opt.short 'j'
      <>  Opt.help "JSON output flag"
      )
