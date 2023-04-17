{-# OPTIONS_GHC -Wno-partial-fields  #-}

import Data.Fixed (Pico)
import System.Time.Extra (Seconds)

import Options.Applicative

import Cardano.Tracer.Test.Acceptor


{----------------------------------------------------------------------------
  Types for Command Line Processing
----------------------------------------------------------------------------}

-- | A set of datapoints which we poll at the same period.
data DPGroup = DPG { dpg_period :: Seconds
                   , dpg_names  :: [String]
                   }
  deriving (Eq,Ord,Read,Show)
               
data Commands
  = C_Initiator
    { ekg_period   :: Pico       
    , localSockets :: [FilePath]  -- ^ we can initiate to many sockets (servers)
    , dpgroups     :: [DPGroup]
    } 
  | C_Responder 
    { ekg_period   :: Pico
    , localSocket  :: FilePath    -- ^ we serve one local socket
    , dpgroups     :: [DPGroup]
    } 
  deriving (Eq,Ord,Read,Show)

{----------------------------------------------------------------------------
  Parsers
----------------------------------------------------------------------------}

cmds :: Parser Commands
cmds = subparser
  (  command "Initiator"
       (info initiatorCmd
         (progDesc "Initiate connections to tracing servers"))
  <> command "Responder"
       (info responderCmd
         (progDesc "Respond to tracing servers"))
  )
  
pSocket :: Parser FilePath
pSocket = strOption (long "socket"
                     <> short 's'
                     <> metavar "path"
                     <> help "path to local socket"
                    )

pEkgPeriod :: Parser Pico
pEkgPeriod = option auto (long "ekgperiod"
                          <> short 'e'
                          <> metavar "P"
                          <> value (10/1)
                          <> showDefault
                          <> help "Period at which to sample EKG"
                         )

pPeriod :: Parser Double
pPeriod = option auto (long "period"
                       <> short 'p'
                       <> metavar "P"
                       <> help "Period to sample datapoints"
                       )

pDPGroup :: Parser DPGroup
pDPGroup = DPG <$> pPeriod <*> some pDataPoint
           -- FIXME: add further help.

pDataPoint :: Parser String
pDataPoint = argument str (metavar "DP")

initiatorCmd,responderCmd :: Parser Commands

initiatorCmd = C_Initiator
               <$> pEkgPeriod <*> some pSocket <*> some pDPGroup

responderCmd = C_Responder
               <$> pEkgPeriod <*> pSocket <*> some pDPGroup

{----------------------------------------------------------------------------
  Main
----------------------------------------------------------------------------}


main :: IO ()
main =
  do
  c <- execParser opts
  putStrLn $ ":DEBUG: " ++ show c

  case c of
    C_Initiator ekgp ss dps ->
      launchAcceptorsSimple Initiator ekgp ss  (map unDPG dps)
    C_Responder ekgp s  dps ->
      launchAcceptorsSimple Responder ekgp [s] (map unDPG dps)
      
  where
  unDPG (DPG x y) = (x,y)
  
  opts = info (cmds <**> helper)
    ( fullDesc
      <> progDesc "connect (as initiator or responder)"
      <> header "Client to new-tracing servers" )

