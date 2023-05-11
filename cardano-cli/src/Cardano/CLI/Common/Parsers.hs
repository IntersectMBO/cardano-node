module Cardano.CLI.Common.Parsers
  ( command'
  , pCardanoEra
  , pNetworkId
  , pConsensusModeParams
  , pSocketPath
  ) where

import           Cardano.Api (AnyCardanoEra (..), AnyConsensusModeParams (..), CardanoEra (..),
                   ConsensusModeParams (..), EpochSlots (..), File (..), NetworkId (..),
                   NetworkMagic (..), SocketPath, bounded)
import           Cardano.CLI.Environment (EnvCli (..))

import           Data.Foldable
import           Data.Maybe (maybeToList)
import           Data.Word (Word64)
import           Options.Applicative
import qualified Options.Applicative as Opt

pCardanoEra :: Parser AnyCardanoEra
pCardanoEra = asum
  [ Opt.flag' (AnyCardanoEra ByronEra)
      (  Opt.long "byron-era"
      <> Opt.help "Specify the Byron era"
      )
  , Opt.flag' (AnyCardanoEra ShelleyEra)
      (  Opt.long "shelley-era"
      <> Opt.help "Specify the Shelley era"
      )
  , Opt.flag' (AnyCardanoEra AllegraEra)
      (  Opt.long "allegra-era"
      <> Opt.help "Specify the Allegra era"
      )
  , Opt.flag' (AnyCardanoEra MaryEra)
      (  Opt.long "mary-era"
      <> Opt.help "Specify the Mary era"
      )
  , Opt.flag' (AnyCardanoEra AlonzoEra)
      (  Opt.long "alonzo-era"
      <> Opt.help "Specify the Alonzo era"
      )
  , Opt.flag' (AnyCardanoEra BabbageEra)
      (  Opt.long "babbage-era"
      <> Opt.help "Specify the Babbage era (default)"
      )
    -- Default for now:
  , pure (AnyCardanoEra BabbageEra)
  ]
command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
  mconcat
    [ command c (info (p <**> helper) $ mconcat [ progDesc descr ])
    , metavar c
    ]

pNetworkId :: EnvCli -> Parser NetworkId
pNetworkId envCli = asum $ mconcat
  [ [ Opt.flag' Mainnet $ mconcat
      [ Opt.long "mainnet"
      , Opt.help $ mconcat
        [ "Use the mainnet magic id. This overrides the CARDANO_NODE_NETWORK_ID "
        , "environment variable"
        ]
      ]
    , fmap (Testnet . NetworkMagic) $ Opt.option (bounded "TESTNET_MAGIC") $ mconcat
      [ Opt.long "testnet-magic"
      , Opt.metavar "NATURAL"
      , Opt.help $ mconcat
        [ "Specify a testnet magic id. This overrides the CARDANO_NODE_NETWORK_ID "
        , "environment variable"
        ]
      ]
    ]
  , -- Default to the network id specified by the environment variable if it is available.
    pure <$> maybeToList (envCliNetworkId envCli)
  ]

pConsensusModeParams :: Parser AnyConsensusModeParams
pConsensusModeParams = asum
  [ pShelleyMode *> pShelleyConsensusMode
  , pByronMode *> pByronConsensusMode
  , pCardanoMode *> pCardanoConsensusMode
  , pDefaultConsensusMode
  ]
  where
    pShelleyMode :: Parser ()
    pShelleyMode =
      Opt.flag' () $ mconcat
        [ Opt.long "shelley-mode"
        , Opt.help "For talking to a node running in Shelley-only mode."
        ]

    pByronMode :: Parser ()
    pByronMode =
      Opt.flag' () $ mconcat
        [ Opt.long "byron-mode"
        , Opt.help "For talking to a node running in Byron-only mode."
        ]

    pCardanoMode :: Parser ()
    pCardanoMode =
      Opt.flag' () $ mconcat
        [ Opt.long "cardano-mode"
        , Opt.help "For talking to a node running in full Cardano mode (default)."
        ]

    pCardanoConsensusMode :: Parser AnyConsensusModeParams
    pCardanoConsensusMode = AnyConsensusModeParams . CardanoModeParams <$> pEpochSlots

    pByronConsensusMode :: Parser AnyConsensusModeParams
    pByronConsensusMode = AnyConsensusModeParams . ByronModeParams <$> pEpochSlots

    pShelleyConsensusMode :: Parser AnyConsensusModeParams
    pShelleyConsensusMode = pure (AnyConsensusModeParams ShelleyModeParams)

    pDefaultConsensusMode :: Parser AnyConsensusModeParams
    pDefaultConsensusMode =
      pure . AnyConsensusModeParams . CardanoModeParams $ EpochSlots defaultByronEpochSlots

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

pEpochSlots :: Parser EpochSlots
pEpochSlots =
  fmap EpochSlots $ Opt.option (bounded "SLOTS") $ mconcat
    [ Opt.long "epoch-slots"
    , Opt.metavar "SLOTS"
    , Opt.help "The number of slots per epoch for the Byron era."
    , Opt.value defaultByronEpochSlots -- Default to the mainnet value.
    , Opt.showDefault
    ]

pSocketPath :: EnvCli -> Parser SocketPath
pSocketPath envCli =
  asum $ mconcat
    [ [ fmap File $ Opt.strOption $ mconcat
        [ Opt.long "socket-path"
        , Opt.metavar "SOCKET_PATH"
        , Opt.help $ mconcat
          [ "Path to the node socket.  This overrides the CARDANO_NODE_SOCKET_PATH "
          , "environment variable.  The argument is optional if CARDANO_NODE_SOCKET_PATH "
          , "is defined and mandatory otherwise."
          ]
        , Opt.completer (Opt.bashCompleter "file")
        ]
      ]
    , -- Default to the socket path specified by the environment variable if it is available.
      pure . File <$> maybeToList (envCliSocketPath envCli)
    ]
