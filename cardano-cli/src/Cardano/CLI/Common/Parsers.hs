module Cardano.CLI.Common.Parsers
  ( pNetworkId
  , pConsensusModeParams
  ) where

import           Cardano.Api (AnyConsensusModeParams (..), ConsensusModeParams (..),
                   EpochSlots (..), NetworkId (..), NetworkMagic (..), bounded)

import           Data.Foldable
import           Data.Word (Word64)
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

pNetworkId :: Parser NetworkId
pNetworkId = asum
  [ Opt.flag' Mainnet $ mconcat
    [ Opt.long "mainnet"
    , Opt.help "Use the mainnet magic id."
    ]
  , fmap (Testnet . NetworkMagic) $ Opt.option (bounded "TESTNET_MAGIC") $ mconcat
    [ Opt.long "testnet-magic"
    , Opt.metavar "NATURAL"
    , Opt.help "Specify a testnet magic id."
    ]
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
