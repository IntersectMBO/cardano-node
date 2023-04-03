module Cardano.CLI.Common.Parsers
  ( pNetworkId
  ) where

import           Cardano.Api (NetworkId (..), NetworkMagic (..), bounded)

import           Data.Foldable
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
