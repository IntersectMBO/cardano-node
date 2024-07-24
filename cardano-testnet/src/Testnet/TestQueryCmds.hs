{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Testnet.TestQueryCmds
  ( TestQueryCmds(..)
  , forallQueryCommands
  ) where

import           Cardano.CLI.EraBased.Commands.Query (QueryCmds (..))

import           Testnet.TestEnumGenerator (genAllConstructorsList, genTestType)

$(genTestType ''QueryCmds)

$(genAllConstructorsList ''TestQueryCmds)

forallQueryCommands :: Monad m => (TestQueryCmds -> m a) -> m ()
forallQueryCommands f = mapM_ f allTestQueryCmdsConstructors
