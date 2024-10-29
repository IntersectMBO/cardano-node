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

-- | A datatype with the same constructors as 'QueryCmds', but with a "Test" prefix and no arguments.
-- The generated type is called 'TestQueryCmds'.
$(genTestType ''QueryCmds)

-- | A list of all constructors of 'TestQueryCmds', which are nullary.
-- The generated list is called 'allTestQueryCmdsConstructors'.
$(genAllConstructorsList ''TestQueryCmds)

-- | Maps a function over all constructors of 'TestQueryCmds' and sequences the results over a monad.
forallQueryCommands :: Monad m => (TestQueryCmds -> m a) -> m ()
forallQueryCommands f = mapM_ f allTestQueryCmdsConstructors
