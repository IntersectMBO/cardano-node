{-# LANGUAGE CPP #-}

module Cardano.Tracer.Handlers.Logs.Journal
  ( module Impl
  ) where

#ifdef SYSTEMD
import           Cardano.Tracer.Handlers.Logs.Journal.Systemd as Impl
#else
import           Cardano.Tracer.Handlers.Logs.Journal.NoSystemd as Impl
#endif
