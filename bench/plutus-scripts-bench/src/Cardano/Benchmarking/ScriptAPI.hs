{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.ScriptAPI
  (BenchScript, psName, psScript, mkBenchScript)
  where

import           Prelude as Haskell (String)
import           Cardano.Api (ScriptInAnyLang)

data BenchScript
  = BenchScript
    { psName   :: String
    , psScript :: ScriptInAnyLang
    }

mkBenchScript :: String -> ScriptInAnyLang -> BenchScript
mkBenchScript = BenchScript
