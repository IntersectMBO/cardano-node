{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.ScriptAPI
  ( PlutusBenchScript
  , psName
  , psScript
  , mkPlutusBenchScript
  ) where

import           Prelude as Haskell (String)
import           Cardano.Api (ScriptInAnyLang)

data PlutusBenchScript
  = PlutusBenchScript
    { psName   :: String
    , psScript :: ScriptInAnyLang
    }

mkPlutusBenchScript :: String -> ScriptInAnyLang -> PlutusBenchScript
mkPlutusBenchScript = PlutusBenchScript
