{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Benchmarking.Plutus.PlutusInclude
    ( includePlutusScript
    , listPlutusScripts
    ) where

import           Cardano.Api

import           Cardano.Benchmarking.Plutus.BenchCustomCall

library ::
    [(String, ScriptInAnyLang)]
library =
  [ ("BenchCustomCall.hs", asAnyLang customCallScript)
  ]

listPlutusScripts ::
    [String]
listPlutusScripts
  = map fst library

includePlutusScript ::
    String -> Maybe ScriptInAnyLang
includePlutusScript
  = (`lookup` library)

asAnyLang :: forall lang. IsPlutusScriptLanguage lang =>
     PlutusScript lang
  -> ScriptInAnyLang
asAnyLang script
  = toScriptInAnyLang $ PlutusScript (plutusScriptVersion @lang) script
