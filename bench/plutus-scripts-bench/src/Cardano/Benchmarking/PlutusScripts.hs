{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Benchmarking.PlutusScripts
    ( findPlutusScript
    , getAllScripts
    , listPlutusScripts
    ) where

import           Cardano.Api

import qualified Cardano.Benchmarking.PlutusScripts.Loop as Loop

getAllScripts ::
     [(String, ScriptInAnyLang)]
getAllScripts =
  [ (normalizeModuleName Loop.scriptName, asAnyLang Loop.scriptSerialized)
  ]

listPlutusScripts ::
     [String]
listPlutusScripts
  = fst <$> getAllScripts

findPlutusScript ::
     String
  -> Maybe ScriptInAnyLang
findPlutusScript
  = (`lookup` getAllScripts)

asAnyLang :: forall lang. IsPlutusScriptLanguage lang =>
     PlutusScript lang
  -> ScriptInAnyLang
asAnyLang script
  = toScriptInAnyLang $ PlutusScript (plutusScriptVersion @lang) script

-- "A.B.C" --> "C.hs"
normalizeModuleName ::
     String 
  -> String
normalizeModuleName
  = (++ ".hs") . reverse . takeWhile (/= '.') . reverse
