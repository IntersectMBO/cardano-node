{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Benchmarking.PlutusScripts
    ( encodePlutusScript
    , findPlutusScript
    , getAllScripts
    , listPlutusScripts
    , asAnyLang
    , normalizeModuleName
    ) where

import           Prelude

import           Data.ByteString.Lazy as LBS (ByteString)
import           Data.List (find)
import           Data.Text (pack, split)
import           System.FilePath (takeBaseName)

import           Cardano.Api

import qualified Cardano.Benchmarking.PlutusScripts.CustomCall as CustomCall
import qualified Cardano.Benchmarking.PlutusScripts.EcdsaSecp256k1Loop as ECDSA
import qualified Cardano.Benchmarking.PlutusScripts.Loop as Loop
import qualified Cardano.Benchmarking.PlutusScripts.LoopV3 as LoopV3
import qualified Cardano.Benchmarking.PlutusScripts.SchnorrSecp256k1Loop as Schnorr
import           Cardano.Benchmarking.ScriptAPI


getAllScripts :: [PlutusBenchScript]
getAllScripts =
  [ CustomCall.script
  , ECDSA.script
  , Loop.script
  , LoopV3.script
  , Schnorr.script
  ]

listPlutusScripts ::
     [String]
listPlutusScripts
  = psName <$> getAllScripts

findPlutusScript ::
     String
  -> Maybe ScriptInAnyLang
findPlutusScript s
  =   psScript
  <$> find (\x -> last (split (=='.') . pack . psName $ x) == s') getAllScripts
  where
    s' = pack $ takeBaseName s

encodePlutusScript ::
     ScriptInAnyLang
  -> LBS.ByteString
encodePlutusScript
  = \case
    ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) s -> textEnvelopeToJSON Nothing s
    ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) s -> textEnvelopeToJSON Nothing s
    ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) s -> textEnvelopeToJSON Nothing s
    _                                                       -> "{}"


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
