module Cardano.Benchmarking.ScriptAPI
  ( PlutusBenchScript
  , psName
  , psScript
  , mkPlutusBenchScript
  ) where

import           Prelude as Haskell (String, ($))
import           Data.Char (isUpper)
import           Data.Maybe (fromMaybe)
import           System.FilePath (splitExtension, stripExtension, takeFileName)
import           Cardano.Api (ScriptInAnyLang, IsPlutusScriptLanguage,
                   PlutusScriptVersion, PlutusScript (PlutusScriptSerialised),
                   Script (PlutusScript), toScriptInAnyLang)
import qualified PlutusLedgerApi.Common as Plutus (SerialisedScript)

data PlutusBenchScript
  = PlutusBenchScript
    { psName   :: String
    , psScript :: ScriptInAnyLang
    }

-- | Create a PlutusBenchScript from a serialised PlutusCore script.
-- This eliminates boilerplate by handling script name extraction
-- and version-polymorphic script construction.
mkPlutusBenchScript ::
     IsPlutusScriptLanguage lang
  => String                   -- ^ Module name (from Template Haskell).
  -> PlutusScriptVersion lang -- ^ Calling convention / builtins (V1, V2, V3).
  -> Plutus.SerialisedScript  -- ^ Flat-encoded UPLC program.
  -> PlutusBenchScript
mkPlutusBenchScript moduleName version serializedScript =
  PlutusBenchScript scriptName scriptInAnyLang
  where
    scriptName = prepareScriptName moduleName
    scriptSerialized = PlutusScriptSerialised serializedScript
    scriptInAnyLang = toScriptInAnyLang (PlutusScript version scriptSerialized)

-- This is doing two or three sorts of normalisation at once:
-- It strips leading / -separated components, drops the ".hs" suffix
-- if present, then chooses the last . -separated component.
-- If there is a suffix different from .hs that begins with a capital
-- letter, that is returned.
-- e.g. "Data/List/System.FilePath.Text.hs" --> "Text"
--      "Data/List/System.FilePath.Text"    --> "Text"
prepareScriptName :: String -> String
prepareScriptName script
  = case splitExtension file' of
      (s, "")                      -> s     -- no dots so take it as-is
      (_, '.':s@(c:_)) | isUpper c -> s     -- take last dot-separated component
      _                            -> file' -- shouldn't happen
  where
    file  = takeFileName script -- ignore leading directories
    -- no trailing .hs so use filename as-is
    file' = fromMaybe file $ stripExtension "hs" file

