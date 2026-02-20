module Cardano.Benchmarking.ScriptAPI
  ( PlutusBenchScript
  , psName
  , psScript
  , mkPlutusBenchScriptFromCompiled
  ) where

import           Prelude as Haskell (String, ($))
import           Data.Char (isUpper)
import           Data.Maybe (fromMaybe)
import           System.FilePath (splitExtension, stripExtension, takeFileName)
import           Cardano.Api (ScriptInAnyLang, IsPlutusScriptLanguage, PlutusScriptVersion,
                   PlutusScript (..), Script (..), toScriptInAnyLang)
import qualified PlutusLedgerApi.Common as Plutus (serialiseCompiledCode)
import qualified PlutusTx (CompiledCode)

data PlutusBenchScript
  = PlutusBenchScript
    { psName   :: String
    , psScript :: ScriptInAnyLang
    }

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

-- | Create a PlutusBenchScript from a compiled PlutusCore script.
-- This eliminates boilerplate by handling script name extraction,
-- serialization, and version-polymorphic script construction.
-- The serialization is version-agnostic and works for V1, V2, and V3.
mkPlutusBenchScriptFromCompiled ::
     IsPlutusScriptLanguage lang
  => PlutusScriptVersion lang        -- ^ Plutus script version (V1, V2, or V3)
  -> String                          -- ^ Module name (from Template Haskell)
  -> PlutusTx.CompiledCode a         -- ^ Compiled Plutus code
  -> PlutusBenchScript
mkPlutusBenchScriptFromCompiled version moduleName compiledCode =
  PlutusBenchScript scriptName scriptInAnyLang
  where
    scriptName = prepareScriptName moduleName
    serializedScript = Plutus.serialiseCompiledCode compiledCode
    scriptSerialized = PlutusScriptSerialised serializedScript
    scriptInAnyLang = toScriptInAnyLang (PlutusScript version scriptSerialized)
