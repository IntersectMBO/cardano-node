{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}

{-|
Module      : Cardano.TxGenerator.ScriptVariants
Description : Variants of a Plutus script with distinct hashes and a chosen size.

The ledger caches decoded scripts by hash for the duration of one block, so a
benchmark in which nearby transactions use one script measures decoding only
once per block. Variants of a script give every transaction its own hash.

Variant @i@ of a base script with program @p@ is

> [(lam _ p) (delay padding_i)]

where @padding_i@ holds the variant's identifier and a generated program tree
that brings the serialised script to the requested size. The padding is
decoded like any other part of a script, but is never evaluated: the
application returns @p@ itself, so every variant behaves like the base script
and costs the same few extra execution steps.
-}
module Cardano.TxGenerator.ScriptVariants
  ( ScriptSizeSpec (..)
  , VariantIx (..)
  , plutusScriptSize
  , scriptVariant
  , scriptVariants
  ) where

import           Cardano.Api (PlutusScript (PlutusScriptSerialised), Script (PlutusScript),
                   ScriptInAnyLang (..))

import           Cardano.TxGenerator.Types (TxGenError (TxGenError))

import           Prelude

import qualified Data.ByteString as BS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import           GHC.Generics (Generic)

import qualified PlutusCore as PLC
import           PlutusLedgerApi.Common (serialiseUPLC, uncheckedDeserialiseUPLC)
import qualified UntypedPlutusCore as UPLC


-- | The size of the variants of a script, as the ledger counts it: the
-- length of the serialised script.
data ScriptSizeSpec
  = ScriptSizeMinimal
    -- ^ No padding beyond the variant identifier: a few bytes above the base script.
  | ScriptSizeFixed !Int
    -- ^ Every variant this many bytes, or a few bytes above where the encoding
    -- does not allow the exact size.
  deriving (Show, Eq, Generic)

-- | The number of a variant of a script, from 0.
newtype VariantIx = VariantIx Int deriving newtype (Enum, Eq, Ord, Show)

type Program = UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
type Term    = UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()

-- | The serialised size of a Plutus script, which is what the ledger
-- charges reference script fees for; 'Nothing' for a simple script.
plutusScriptSize :: ScriptInAnyLang -> Maybe Int
plutusScriptSize = fmap (SBS.length . fst) . either (const Nothing) Just . plutusScriptBytes

-- | Variants @0 .. count - 1@ of a Plutus script. All of them have the same
-- size, so they cost the same in fees.
scriptVariants :: ScriptSizeSpec -> Int -> ScriptInAnyLang -> Either TxGenError [ScriptInAnyLang]
scriptVariants size count script = do
  (bytes, rebuild) <- plutusScriptBytes script
  let base = uncheckedDeserialiseUPLC bytes
  shape <- paddingShape size base
  checkIdentifierRange count
  pure [ rebuild $ serialiseUPLC $ variantProgram base shape i | i <- take count [VariantIx 0 ..] ]

-- | The variant @i@ of a Plutus script, as 'scriptVariants' builds it.
scriptVariant :: ScriptSizeSpec -> ScriptInAnyLang -> VariantIx -> Either TxGenError ScriptInAnyLang
scriptVariant size script i@(VariantIx n) = do
  (bytes, rebuild) <- plutusScriptBytes script
  let base = uncheckedDeserialiseUPLC bytes
  shape <- paddingShape size base
  checkIdentifierRange (n + 1)
  pure $ rebuild $ serialiseUPLC $ variantProgram base shape i

plutusScriptBytes :: ScriptInAnyLang -> Either TxGenError (ShortByteString, ShortByteString -> ScriptInAnyLang)
plutusScriptBytes (ScriptInAnyLang lang (PlutusScript version (PlutusScriptSerialised bytes)))
  = Right (bytes, ScriptInAnyLang lang . PlutusScript version . PlutusScriptSerialised)
plutusScriptBytes _
  = Left $ TxGenError "ScriptVariants: only Plutus scripts have variants"

-- The variant identifiers are offset, so that all of them take the same number
-- of bits to encode, and with them all variants the same number of bytes.
identifierOffset, identifierLimit :: Int
identifierOffset = 1_048_576   -- 2^20
identifierLimit  = 67_108_864  -- 2^26

checkIdentifierRange :: Int -> Either TxGenError ()
checkIdentifierRange count
  | count < 0 || count > identifierLimit - identifierOffset
  = Left $ TxGenError $ "ScriptVariants: unsupported number of variants: " ++ show count
  | otherwise = Right ()

-- | How much padding a variant carries: a number of generated chunks of
-- program, and the length of a filler bytestring for the last few bytes.
data PaddingShape = PaddingShape !Int !Int

variantProgram :: Program -> PaddingShape -> VariantIx -> Program
variantProgram (UPLC.Program () version body) (PaddingShape chunks fillerLength) (VariantIx i) =
  UPLC.Program () version $
    UPLC.Apply () (UPLC.LamAbs () binder body) (UPLC.Delay () padding)
 where
  -- 'body' is closed, so binding a variable around it shifts none of its indices.
  padding = balanced $
      integer (fromIntegral $ identifierOffset + i)
    : bytestring (BS.replicate fillerLength 0xab)
    : map chunk [0 .. chunks - 1]

paddingShape :: ScriptSizeSpec -> Program -> Either TxGenError PaddingShape
paddingShape ScriptSizeMinimal _ = Right (PaddingShape 0 0)
paddingShape (ScriptSizeFixed target) base
  | target <= sizeOf (PaddingShape 0 0) = Right (PaddingShape 0 0)
  | otherwise = Right (PaddingShape chunks (fillerFor chunks))
 where
  sizeOf shape = SBS.length $ serialiseUPLC $ variantProgram base shape (VariantIx 0)
  minimal = sizeOf (PaddingShape 0 0)
  perChunk :: Double
  perChunk = fromIntegral (sizeOf (PaddingShape 64 0) - minimal) / 64
  -- Chunks fill all but the last few bytes, which the filler tops up.
  chunks = shrink $ max 0 $ floor (fromIntegral (target - minimal - 16) / perChunk)
  shrink n
    | n > 0 && sizeOf (PaddingShape n 0) > target - 4 = shrink (n - 1 - n `div` 32)
    | otherwise = max 0 n
  -- The smallest filler that reaches the target; size grows with the filler.
  fillerFor n = search 0 target
   where
    search lo hi
      | lo >= hi = lo
      | sizeOf (PaddingShape n mid) >= target = search lo mid
      | otherwise = search (mid + 1) hi
     where mid = (lo + hi) `div` 2

-- | A closed piece of program, mixing the kinds of terms compiled scripts are
-- made of: lambdas, applications, variables, builtins and constants.
chunk :: Int -> Term
chunk k = case k `mod` 4 of
  0 -> lam $ apply2 (builtin PLC.AddInteger) (var 1) (integer $ fromIntegral k)
  1 -> lam $ apply2 (builtin PLC.AppendByteString) (var 1) (bytestring $ BS.pack $ map fromIntegral [k .. k + 7])
  2 -> lam $ lam $ apply2 (builtin PLC.EqualsInteger) (var 2) (var 1)
  _ -> UPLC.Apply () (builtin PLC.Sha2_256) (bytestring $ BS.pack $ map fromIntegral [k .. k + 15])
 where
  lam = UPLC.LamAbs () binder
  apply2 f x = UPLC.Apply () (UPLC.Apply () f x)

-- | Combines terms into a balanced tree of applications, as deep as a
-- compiled script rather than one long spine.
balanced :: [Term] -> Term
balanced []  = integer 0
balanced [t] = t
balanced ts  = UPLC.Apply () (balanced l) (balanced r)
 where (l, r) = splitAt (length ts `div` 2) ts

binder :: UPLC.DeBruijn
binder = UPLC.DeBruijn 0

-- | A variable, counting binders outwards from 1.
var :: Integer -> Term
var = UPLC.Var () . UPLC.DeBruijn . fromIntegral

builtin :: UPLC.DefaultFun -> Term
builtin = UPLC.Builtin ()

integer :: Integer -> Term
integer = UPLC.Constant () . PLC.someValue

bytestring :: BS.ByteString -> Term
bytestring = UPLC.Constant () . PLC.someValue
