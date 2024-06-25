{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This validator script is based on the Plutus benchmark
--      'Hash n bytestrings onto G2 and add points'
--  cf. https://github.com/IntersectMBO/plutus/blob/master/plutus-benchmark/bls12-381-costs/test/9.6/bls12-381-costs.golden

module Cardano.Benchmarking.PlutusScripts.HashOntoG2AndAdd (script) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV3, PlutusScriptVersion (..),
                   Script (..), toScriptInAnyLang)

import           Cardano.Benchmarking.ScriptAPI
import qualified PlutusLedgerApi.V3 as PlutusV3

import           Prelude as Haskell (String, (.), (<$>))

import qualified Data.ByteString.Short as SBS
import           GHC.ByteOrder (ByteOrder (LittleEndian))

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified PlutusTx
import qualified PlutusTx.Builtins.Internal as BI (BuiltinList, head, snd, tail, unitval,
                   unsafeDataAsConstr)
import           PlutusTx.Prelude as Tx hiding (Semigroup (..), (.), (<$>))


scriptName :: Haskell.String
scriptName
  = prepareScriptName $(LitE . StringL . loc_module <$> qLocation)

script :: PlutusBenchScript
script = mkPlutusBenchScript scriptName (toScriptInAnyLang (PlutusScript PlutusScriptV3 scriptSerialized))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinUnit
mkValidator arg =
  if red_n < 1000000 -- large number ensures same bitsize for all counter values
    then traceError "redeemer is < 1000000"
    else loop red_n red_l
  where
    -- lazily decode script context up to redeemer, which is less expensive and results in much smaller tx size
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    redeemerFollowedByScriptInfo :: BI.BuiltinList BuiltinData
    redeemerFollowedByScriptInfo = BI.tail (constrArgs arg)

    redeemer :: BuiltinData
    redeemer = BI.head redeemerFollowedByScriptInfo

    red_n :: Integer
    red_l :: [BuiltinByteString]
    (red_n, red_l) = PlutusV3.unsafeFromBuiltinData redeemer

    hashAndAddG2 :: [BuiltinByteString] -> Integer -> BuiltinBLS12_381_G2_Element
    hashAndAddG2 l i =
      go l (Tx.bls12_381_G2_uncompress Tx.bls12_381_G2_compressed_zero)
      where go [] !acc     = acc
            go (q:qs) !acc = go qs $ Tx.bls12_381_G2_add (Tx.bls12_381_G2_hashToGroup q (integerToByteString LittleEndian 0 i)) acc
    loop i l
      | i == 1000000 = BI.unitval
      | otherwise    = let !_ = hashAndAddG2 l i in loop (pred i) l

hashAndAddG2ShortBs :: SBS.ShortByteString
hashAndAddG2ShortBs = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [|| mkValidator ||])

scriptSerialized :: PlutusScript PlutusScriptV3
scriptSerialized = PlutusScriptSerialised hashAndAddG2ShortBs
