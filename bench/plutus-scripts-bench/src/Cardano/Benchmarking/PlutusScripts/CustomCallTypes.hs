{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Benchmarking.PlutusScripts.CustomCallTypes
where

import qualified PlutusTx
import           PlutusTx.Prelude as Plutus hiding (Semigroup (..), (.))
import           Prelude as Haskell (Eq, Show)


-- this alias describes what the CustomCall script
-- expects both as datum and as redeemer
type CustomCallArg = (CustomCallCommand, [CustomCallData])

data CustomCallCommand
    = EvalSpine
    | EvalValues
    | EvalAndValidate
    deriving (Haskell.Eq, Haskell.Show)

data CustomCallData
    = CCNone
    | CCInteger Integer
    | CCByteString BuiltinByteString
    | CCSum Integer [Integer]
    | CCConcat BuiltinByteString [BuiltinByteString]
    deriving (Haskell.Eq, Haskell.Show)


PlutusTx.unstableMakeIsData ''CustomCallCommand
PlutusTx.unstableMakeIsData ''CustomCallData
