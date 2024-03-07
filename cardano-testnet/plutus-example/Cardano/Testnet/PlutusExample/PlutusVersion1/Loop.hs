{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Testnet.PlutusExample.PlutusVersion1.Loop
  ( loopScript
  , loopScriptShortBs
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Prelude hiding (pred, ($), (&&), (<), (==))

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import           PlutusTx
import           PlutusTx.Builtins (unsafeDataAsI)
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum redeemer _txContext
  = if n < 1000000
       then traceError "redeemer is < 1000000"
       else loop n
  where
    n = unsafeDataAsI redeemer
    loop i = if i == 1000000 then () else loop $ pred i

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

loopScriptShortBs :: SBS.ShortByteString
loopScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

loopScript :: PlutusScript PlutusScriptV1
loopScript = PlutusScriptSerialised loopScriptShortBs
