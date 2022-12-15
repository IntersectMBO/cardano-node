{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Cardano.Benchmarking.PlutusScripts.Loop
  ( scriptName
  , scriptSerialized
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (pred, ($), (&&), (<), (==))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx
import PlutusTx.Builtins (unsafeDataAsI)
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))


scriptName :: String
scriptName
  = $(LitE . StringL . loc_module <$> qLocation)


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

scriptSerialized :: PlutusScript PlutusScriptV1
scriptSerialized = PlutusScriptSerialised loopScriptShortBs
