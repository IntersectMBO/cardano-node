{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Testnet.PlutusExample.PlutusVersion2.SchnorrSecp256k1Loop
    ( v2SchnorrLoopScript
    , v2SchnorrLoopScriptShortBs
    ) where

import           Cardano.Api (PlutusScript, PlutusScriptV2)
import           Cardano.Api.Shelley (PlutusScript (..))

import qualified PlutusLedgerApi.V2 as PlutusV2

import           Prelude ((.))

import           Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import qualified PlutusTx
import qualified PlutusTx.Builtins as BI
import           PlutusTx.Prelude as P hiding (Semigroup (..), unless, (.))

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum red _txContext =
  case PlutusV2.fromBuiltinData red of
    Nothing -> P.traceError "Trace error: Invalid redeemer"
    Just (n, vkey, msg, sig) ->
      if n < (1000000 :: Integer) -- large number ensures same bitsize for all counter values
      then traceError "redeemer is < 1000000"
      else loop n vkey msg sig
  where
    loop i v m s
      | i == 1000000 = ()
      | BI.verifySchnorrSecp256k1Signature v m s = loop (pred i) v m s
      | otherwise = P.traceError "Trace error: Schnorr validation failed"

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

v2SchnorrLoopScriptShortBs :: SBS.ShortByteString
v2SchnorrLoopScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

v2SchnorrLoopScript :: PlutusScript PlutusScriptV2
v2SchnorrLoopScript = PlutusScriptSerialised v2SchnorrLoopScriptShortBs
