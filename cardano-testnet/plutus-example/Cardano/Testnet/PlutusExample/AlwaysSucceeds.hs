{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Testnet.PlutusExample.AlwaysSucceeds
  ( alwaysSucceedsScript
  , alwaysSucceedsScriptShortBs
  ) where

import qualified Cardano.Api.Shelley as Api

import           Prelude hiding (($))

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Plutus.Script.Utils.Scripts as Plutus
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

alwaysSucceedsScriptShortBs :: SBS.ShortByteString
alwaysSucceedsScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

alwaysSucceedsScript :: Api.PlutusScript lang
alwaysSucceedsScript = Api.PlutusScriptSerialised alwaysSucceedsScriptShortBs

