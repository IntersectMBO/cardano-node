{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.PlutusExample.LockingScript
  ( apiExampleUntypedPlutusLockingScript
  , untypedLockingScriptAsShortBs
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import qualified Data.ByteString.Short as SBS
import qualified Flat

import qualified Plutus.V1.Ledger.Scripts as Plutus
import           PlutusTx (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)

{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

untypedLockingScriptAsShortBs :: SBS.ShortByteString
untypedLockingScriptAsShortBs =  SBS.toShort $ Flat.flat script

apiExampleUntypedPlutusLockingScript :: PlutusScript PlutusScriptV1
apiExampleUntypedPlutusLockingScript = PlutusScriptSerialised untypedLockingScriptAsShortBs

