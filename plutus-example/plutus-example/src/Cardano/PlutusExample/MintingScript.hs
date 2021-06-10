
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- TODO: Ledger currently always supplies a redeemer while the plutus-repo's
-- minting validator does not expect a redeemer. This needs to be rectified
-- before we can successfully use minting Plutus scripts.
-- TODO: We should potentially parameterize the script creation

module Cardano.PlutusExample.MintingScript
  ( apiExamplePlutusMintingScript
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS

import           Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)


{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> ScriptContext -> Bool
mkPolicy ownerPkh ctx =
    traceIfFalse "Only owner can mint" mustBeSignedBy'
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    mustBeSignedBy' :: Bool
    mustBeSignedBy' = txSignedBy info ownerPkh

policy :: PubKeyHash -> Scripts.MonetaryPolicy
policy ownerPkh = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \ownerPkh' -> Scripts.wrapMonetaryPolicy $ mkPolicy ownerPkh' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode ownerPkh

plutusScript :: Script
plutusScript =
  unMonetaryPolicyScript
    $ policy "e7dc13db93c1f56b3fd51752c14d5fdd20157334b9f0a0186d8dbd39"

validator :: Validator
validator =
  Validator . unMonetaryPolicyScript
    $ policy "e7dc13db93c1f56b3fd51752c14d5fdd20157334b9f0a0186d8dbd39"

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor


