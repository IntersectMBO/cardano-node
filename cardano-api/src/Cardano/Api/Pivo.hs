{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
module Cardano.Api.Pivo where

import Data.Typeable (Typeable)

import           Cardano.Api.SerialiseTextEnvelope (HasTextEnvelope, textEnvelopeType)
import           Cardano.Api.HasTypeProxy (HasTypeProxy (AsType), proxyToAsType)
import           Cardano.Api.SerialiseCBOR (SerialiseAsCBOR)

import qualified Cardano.Ledger.Pivo.Update as Pivo.Update

--------------------------------------------------------------------------------
-- Pivo.Update.Payload orphan instances
--------------------------------------------------------------------------------

instance HasTypeProxy (Pivo.Update.Payload era) where
  data AsType (Pivo.Update.Payload era) = AsPivoUpdatePayload
  proxyToAsType _ = AsPivoUpdatePayload

instance Typeable era => SerialiseAsCBOR (Pivo.Update.Payload era)

instance Typeable era => HasTextEnvelope (Pivo.Update.Payload era) where
  textEnvelopeType _ = "Pivo update payload"

data DummyPivoEra = DummyPivoEra
  deriving Typeable
