{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Features
  ( Feature(..)
  , SupportedInEra(..)
  , supportedInEra
  ) where

import           Cardano.Api.Eras
import           Data.Aeson (ToJSON (..))

data ProtocolParameterUTxOCostPerWord

data ProtocolParameterUTxOCostPerByte

-- | A representation of a feature that is supported in a given era.
data Feature f where
  ProtocolParameterUTxOCostPerWord :: Feature ProtocolParameterUTxOCostPerWord
  ProtocolParameterUTxOCostPerByte :: Feature ProtocolParameterUTxOCostPerByte

deriving instance Eq (Feature f)
deriving instance Show (Feature f)

instance ToJSON (Feature f) where
  toJSON = toJSON . show

-- | A representation of a feature whether a feature is supported in a given era.
data SupportedInEra f era where
  ProtocolParameterUTxOCostPerWordSupportedInAlonzoEra  :: SupportedInEra ProtocolParameterUTxOCostPerWord AlonzoEra

  ProtocolParameterUTxOCostPerByteSupportedInBabbageEra :: SupportedInEra ProtocolParameterUTxOCostPerByte BabbageEra
  ProtocolParameterUTxOCostPerByteSupportedInConwayEra  :: SupportedInEra ProtocolParameterUTxOCostPerByte ConwayEra

deriving instance Eq   (SupportedInEra f era)
deriving instance Show (SupportedInEra f era)

instance ToJSON (SupportedInEra f era) where
  toJSON = toJSON . show

-- | Determine whether a feature is supported in a given era.
--
-- If the feature is not supported in the given era, 'Nothing' is returned.
supportedInEra
  :: Feature f
  -> CardanoEra era
  -> Maybe (SupportedInEra f era)

supportedInEra ProtocolParameterUTxOCostPerWord AlonzoEra  = Just ProtocolParameterUTxOCostPerWordSupportedInAlonzoEra

supportedInEra ProtocolParameterUTxOCostPerByte BabbageEra = Just ProtocolParameterUTxOCostPerByteSupportedInBabbageEra
supportedInEra ProtocolParameterUTxOCostPerByte ConwayEra  = Just ProtocolParameterUTxOCostPerByteSupportedInConwayEra

supportedInEra _ _  = Nothing
