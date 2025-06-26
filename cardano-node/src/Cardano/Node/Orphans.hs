{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans () where

import           Cardano.Api ( HasTypeProxy (..), HasTextEnvelope (..)
                             , ToCBOR (..), FromCBOR (..), SerialiseAsCBOR (..)
                             , Proxy (..))
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.Genesis (GenesisConfigFlags (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots (Flag(..))
import           Ouroboros.Consensus.Protocol.Praos.Common (PraosCredentialsSource (..))
import           Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import qualified Cardano.Crypto.KES.Class as Crypto
import           Cardano.Protocol.Crypto (StandardCrypto, KES)

import           Data.Aeson.Types
import qualified Data.Text as Text
import           Text.Printf (PrintfArg (..))
import           Data.String (IsString (..))

deriving instance Eq NodeDatabasePaths
deriving instance Show NodeDatabasePaths

instance PrintfArg SizeInBytes where
    formatArg (SizeInBytes s) = formatArg s

instance FromJSON NodeDatabasePaths where
  parseJSON o@(Object{})=
    withObject "NodeDatabasePaths"
     (\v -> MultipleDbPaths
              <$> v .: "ImmutableDbPath"
              <*> v .: "VolatileDbPath"
     ) o
  parseJSON (String s) = return . OnePathForAllDbs $ Text.unpack s
  parseJSON _ = fail "NodeDatabasePaths must be an object or a string"

deriving newtype instance FromJSON (Flag symbol)
deriving newtype instance ToJSON (Flag symbol)

instance FromJSON GenesisConfigFlags where
  parseJSON = withObject "GenesisConfigFlags" $ \v ->
    GenesisConfigFlags
      <$> v .:? "EnableCSJ"       .!= True
      <*> v .:? "EnableLoEAndGDD" .!= True
      <*> v .:? "EnableLoP"       .!= True
      <*> v .:? "BlockFetchGracePeriod"
      <*> v .:? "BucketCapacity"
      <*> v .:? "BucketRate"
      <*> v .:? "CSJJumpSize"
      <*> v .:? "GDDRateLimit"

-- TODO(11.0): move to `ouroboros-consensus`
instance ToCBOR (PraosCredentialsSource StandardCrypto) where
   toCBOR = \case
     PraosCredentialsUnsound ocert kesKey -> toCBOR (ocert, kesKey)
     PraosCredentialsAgent _path ->
       error "PraosCredentialsAgent cannot be serialized to CBOR"

-- TODO(11.0): move to `ouroboros-consensus`
instance FromCBOR (PraosCredentialsSource StandardCrypto) where
   fromCBOR = do
     (ocert, kesKey) <- fromCBOR
     pure $ PraosCredentialsUnsound ocert kesKey

-- TODO(11.0): consider moving to `cardano-api`
instance SerialiseAsCBOR (PraosCredentialsSource StandardCrypto)

-- TODO(11.0): consider moving to `cardano-api`
instance HasTypeProxy (PraosCredentialsSource StandardCrypto) where
   data AsType (PraosCredentialsSource StandardCrypto) = AsPraosCredentialsSource
   proxyToAsType _ = AsPraosCredentialsSource

-- TODO(11.0): consider moving to `cardano-api`
instance HasTextEnvelope (PraosCredentialsSource StandardCrypto) where
  textEnvelopeType _ =
    "PraosCredentialsSource_"
      <> fromString (Crypto.algorithmNameKES (Proxy @(KES StandardCrypto)))
