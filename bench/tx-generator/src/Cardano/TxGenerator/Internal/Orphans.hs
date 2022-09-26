
{-# OPTIONS_GHC -fno-warn-orphans #-}

module  Cardano.TxGenerator.Internal.Orphans
        ()
        where


import           Data.Aeson (FromJSON (..))

import           Cardano.CLI.Types (SigningKeyFile (..))


instance FromJSON SigningKeyFile where
  parseJSON a = SigningKeyFile <$> parseJSON a
