{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Orphans () where

import           Cardano.Api ()

import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..))
import           Ouroboros.Network.SizeInBytes (SizeInBytes (..))

import           Data.Aeson.Types
import           Text.Printf (PrintfArg (..))

instance PrintfArg SizeInBytes where
    formatArg (SizeInBytes s) = formatArg s

instance ToJSON AcceptedConnectionsLimit where
  toJSON AcceptedConnectionsLimit
          { acceptedConnectionsHardLimit
          , acceptedConnectionsSoftLimit
          , acceptedConnectionsDelay
          } =
    object [ "AcceptedConnectionsLimit" .=
      object [ "hardLimit" .=
                  toJSON acceptedConnectionsHardLimit
             , "softLimit" .=
                  toJSON acceptedConnectionsSoftLimit
             , "delay" .=
                  toJSON acceptedConnectionsDelay
             ]
           ]

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"
