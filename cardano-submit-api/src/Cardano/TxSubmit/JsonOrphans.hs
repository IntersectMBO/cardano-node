{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.TxSubmit.JsonOrphans () where

import Cardano.Api.TxBody ( TxId (..) )
import Cardano.Crypto.Hash.Class as Cryptos
import Data.Aeson

import qualified Shelley.Spec.Ledger.TxBody as Shelley

deriving newtype instance ToJSON TxId
deriving newtype instance ToJSON (Shelley.TxId c)
