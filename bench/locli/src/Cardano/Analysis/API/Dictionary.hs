{-# LANGUAGE DeriveAnyClass #-}
module Cardano.Analysis.API.Dictionary (module Cardano.Analysis.API.Dictionary) where

import Cardano.Prelude

import Data.Aeson
import Data.Map.Strict qualified as M

import Cardano.Analysis.API.Field
import Cardano.Analysis.API.Types
import Cardano.Analysis.API.Metrics ()


data DictEntry where
  DictEntry ::
    { deField       :: !Text
    , deShortDesc   :: !Text
    , deDescription :: !Text
    } -> DictEntry
  deriving (Generic, FromJSON, ToJSON, Show)

data Dictionary where
  Dictionary ::
    { dBlockProp   :: !(Map Text DictEntry)
    , dClusterPerf :: !(Map Text DictEntry)
    } -> Dictionary
  deriving (Generic, FromJSON, ToJSON, Show)

metricDictionary :: Dictionary
metricDictionary =
  Dictionary
  { dBlockProp   = cdfFields @BlockProp <&> extractEntry & M.fromList
  , dClusterPerf = cdfFields @MachPerf  <&> extractEntry & M.fromList
  }
 where extractEntry :: Field DSelect p a -> (Text, DictEntry)
       extractEntry Field{..} = (fId,) $
         DictEntry
         { deField       = fId
         , deShortDesc   = fShortDesc
         , deDescription = fDescription
         }
