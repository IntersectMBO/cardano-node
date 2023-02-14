{-# LANGUAGE DeriveAnyClass #-}

{- HLINT ignore "Evaluate" -}

module Cardano.Analysis.API.Dictionary (module Cardano.Analysis.API.Dictionary) where

import Cardano.Prelude

import Data.Aeson
import Data.Map.Strict qualified as M

import Cardano.Analysis.API.Field
import Cardano.Analysis.API.Types
import Cardano.Analysis.API.Metrics ()
import Cardano.Util (showText)


data DictEntry where
  DictEntry ::
    { deField       :: !Text
    , deShortDesc   :: !Text
    , deDescription :: !Text
    , deLogScale    :: !Text
    , deRange       :: !Text
    , deUnit        :: !Text
    } -> DictEntry
  deriving (Generic, FromJSON, ToJSON, Show)

data ChartArgs

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
         , deLogScale    = case fScale of
                             Log -> "true"
                             Lin -> "false"
         , deRange       = case fRange of
                             Free -> ""
                             Z0 x -> "0:" <> showText x
                             Z1 x -> "0:" <> showText x
                             R01  -> "0:1"
         , deUnit        = renderUnit fUnit
         }
