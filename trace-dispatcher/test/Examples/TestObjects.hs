{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Examples.TestObjects where

import qualified Data.Aeson as AE
import           GHC.Generics
import           Katip

data LO =
    LO1 {amount:: Int}
  | LO2 {comment:: String}
  deriving (Generic, Show, AE.ToJSON, ToObject)

instance LogItem LO where
  payloadKeys _ _ = AllKeys

logObject1 :: LO
logObject1 = LO1 1

logObject2 :: LO
logObject2 = LO2 "Second"

logObject3 :: LO
logObject3 = LO1 3

logObject4 :: LO
logObject4 = LO2 "Fourth"
