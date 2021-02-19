{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Examples.TestObjects where

import           Cardano.Logging
import qualified Data.Aeson as AE
import           Data.Text (Text, pack)
import           GHC.Generics
import           Katip

data LO =
    LO1 {amount:: Int}
  | LO2 {comment:: Text}
  deriving (Generic, Show, AE.ToJSON, ToObject)

instance Logging LO where
  forHuman  LO1 {..} = pack $ "The amount is " ++ show amount
  forHuman  LO2 {..} = comment
  asMetrics LO1 {..} = [IntM (Just "amount") amount]
  asMetrics LO2 {}   = []

logObject1 :: LO
logObject1 = LO1 1

logObject2 :: LO
logObject2 = LO2 "Second"

logObject3 :: LO
logObject3 = LO1 3

logObject4 :: LO
logObject4 = LO2 "Fourth"
