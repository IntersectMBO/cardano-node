{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.ErrorRender
  ( renderEraMismatch
  ) where

-- This file contains error renders. They should have been defined at a lower level, with the error
-- type definitions, but for some reason have not been.
-- They will be defined here for now and then moved where they are supposed to be once they
-- are working.

import           Data.Text (Text)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))

renderEraMismatch :: EraMismatch -> Text
renderEraMismatch EraMismatch{ledgerEraName, otherEraName} =
  "The era of the node and the tx do not match. " <>
  "The node is running in the " <> ledgerEraName <>
  " era, but the transaction is for the " <> otherEraName <> " era."

