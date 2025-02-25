-- | The various Cardano protocol parameters, including:
--
-- * the current values of updatable protocol parameters: 'ProtocolParameters'
module Cardano.Api.Internal.ProtocolParameters
  ( -- * The updatable protocol parameters
    ProtocolParameters (..)
  , convertToLedgerProtocolParameters
  , toLedgerPParams
  , fromLedgerPParams
  )
where

--------------------------------------------------------------------------------

data ProtocolParameters = ProtocolParameters
  { protocolParamPrices :: Int
  , protocolParamMaxTxExUnits  :: Int
  , protocolParamCostModels :: Int
  }
convertToLedgerProtocolParameters :: ()
convertToLedgerProtocolParameters = error ""
toLedgerPParams :: ()
toLedgerPParams = error ""
fromLedgerPParams :: ()
fromLedgerPParams = error ""
