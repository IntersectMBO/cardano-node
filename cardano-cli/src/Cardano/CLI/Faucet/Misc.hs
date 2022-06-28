{-# LANGUAGE ImportQualifiedPost #-}

module Cardano.CLI.Faucet.Misc where

import Cardano.Api (ConsensusModeParams(CardanoModeParams), CardanoMode, EpochSlots(EpochSlots), AddressAny, parseAddressAny, Lovelace, TxOutValue(TxOutAdaOnly, TxOutValue), valueToLovelace, CardanoEra, EraInMode, toEraInMode, ConsensusMode(CardanoMode))
import Cardano.Prelude
import Cardano.CLI.Faucet.Types
import Data.Text qualified as T
import Text.Parsec
import Control.Monad.Trans.Except.Extra (left)

getValue :: TxOutValue era -> Lovelace
getValue (TxOutAdaOnly _ ll) = ll
getValue (TxOutValue _ val) = fromMaybe 0 (valueToLovelace val)

parseAddress :: Text -> ExceptT FaucetError IO AddressAny
parseAddress addr = case parse (parseAddressAny <* eof) "" (T.unpack addr) of
  Right a -> return $ a
  Left e -> left $ FaucetErrorInvalidAddress addr e

defaultCModeParams :: ConsensusModeParams CardanoMode
defaultCModeParams = CardanoModeParams (EpochSlots defaultByronEpochSlots)

defaultByronEpochSlots :: Word64
defaultByronEpochSlots = 21600

convertEra :: Monad m => CardanoEra era -> ExceptT FaucetError m (EraInMode era CardanoMode)
convertEra era = case (toEraInMode era CardanoMode) of
  Just eraInMode -> pure eraInMode
  Nothing -> left $ FaucetErrorEraConversion
