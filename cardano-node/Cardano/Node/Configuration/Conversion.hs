{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module converts `CardanoConfiguration` to `Genesis.Config`
-- TODO(KS): We need to promote the hidden module in cardano-ledger to public.

module Cardano.Node.Configuration.Conversion
  ( convertConfig
  )
where

import Cardano.Prelude

import Cardano.Binary (Raw)
-- TODO(KS): Hidden module in cardano-ledger!
--import Cardano.Chain.Genesis.Config
--  ( Config
--  , ConfigurationError(..)
--  , mkConfigFromFile
--  )
import Cardano.Crypto.Hashing (Hash, decodeAbstractHash)
import Cardano.Crypto.ProtocolMagic
  (RequiresNetworkMagic(..))
import Cardano.Node.Configuration.Types as Shell


data ConfigurationError
    = MissingConfigurationError
    | ConfigurationLoadingError Text
    deriving (Eq, Show)

-- | Converts 'CardanoConfiguration' (from 'cardano-shell') into a 'Config' (from 'cardano-ledger')
--   This function mostly relies on FromJSON parsing, because much of 'Config' originates from
--   data locked in the genesis block as JSON. Sadly when generating the 'Config' for mainnet,
--   this means that `cardano-ledger` can't benefit as much from `cardano-shell`'s function as
--   an insulating layer around config & parsing. - @mhuesch
convertConfig
  :: (MonadIO m, MonadError ConfigurationError m)
  => Shell.CardanoConfiguration
  -> m ()
--  -> m Config
convertConfig cc = do

    -- Genesis hash
    let mainnetGenFp = coGenesisFile $ ccCore cc
    gHash <- decodeGenesisHash genesisHash `wrapError` ConfigurationLoadingError
    --gHash <- decodeGenesisHash genesisHash `wrapError` GenesisHashDecodeError

    --mkConfigFromFile (cvtRNM . coRequiresNetworkMagic $ ccCore cc) mainnetGenFp gHash

    pure ()
 where
  decodeGenesisHash :: Text -> Either Text (Hash Raw)
  decodeGenesisHash genHash = decodeAbstractHash genHash

  genesisHash :: Text
  genesisHash = coGenesisHash $ ccCore cc

  cvtRNM :: Shell.RequireNetworkMagic -> RequiresNetworkMagic
  cvtRNM Shell.NoRequireNetworkMagic = RequiresNoMagic
  cvtRNM Shell.RequireNetworkMagic   = RequiresMagic
