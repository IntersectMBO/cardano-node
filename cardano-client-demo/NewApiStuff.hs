{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module NewApiStuff
  (
  applyBlock,
  LedgerState,
  initialLedgerState,
  RequiresNetworkMagic(..),

  ) where

import           Cardano.Api
import qualified Cardano.Api.Block as Block
import           Cardano.Api.Eras (ShelleyLedgerEra)
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Control.Monad.Except
import           Ouroboros.Consensus.Byron.Ledger.Block
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron
import           Ouroboros.Consensus.Ledger.Abstract (tickThenApply)
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley

--------------------------------------------------------------------------------
-- TODO move this stuff to a new ledger state module in cardano-api and also
-- export it from Cardano.Api
--------------------------------------------------------------------------------

data LedgerState era where

  ByronLedgerState
    :: Genesis.Config
    -> Ledger.LedgerState ByronBlock
    -> LedgerState ByronEra

  ShelleyLedgerState
    :: Shelley.ShelleyLedgerConfig (ShelleyLedgerEra era)
    -> ShelleyBasedEra era
    -> Ledger.LedgerState (Shelley.ShelleyBlock (ShelleyLedgerEra era))
    -> LedgerState era

-- TODO what do we wan to expose? Note that we already have some typed in
-- Cardano.Api.Query to handle UTxOs.
--
-- pattern LedgerState :: UTxO -> LedgerState ByronEra pattern LedgerState utxo
-- <- (deconstructLedgerState -> utxo)
--
-- deconstructLedgerState :: LedgerState era -> UTxO
-- deconstructLedgerState ledgerState = case ledgerState of
--   ByronLedgerState
--     _
--     (Byron.ByronLedgerState
--       (_ :: Ouroboros.Consensus.Block.WithOrigin BlockNo)
--       (Cardano.Chain.Block.ChainValidationState
--         _
--         _
--         utxo
--         _
--         _
--       )
--       (_ :: Byron.ByronTransition)
--     )
--     -> utxo

applyBlock :: forall era . LedgerState era -> Block era -> LedgerState era
applyBlock ls apiBlock = case ls of
  ByronLedgerState config byronLs -> case apiBlock of
    Block.ByronBlock byronBlock -> ByronLedgerState config
      $ either (error . show) id
      $ runExcept
      $ tickThenApply -- TODO validation check as is done in `Cardano.DbSync.LedgerState` `applyBlock`
          config
          byronBlock
          byronLs
    Block.ShelleyBlock _ _ -> undefined -- How do we apply a shelley block to a byron ledger state?
  ShelleyLedgerState config lsEra shelleyLs -> case apiBlock of
    Block.ByronBlock byronBlock -> undefined -- How do we apply a byron block to a shelley ledger state?
    Block.ShelleyBlock blkEra shelleyBlock -> case lsEra of
      ShelleyBasedEraMary -> undefined
      ShelleyBasedEraShelley -> undefined
      ShelleyBasedEraAllegra -> goShelley blkEra
      where
        goShelley :: (Shelley.ShelleyBasedEra (ShelleyLedgerEra era), IsShelleyBasedEra era) => ShelleyBasedEra era -> LedgerState era
        goShelley era = ShelleyLedgerState config era
            $ either (error . show) id
            $ runExcept
            $ tickThenApply -- TODO validation check as is done in `Cardano.DbSync.LedgerState` `applyBlock`
                config
                shelleyBlock
                shelleyLs

  -- TODO Byron config
  --
  --
  -- db-sync gets this in Cardano.DbSync.runDbSyncNode
  --
  --    enc <- readDbSyncNodeConfig (enpConfigFile enp)
  --    genCfg :: GenesisConfig <- readCardanoGenesisConfig enc
  --
  -- data GenesisConfig = GenesisCardano !DbSyncNodeConfig !Byron.Config !ShelleyConfig
  --
  -- (enp :: DbSyncNodeParams) is an arg parsed from the command line arguments
  -- in cardano-db-sync.hs
  --
  --
  -- readCardanoGenesisConfig :: DbSyncNodeConfig -> ExceptT DbSyncNodeError IO GenesisConfig
  -- readCardanoGenesisConfig enc =
  --   ...
  --   GenesisCardano enc <$> readByronGenesisConfig enc <*> readShelleyGenesisConfig enc
  --
  -- readByronGenesisConfig :: DbSyncNodeConfig -> ExceptT DbSyncNodeError IO Byron.Config
  -- readByronGenesisConfig enc = do
  --   let file = unGenesisFile $ dncByronGenesisFile enc
  --   genHash <- firstExceptT NEError
  --                 . hoistEither
  --                 $ decodeAbstractHash (unGenesisHashByron $ dncByronGenesisHash enc)
  --   firstExceptT (NEByronConfig file)
  --                 $ Byron.mkConfigFromFile (dncRequiresNetworkMagic enc) file genHash
  --
  --Cardano.Chain.Genesis.Config
  --
  -- -- | Construct a 'Config' from an external genesis file.
  -- --
  -- -- The 'FilePath' refers to a canonical JSON file. It will be hashed and
  -- -- checked against the expected hash, which should be known from config.
  -- --
  -- mkConfigFromFile
  --   :: (MonadError ConfigurationError m, MonadIO m)
  --   => RequiresNetworkMagic
  --   -> FilePath
  --   -> Hash Raw  -- ^ The expected hash of the file
  --   -> m Config
  -- mkConfigFromFile rnm fp expectedHash
  --
  --
  -- OK! Turns out there is a much simpler function! see `readConfigFile` below
  --
  --

-- | The easy way to get the initial ledger state. Adapte from `ouroboros-network/ouroboros-consensus-cardano/tools/db-analyser/Block/Byron.hs`
initialLedgerState
  :: FilePath -- ^ The genesis config file. See `cardano-ledger-specs`'s `mainnet-genesis.json`.
  -- -> Maybe (Hash Raw)
  -> RequiresNetworkMagic
  -> IO (LedgerState ByronEra)
initialLedgerState configFile requiresNetworkMagic = do
    let explicitGenesisHash = Nothing -- TODO make this a parameter?
    genesisHash <- case explicitGenesisHash of
      Nothing -> either (error . show) return =<< runExceptT
        (Genesis.unGenesisHash . snd <$> Genesis.readGenesisData configFile)
      Just hash -> return hash
    genesisConfig <- either (error . show) return =<< runExceptT
      (Genesis.mkConfigFromFile
        requiresNetworkMagic
        configFile
        genesisHash)
    return $ ByronLedgerState
      genesisConfig
      (Byron.initByronLedgerState
                            genesisConfig
                            Nothing -- don't override UTxO
      )

--Ouroboros.Consensus.Byron.Ledger.Ledger
--
-- data instance LedgerState ByronBlock = ByronLedgerState {
--       byronLedgerTipBlockNo :: !(WithOrigin BlockNo)     -- data WithOrigin t = Origin | At !t
--     , byronLedgerState      :: !CC.ChainValidationState
--     , byronLedgerTransition :: !ByronTransition
--     }
--   deriving (Eq, Show, Generic, NoThunks)
--
-- applyByronBlock :: Cardano.Chain.ValidationMode.ValidationMode
--                 -> LedgerConfig ByronBlock
--                 -> ByronBlock
--                 -> TickedLedgerState ByronBlock
--                 -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
--
-- -- | The ticked Byron ledger state
-- data instance Ticked (LedgerState ByronBlock) = TickedByronLedgerState {
--       tickedByronLedgerState        :: !CC.ChainValidationState
--     , untickedByronLedgerTransition :: !ByronTransition
--     }
--   deriving (Generic, NoThunks)
--
-- type instance LedgerCfg (LedgerState ByronBlock) = Cardano.Chain.Genesis.Config

--Ouroboros.Consensus.Ledger.Basics
--
-- type LedgerConfig      blk = LedgerCfg (LedgerState blk)
-- type LedgerError       blk = LedgerErr (LedgerState blk)
-- type TickedLedgerState blk = Ticked    (LedgerState blk)

--Cardano.Chain.ValidationMode
--
-- data ValidationMode = ValidationMode
--     { blockValidationMode :: !BlockValidationMode
--     , txValidationMode    :: !TxValidationMode
--     } deriving (Show)

--Cardano.Chain.Block.Validation
--
-- data ChainValidationState = ChainValidationState
--   { cvsLastSlot        :: !SlotNumber
--   , cvsPreviousHash    :: !(Either GenesisHash HeaderHash)
--   -- ^ GenesisHash for the previous hash of the zeroth boundary block and
--   --   HeaderHash for all others.
--   , cvsUtxo            :: !UTxO
--   , cvsUpdateState     :: !UPI.State
--   , cvsDelegationState :: !DI.State
--   } deriving (Eq, Show, Generic, NFData, NoThunks)



--Ouroboros.Consensus.Byron.Ledger.Ledger
--
-- -- | Information required to determine the transition from Byron to Shelley
-- data ByronTransition =
--     -- | Per candidate proposal, the 'BlockNo' in which it became a candidate
--     --
--     -- The HFC needs to know when a candidate proposal becomes stable. We cannot
--     -- reliably do this using 'SlotNo': doing so would mean that if we were to
--     -- switch to a denser fork, something that was previously deemed stable is
--     -- suddenly not deemed stable anymore (although in actuality it still is).
--     -- We therefore must do this based on 'BlockNo' instead, but unfortunately
--     -- the Byron ledger does not record this information. Therefore, we record
--     -- it here instead.
--     --
--     -- Invariant: the domain of this map should equal the set of candidate
--     -- proposals.
--     ByronTransitionInfo !(Map Update.ProtocolVersion BlockNo)
--   deriving (Eq, Show, Generic, NoThunks)
