{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Cardano.Api.Block
import qualified Cardano.Api.Block as Block
import           Cardano.Api.Eras (ShelleyLedgerEra)
import qualified Cardano.Crypto.Hash as Crypto
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
-- import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Cardano.Api.IPC as IPC
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update
import           Cardano.Crypto.Hash (ByteString)
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import           Control.Monad.Except
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron
import qualified Ouroboros.Consensus.Cardano as Cardano
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import qualified Ouroboros.Consensus.Cardano.Node as Cardano
import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Ledger.Abstract (tickThenApply)
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.Extended as Ledger
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley
import qualified Ouroboros.Consensus.Shelley.Protocol as Consensus

--------------------------------------------------------------------------------
-- TODO move this stuff to a new ledger state module in cardano-api and also
-- export it from Cardano.Api
--------------------------------------------------------------------------------

data LedgerState = LedgerState
  { lsConfig :: !(Ledger.ExtLedgerCfg (Cardano.HardForkBlock (Cardano.CardanoEras Cardano.StandardCrypto)))
  , lsState :: !(Ledger.ExtLedgerState (Cardano.CardanoBlock Cardano.StandardCrypto))
  }

data Config = Config
  !Genesis.Config
  -- !(Shelley.ShelleyGenesis StandardShelley)
  -- !GenesisHashShelley

-- TODO what do we want to expose? Note that we already have some typed in
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

applyBlock :: LedgerState -> Block era -> LedgerState
applyBlock (LedgerState config extLS) block = case block of
  ShelleyBlock shelleyEra shelleyBlock -> case shelleyEra of
    ShelleyBasedEraShelley -> go (Cardano.BlockShelley shelleyBlock)
    ShelleyBasedEraAllegra -> go (Cardano.BlockAllegra shelleyBlock)
    ShelleyBasedEraMary -> go (Cardano.BlockMary shelleyBlock)
  ByronBlock byronBlock -> go (Cardano.BlockByron byronBlock)
  where
    go :: Cardano.CardanoBlock Cardano.StandardCrypto -> LedgerState
    go x = LedgerState config
            $ either (error . show) id
            $ runExcept
            $ tickThenApply -- TODO validation check as is done in `Cardano.DbSync.LedgerState` `applyBlock`
                config
                x
                extLS

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

data GenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
  | GenesisDecodeError !FilePath !Text

-- | The easy way to get the initial ledger state. Adapte from `ouroboros-network/ouroboros-consensus-cardano/tools/db-analyser/Block/Byron.hs`
initialLedgerState
  :: FilePath -- ^ The genesis config file. See `cardano-ledger-specs`'s `mainnet-genesis.json`.
  -- -> Maybe (Hash Raw)
  -> RequiresNetworkMagic
  -> ExceptT GenesisError IO LedgerState
initialLedgerState configFile requiresNetworkMagic = do
  let shellyGenesisFile :: FilePath
      shellyGenesisFile = undefined
  let explicitGenesisHash = Nothing -- TODO make this a parameter?


  -- copied from readDbSyncNodeConfig / readCardanoGenesisConfig
  undefined

  -- Get the Shelley configuration
  -- copied from readShelleyGenesisConfig
  (shelleyGenesis, shelleyGenesisHash) <- do
    -- copied from Cardano.DbSync.Config.Shelley.readGenesis
    content <- handleIOExceptT (GenesisReadError shellyGenesisFile . Text.pack . show) $ BS.readFile shellyGenesisFile
    let genesisHash = GenesisHashShelley (Crypto.hashWith id content)
    -- TODO checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError shellyGenesisFile . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' @(Cardano.ShelleyGenesis Cardano.StandardShelley) content
    return (genesis, genesisHash)

  -- Get the Byron configuration
  -- copied from readByronGenesisConfig
  byronGenesis <- do
    genesisHash <- case explicitGenesisHash of
      Nothing -> firstExceptT (GenesisReadError configFile . Text.pack . show)
        (Genesis.unGenesisHash . snd <$> Genesis.readGenesisData configFile)
      Just hash -> return hash
    firstExceptT
      (GenesisReadError configFile . Text.pack . show)
      (Genesis.mkConfigFromFile
        requiresNetworkMagic
        configFile
        genesisHash
      )


  let -- See db-sync's `DbSyncNodeConfig`
      dncPBftSignatureThreshold = undefined

      dncByronProtocolVersion :: Cardano.Chain.Update.ProtocolVersion
      dncByronProtocolVersion = undefined

      dncByronSoftwareVersion = undefined
      shelleyProtVer = Cardano.ProtVer
                          (fromIntegral $ Cardano.Chain.Update.pvMajor dncByronProtocolVersion)
                          (fromIntegral $ Cardano.Chain.Update.pvMinor dncByronProtocolVersion)
      dncByronToShelley :: Cardano.ProtocolParamsTransition Byron.ByronBlock (Shelley.ShelleyBlock Cardano.StandardShelley)

      -- TODO should these be loaded from a config file? See cardano-node/doc/getting-started/understanding-config-files.md
      dncByronToShelley = Cardano.ProtocolParamsTransition $ Cardano.TriggerHardForkAtVersion 2
      dncShelleyToAllegra = Cardano.ProtocolParamsTransition $ Cardano.TriggerHardForkAtVersion 3
      dncAllegraToMary = Cardano.ProtocolParamsTransition $ Cardano.TriggerHardForkAtVersion 4

      config = Ledger.ExtLedgerCfg
        $ Consensus.pInfoConfig
        $ Cardano.protocolInfo @IO
        $ -- see cardano-db-sync/cardano-db-sync/src/Cardano/DbSync/Config/Cardano.hs `mkProtocolCardano`
          Cardano.ProtocolCardano
            Cardano.ProtocolParamsByron
              { Cardano.byronGenesis = byronGenesis
              , Cardano.byronPbftSignatureThreshold = Cardano.PBftSignatureThreshold <$> dncPBftSignatureThreshold
              , Cardano.byronProtocolVersion = dncByronProtocolVersion
              , Cardano.byronSoftwareVersion = dncByronSoftwareVersion
              , Cardano.byronLeaderCredentials = Nothing
              }
            Cardano.ProtocolParamsShelleyBased
              { Cardano.shelleyBasedGenesis = shelleyGenesis
              , Cardano.shelleyBasedInitialNonce = Cardano.Nonce (Crypto.castHash . unGenesisHashShelley $ shelleyGenesisHash)
              , Cardano.shelleyBasedLeaderCredentials = [] -- TODO is this correct?
              }
            Cardano.ProtocolParamsShelley
              { Cardano.shelleyProtVer = shelleyProtVer
              }
            Cardano.ProtocolParamsAllegra
              { Cardano.allegraProtVer = shelleyProtVer
              }
            Cardano.ProtocolParamsMary
              { Cardano.maryProtVer = shelleyProtVer
              }
            dncByronToShelley
            dncShelleyToAllegra
            dncAllegraToMary
  return (LedgerState config undefined)

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

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

renderShelleyGenesisError :: GenesisError -> Text
renderShelleyGenesisError sge =
    case sge of
      GenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      GenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Shelley genesis file: the actual hash is ", renderHash actual
          , ", but the expected Shelley genesis hash given in the node "
          , "configuration file is ", renderHash expected, "."
          ]

      GenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]
  where
    renderHash :: GenesisHashShelley -> Text
    renderHash (GenesisHashShelley h) = Text.decodeUtf8 $ Base16.encode (Crypto.hashToBytes h)
