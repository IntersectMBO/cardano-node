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
  (CardanoLedgerState(..), initLedgerStateVar)
  where

import           Data.ByteString (ByteString)
import           Data.Text (Text)

import qualified Cardano.Api.Block
import qualified Cardano.BM.Configuration.Model as BM
import qualified Cardano.BM.Data.Configuration as BM
import qualified Cardano.Chain.Genesis
import qualified Cardano.Chain.Update
import qualified Cardano.Crypto
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import           GHC.Conc
import qualified Ouroboros.Consensus.Byron.Ledger.Block
import qualified Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as C
import qualified Ouroboros.Consensus.Cardano.Block
import qualified Ouroboros.Consensus.Cardano.Block as C
import qualified Ouroboros.Consensus.Cardano.CanHardFork
import qualified Ouroboros.Consensus.Config as C
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.Ledger.Extended as C
import qualified Ouroboros.Consensus.Node.ProtocolInfo
import qualified Ouroboros.Consensus.Shelley.Eras
import qualified Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Shelley.Spec.Ledger.BaseTypes
import qualified Shelley.Spec.Ledger.Genesis
import qualified Shelley.Spec.Ledger.PParams

data CardanoLedgerState = CardanoLedgerState
  { clsState :: !(C.ExtLedgerState (C.CardanoBlock C.StandardCrypto))
  , clsConfig :: !(C.TopLevelConfig (C.CardanoBlock C.StandardCrypto))
  }

newtype LedgerStateVar = LedgerStateVar
  { unLedgerStateVar :: TVar CardanoLedgerState
  }

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano !DbSyncNodeConfig !Cardano.Chain.Genesis.Config !ShelleyConfig

data ShelleyConfig = ShelleyConfig
  { scConfig :: !(Shelley.Spec.Ledger.Genesis.ShelleyGenesis Ouroboros.Consensus.Shelley.Eras.StandardShelley)
  , scGenesisHash :: !GenesisHashShelley
  }


data DbSyncNodeConfig = DbSyncNodeConfig
  { dncNetworkName :: !NetworkName
  , dncLoggingConfig :: !BM.Configuration
  , dncNodeConfigFile :: !NodeConfigFile
  , dncProtocol :: !DbSyncProtocol
  , dncRequiresNetworkMagic :: !Cardano.Crypto.RequiresNetworkMagic
  , dncEnableLogging :: !Bool
  , dncEnableMetrics :: !Bool
  , dncPBftSignatureThreshold :: !(Maybe Double)
  , dncByronGenesisFile :: !GenesisFile
  , dncByronGenesisHash :: !GenesisHashByron
  , dncShelleyGenesisFile :: !GenesisFile
  , dncShelleyGenesisHash :: !GenesisHashShelley
  , dncByronSoftwareVersion :: !Cardano.Chain.Update.SoftwareVersion
  , dncByronProtocolVersion :: !Cardano.Chain.Update.ProtocolVersion

  , dncShelleyHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  , dncAllegraHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork
  , dncMaryHardFork :: !Ouroboros.Consensus.Cardano.CanHardFork.TriggerHardFork

  , dncByronToShelley :: !ByronToShelley
  , dncShelleyToAllegra :: !ShelleyToAllegra
  , dncAllegraToMary :: !AllegraToMary
  }

-- May have other constructors when we are preparing for a HFC event.
data DbSyncProtocol
  = DbSyncProtocolCardano
  deriving Show

type ByronToShelley =
  C.ProtocolParamsTransition Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)

type ShelleyToAllegra =
  C.ProtocolParamsTransition
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra)

type AllegraToMary =
  C.ProtocolParamsTransition
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra)
    (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary)

data DbSyncPreConfig = DbSyncPreConfig
  { pcNetworkName :: !NetworkName
  , pcLoggingConfig :: !BM.Representation
  , pcNodeConfigFile :: !NodeConfigFile
  , pcEnableLogging :: !Bool
  , pcEnableMetrics :: !Bool
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  } deriving Show

newtype LogFileDir
  = LogFileDir FilePath

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NodeConfigFile = NodeConfigFile
  { unNodeConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

initLedgerStateVar :: GenesisConfig -> IO LedgerStateVar
initLedgerStateVar genesisConfig =
  fmap LedgerStateVar . newTVarIO $
    CardanoLedgerState
      { clsState = Ouroboros.Consensus.Node.ProtocolInfo.pInfoInitLedger protocolInfo
      , clsConfig = Ouroboros.Consensus.Node.ProtocolInfo.pInfoConfig protocolInfo
      }
  where
    protocolInfo = mkProtocolInfoCardano genesisConfig

mkProtocolInfoCardano :: GenesisConfig -> Ouroboros.Consensus.Node.ProtocolInfo.ProtocolInfo IO CardanoBlock
mkProtocolInfoCardano = Ouroboros.Consensus.Cardano.protocolInfo . mkProtocolCardano

type CardanoBlock =
        Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
            (Ouroboros.Consensus.Cardano.Block.CardanoEras C.StandardCrypto)

mkProtocolCardano :: GenesisConfig -> C.Protocol m CardanoBlock CardanoProtocol
mkProtocolCardano ge =
  case ge of
    GenesisCardano dnc byronGenesis shelleyGenesis ->
        C.ProtocolCardano
          C.ProtocolParamsByron
            { C.byronGenesis = byronGenesis
            , C.byronPbftSignatureThreshold = C.PBftSignatureThreshold <$> dncPBftSignatureThreshold dnc
            , C.byronProtocolVersion = dncByronProtocolVersion dnc
            , C.byronSoftwareVersion = dncByronSoftwareVersion dnc
            , C.byronLeaderCredentials = Nothing
            }
          C.ProtocolParamsShelleyBased
            { C.shelleyBasedGenesis = scConfig shelleyGenesis
            , C.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
            , C.shelleyBasedLeaderCredentials = []
            }
          C.ProtocolParamsShelley
            { C.shelleyProtVer = shelleyProtVer dnc
            }
          C.ProtocolParamsAllegra
            { C.allegraProtVer = shelleyProtVer dnc
            }
          C.ProtocolParamsMary
            { C.maryProtVer = shelleyProtVer dnc
            }
          (dncByronToShelley dnc)
          (dncShelleyToAllegra dnc)
          (dncAllegraToMary dnc)

shelleyPraosNonce :: ShelleyConfig -> Shelley.Spec.Ledger.BaseTypes.Nonce
shelleyPraosNonce sCfg = Shelley.Spec.Ledger.BaseTypes.Nonce (Cardano.Crypto.Hash.Class.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

shelleyProtVer :: DbSyncNodeConfig -> Shelley.Spec.Ledger.PParams.ProtVer
shelleyProtVer dnc =
  let bver = dncByronProtocolVersion dnc in
  Shelley.Spec.Ledger.PParams.ProtVer
    (fromIntegral $ Cardano.Chain.Update.pvMajor bver)
    (fromIntegral $ Cardano.Chain.Update.pvMinor bver)

type CardanoProtocol =
        Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkProtocol
            '[ Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardAllegra
            , Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardMary
            ]
