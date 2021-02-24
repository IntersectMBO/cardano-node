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
  ( -- * Initialization / Accumulation
    initialLedgerState
  , applyBlock

    -- * Types
  , LedgerState
      ( ..
      , LedgerStateByron
      , LedgerStateShelley
      , LedgerStateAllegra
      , LedgerStateMary
      )
  , Env(..)
  , envSecurityParam
  )
  where

import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import           Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Short as BSS
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word

import qualified Cardano.Api.Block
import qualified Cardano.Api.Eras
import qualified Cardano.Api.IPC as Api
import qualified Cardano.Api.Modes as Api
import qualified Cardano.Chain.Genesis
import qualified Cardano.Crypto.Hash.Blake2b
import qualified Cardano.Crypto.Hash.Class
import qualified Cardano.Slotting.Slot
import           Data.SOP.Strict
import qualified Ouroboros.Consensus.Block.Abstract
import qualified Ouroboros.Consensus.Byron.Ledger.Block
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger
import qualified Ouroboros.Consensus.Cardano as C
import qualified Ouroboros.Consensus.Cardano.Block
import qualified Ouroboros.Consensus.Cardano.Block as C
import qualified Ouroboros.Consensus.Cardano.CanHardFork
import qualified Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.HardFork.Combinator.State.Types
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
import qualified Ouroboros.Consensus.HardFork.History.Summary
import qualified Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Shelley.Eras
import qualified Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger
import qualified Ouroboros.Consensus.Shelley.Protocol
import qualified Ouroboros.Network.Block


-- | Get the initial ledger state (and corresponding environment).
initialLedgerState
  :: Api.LocalNodeConnectInfo Api.CardanoMode
  -> IO (Env, LedgerState)
  -- ^ ( The environment
  --   , The initial ledger state
  --   )
initialLedgerState localNodeConnectInfo = do
  -- Configuration
  ledgerConfig <-
    either (error . show) id
    <$> Api.queryNodeLocalStateNoTip
          localNodeConnectInfo
          (Api.QueryLedgerConfig Api.CardanoModeIsMultiEra)

  consensusConfig <-
    either (error . show) id
    <$> Api.queryNodeLocalStateNoTip
          localNodeConnectInfo
          (Api.QueryConsensusConfig Api.CardanoModeIsMultiEra)

  Prelude.putStrLn "****"
  Prelude.putStrLn $ "gdAvvmDistr size: " <> show
    (let
        Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkLedgerConfig {
            hardForkLedgerConfigPerEra = Ouroboros.Consensus.HardFork.Combinator.AcrossEras.PerEraLedgerConfig
              (Ouroboros.Consensus.HardFork.Combinator.WrapPartialLedgerConfig (Ouroboros.Consensus.Cardano.CanHardFork.ByronPartialLedgerConfig byronLedgerConfig _) :* _)
          }
          = ledgerConfig
      in  Map.size
            $ Cardano.Chain.Genesis.unGenesisAvvmBalances
            $ Cardano.Chain.Genesis.gdAvvmDistr
            $ Cardano.Chain.Genesis.configGenesisData
            $ byronLedgerConfig
    )
  Prelude.putStrLn "****"

  let env = Env ledgerConfig consensusConfig
  let ledgerState = initialByronLedgerStateToLedgerState (initialByronLedgerState ledgerConfig)

  return (env, ledgerState)

-- | Apply a single block to the current ledger state.
-- TODO to what extent if any does this validate the block?
applyBlock
  :: Env
  -- ^ The environment returned by @initialLedgerState@
  -> LedgerState
  -- ^ The current ledger state
  -> Cardano.Api.Block.Block era
  -- ^ Some block to apply
  -> LedgerState
  -- ^ The new ledger state.
applyBlock env oldState block = let
  cardanoBlock :: Ouroboros.Consensus.Cardano.Block.CardanoBlock Ouroboros.Consensus.Shelley.Eras.StandardCrypto
  cardanoBlock = case block of
    Cardano.Api.Block.ByronBlock byronBlock -> Ouroboros.Consensus.Cardano.Block.BlockByron byronBlock
    Cardano.Api.Block.ShelleyBlock blockEra shelleyBlock -> case blockEra of
      Cardano.Api.Eras.ShelleyBasedEraShelley -> Ouroboros.Consensus.Cardano.Block.BlockShelley shelleyBlock
      Cardano.Api.Eras.ShelleyBasedEraAllegra -> Ouroboros.Consensus.Cardano.Block.BlockAllegra shelleyBlock
      Cardano.Api.Eras.ShelleyBasedEraMary    -> Ouroboros.Consensus.Cardano.Block.BlockMary shelleyBlock
  newState = applyBlock' env oldState cardanoBlock
  in newState

pattern LedgerStateByron
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
  -> LedgerState
pattern LedgerStateByron st <- LedgerState (C.LedgerStateByron st)

pattern LedgerStateShelley
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock (Ouroboros.Consensus.Shelley.Eras.ShelleyEra Ouroboros.Consensus.Shelley.Eras.StandardCrypto))
  -> LedgerState
pattern LedgerStateShelley st <- LedgerState  (C.LedgerStateShelley st)

pattern LedgerStateAllegra
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock (Ouroboros.Consensus.Shelley.Eras.AllegraEra Ouroboros.Consensus.Shelley.Eras.StandardCrypto))
  -> LedgerState
pattern LedgerStateAllegra st <- LedgerState  (C.LedgerStateAllegra st)

pattern LedgerStateMary
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState (Ouroboros.Consensus.Shelley.Ledger.Block.ShelleyBlock (Ouroboros.Consensus.Shelley.Eras.MaryEra Ouroboros.Consensus.Shelley.Eras.StandardCrypto))
  -> LedgerState
pattern LedgerStateMary st <- LedgerState  (C.LedgerStateMary st)

{-# COMPLETE LedgerStateByron
           , LedgerStateShelley
           , LedgerStateAllegra
           , LedgerStateMary #-}

--------------------------------------------------------------------------------
-- My own additions                                                           --
--------------------------------------------------------------------------------

initialByronLedgerState
  :: Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkLedgerConfig (Ouroboros.Consensus.Cardano.Block.CardanoEras Ouroboros.Consensus.Shelley.Eras.StandardCrypto)
  -> Ouroboros.Consensus.Ledger.Basics.LedgerState Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
initialByronLedgerState
  Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkLedgerConfig {
    hardForkLedgerConfigPerEra = Ouroboros.Consensus.HardFork.Combinator.AcrossEras.PerEraLedgerConfig
      (Ouroboros.Consensus.HardFork.Combinator.WrapPartialLedgerConfig (Ouroboros.Consensus.Cardano.CanHardFork.ByronPartialLedgerConfig byronLedgerConfig _) :* _)
  }
  = Ouroboros.Consensus.Byron.Ledger.Ledger.initByronLedgerState
      byronLedgerConfig
      Nothing

initialByronLedgerStateToLedgerState
  :: Ouroboros.Consensus.Ledger.Basics.LedgerState Ouroboros.Consensus.Byron.Ledger.Block.ByronBlock
  -> LedgerState
initialByronLedgerStateToLedgerState byronState
  = LedgerState
    $ Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkLedgerState
      $ Ouroboros.Consensus.HardFork.Combinator.HardForkState
        $ Ouroboros.Consensus.HardFork.Combinator.Util.Telescope.TZ
          $ Ouroboros.Consensus.HardFork.Combinator.State.Types.Current
              Ouroboros.Consensus.HardFork.History.Summary.initBound
              byronState

--------------------------------------------------------------------------------
-- Everything below this is just coppied from db-sync                         --
--------------------------------------------------------------------------------

newtype LedgerState = LedgerState
  { clsState :: Ouroboros.Consensus.Ledger.Basics.LedgerState
                  (Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
                    (Ouroboros.Consensus.Cardano.Block.CardanoEras C.StandardCrypto))
  }


newtype GenesisHashShelley = GenesisHashShelley
  { _unGenesisHashShelley :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)


data LookupFail
  = DbLookupBlockHash !ByteString
  | DbLookupBlockId !Word64
  | DbLookupMessage !Text
  | DbLookupTxHash !ByteString
  | DbLookupTxOutPair !ByteString !Word16
  | DbLookupEpochNo !Word64
  | DbLookupSlotNo !Word64
  | DbMetaEmpty
  | DbMetaMultipleRows
  deriving (Eq, Show)


textShow :: Show a => a -> Text
textShow = Text.pack . show

data ShelleyGenesisError
     = GenesisReadError !FilePath !Text
     | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
     | GenesisDecodeError !FilePath !Text
     deriving Show

data Env = Env
  { envLedgerConfig :: Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkLedgerConfig (Ouroboros.Consensus.Cardano.Block.CardanoEras Ouroboros.Consensus.Shelley.Eras.StandardCrypto)
  , envProtocolConfig :: Ouroboros.Consensus.Shelley.Protocol.ConsensusConfig (Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkProtocol (Ouroboros.Consensus.Cardano.Block.CardanoEras Ouroboros.Consensus.Shelley.Eras.StandardCrypto))
  }

envSecurityParam :: Env -> Word64
envSecurityParam env = k
  where
    C.SecurityParam k
      = Ouroboros.Consensus.HardFork.Combinator.Basics.hardForkConsensusConfigK
      $ envProtocolConfig env


-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlock'
  :: Env
  -> LedgerState
  ->  Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
            (Ouroboros.Consensus.Cardano.Block.CardanoEras C.StandardCrypto)
  -> LedgerState
applyBlock' env oldState blk = oldState { clsState = applyBlk (envLedgerConfig env) blk (clsState oldState) }
  where
    applyBlk
        :: Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkLedgerConfig
            (C.CardanoEras Ouroboros.Consensus.Shelley.Protocol.StandardCrypto)
        -> C.CardanoBlock C.StandardCrypto
        -> Ouroboros.Consensus.Shelley.Ledger.Ledger.LedgerState
            (Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
                (C.CardanoEras Ouroboros.Consensus.Shelley.Protocol.StandardCrypto))
        -> Ouroboros.Consensus.Shelley.Ledger.Ledger.LedgerState
            (Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
                (C.CardanoEras Ouroboros.Consensus.Shelley.Protocol.StandardCrypto))
    applyBlk cfg block lsb =
      case tickThenReapplyCheckHash cfg block lsb of
        Left err -> error $ Text.unpack err
        Right result -> result

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from the block matches
-- the head hash of the ledger state.
tickThenReapplyCheckHash
    :: Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkLedgerConfig
        (C.CardanoEras Ouroboros.Consensus.Shelley.Protocol.StandardCrypto)
    -> C.CardanoBlock C.StandardCrypto
    -> Ouroboros.Consensus.Shelley.Ledger.Ledger.LedgerState
        (Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
            (C.CardanoEras Ouroboros.Consensus.Shelley.Protocol.StandardCrypto))
    -> Either Text (Ouroboros.Consensus.Shelley.Ledger.Ledger.LedgerState
        (Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock
            (C.CardanoEras Ouroboros.Consensus.Shelley.Protocol.StandardCrypto)))
tickThenReapplyCheckHash cfg block lsb =
  if Ouroboros.Consensus.Block.Abstract.blockPrevHash block == Ouroboros.Consensus.Ledger.Abstract.ledgerTipHash lsb
    then Right $ Ouroboros.Consensus.Ledger.Abstract.tickThenReapply cfg block lsb
    else Left $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow
                      $ Cardano.Slotting.Slot.unSlotNo
                      $ Cardano.Slotting.Slot.fromWithOrigin
                          (Cardano.Slotting.Slot.SlotNo 0)
                          (Ouroboros.Consensus.Ledger.Abstract.ledgerTipSlot lsb)
                  , " hash "
                  , renderByteArray
                      $ unChainHash
                      $ Ouroboros.Consensus.Ledger.Abstract.ledgerTipHash lsb
                  , " but block previous hash is "
                  , renderByteArray (unChainHash $ Ouroboros.Consensus.Block.Abstract.blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray
                      $ BSS.fromShort
                      $ Ouroboros.Consensus.HardFork.Combinator.AcrossEras.getOneEraHash
                      $ Ouroboros.Network.Block.blockHash block
                  , "."
                  ]

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

unChainHash :: Ouroboros.Network.Block.ChainHash (C.CardanoBlock era) -> ByteString
unChainHash ch =
  case ch of
    Ouroboros.Network.Block.GenesisHash -> "genesis"
    Ouroboros.Network.Block.BlockHash bh -> BSS.fromShort (Ouroboros.Consensus.HardFork.Combinator.AcrossEras.getOneEraHash bh)


