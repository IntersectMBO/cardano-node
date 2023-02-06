{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Transactions in the context of a consensus mode, and other types used in
-- the transaction submission protocol.
--
module Cardano.Api.InMode (

    -- * Transaction in a consensus mode
    TxInMode(..),
    fromConsensusGenTx,
    toConsensusGenTx,

    -- * Transaction id in a consensus mode
    TxIdInMode(..),
    toConsensusTxId,

    -- * Transaction validation errors
    TxValidationError(..),
    TxValidationErrorInMode(..),
    fromConsensusApplyTxErr,
  ) where

import           Data.SOP.Strict (NS (S, Z))

import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Consensus
import qualified Ouroboros.Consensus.Ledger.SupportsProtocol as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import qualified Ouroboros.Consensus.Shelley.HFEras as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import qualified Ouroboros.Consensus.TypeFamilyWrappers as Consensus

import           Cardano.Api.Eras
import           Cardano.Api.Modes
import           Cardano.Api.Tx
import           Cardano.Api.TxBody


-- ----------------------------------------------------------------------------
-- Transactions in the context of a consensus mode
--

-- | A 'Tx' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxSubmission protocol.
--
data TxInMode mode where

     -- | Everything we consider a normal transaction.
     --
     TxInMode :: Tx era -> EraInMode era mode -> TxInMode mode

     -- | Byron has various things we can post to the chain which are not
     -- actually transactions. This covers: update proposals, votes and
     -- delegation certs.
     --
     TxInByronSpecial :: Consensus.GenTx Consensus.ByronBlock
                      -> EraInMode ByronEra mode -> TxInMode mode

deriving instance Show (TxInMode mode)

fromConsensusGenTx
  :: ConsensusBlockForMode mode ~ block
  => ConsensusMode mode -> Consensus.GenTx block -> TxInMode mode
fromConsensusGenTx ByronMode (Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))) =
  TxInByronSpecial tx' ByronEraInByronMode

fromConsensusGenTx ShelleyMode (Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))) =
  let Consensus.ShelleyTx _txid shelleyEraTx = tx'
  in TxInMode (ShelleyTx ShelleyBasedEraShelley shelleyEraTx) ShelleyEraInShelleyMode

fromConsensusGenTx CardanoMode (Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))) =
  TxInByronSpecial tx' ByronEraInCardanoMode

fromConsensusGenTx CardanoMode (Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (Z tx')))) =
  let Consensus.ShelleyTx _txid shelleyEraTx = tx'
  in TxInMode (ShelleyTx ShelleyBasedEraShelley shelleyEraTx) ShelleyEraInCardanoMode

fromConsensusGenTx CardanoMode (Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (Z tx'))))) =
  let Consensus.ShelleyTx _txid shelleyEraTx = tx'
  in TxInMode (ShelleyTx ShelleyBasedEraAllegra shelleyEraTx) AllegraEraInCardanoMode

fromConsensusGenTx CardanoMode (Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (Z tx')))))) =
  let Consensus.ShelleyTx _txid shelleyEraTx = tx'
  in TxInMode (ShelleyTx ShelleyBasedEraMary shelleyEraTx) MaryEraInCardanoMode

fromConsensusGenTx CardanoMode (Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (Z tx'))))))) =
  let Consensus.ShelleyTx _txid shelleyEraTx = tx'
  in TxInMode (ShelleyTx ShelleyBasedEraAlonzo shelleyEraTx) AlonzoEraInCardanoMode

fromConsensusGenTx CardanoMode (Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (S (Z tx')))))))) =
  let Consensus.ShelleyTx _txid shelleyEraTx = tx'
  in TxInMode (ShelleyTx ShelleyBasedEraBabbage shelleyEraTx) BabbageEraInCardanoMode

toConsensusGenTx :: ConsensusBlockForMode mode ~ block
                 => TxInMode mode
                 -> Consensus.GenTx block
toConsensusGenTx (TxInMode (ByronTx tx) ByronEraInByronMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ByronTx (Consensus.byronIdTx tx) tx

toConsensusGenTx (TxInMode (ByronTx tx) ByronEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ByronTx (Consensus.byronIdTx tx) tx
    --TODO: add the above as mkByronTx to the consensus code,
    -- matching mkShelleyTx below

toConsensusGenTx (TxInByronSpecial gtx ByronEraInByronMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))

toConsensusGenTx (TxInByronSpecial gtx ByronEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))

toConsensusGenTx (TxInMode (ShelleyTx _ tx) ShelleyEraInShelleyMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) ShelleyEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (Z tx')))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) AllegraEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (Z tx'))))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) MaryEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (Z tx')))))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) AlonzoEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (Z tx'))))))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) BabbageEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (S (Z tx')))))))
  where
    tx' = Consensus.mkShelleyTx tx


toConsensusGenTx (TxInMode (ShelleyTx _ _) ByronEraInByronMode) =
  error "Cardano.Api.InMode.toConsensusGenTx: ShelleyTx In Byron era"
toConsensusGenTx (TxInMode (ShelleyTx _ _) ByronEraInCardanoMode) =
  error "Cardano.Api.InMode.toConsensusGenTx: ShelleyTx In Byron era"

-- ----------------------------------------------------------------------------
-- Transaction ids in the context of a consensus mode
--

-- | A 'TxId' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxMonitoring protocol.
--

data TxIdInMode mode where
  TxIdInMode :: TxId -> EraInMode era mode -> TxIdInMode mode

toConsensusTxId
  :: ConsensusBlockForMode mode ~ block
  => TxIdInMode mode -> Consensus.TxId  (Consensus.GenTx block)
toConsensusTxId (TxIdInMode txid ByronEraInByronMode) =
  Consensus.HardForkGenTxId . Consensus.OneEraGenTxId . Z $ Consensus.WrapGenTxId txid'
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.ByronBlock)
  txid' = Consensus.ByronTxId $ toByronTxId txid

toConsensusTxId (TxIdInMode t ShelleyEraInShelleyMode) =
    Consensus.HardForkGenTxId $ Consensus.OneEraGenTxId  $ Z  (Consensus.WrapGenTxId txid')
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardShelleyBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId t

toConsensusTxId (TxIdInMode txid ByronEraInCardanoMode) =
  Consensus.HardForkGenTxId . Consensus.OneEraGenTxId . Z $ Consensus.WrapGenTxId txid'
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.ByronBlock)
  txid' = Consensus.ByronTxId $ toByronTxId txid

toConsensusTxId (TxIdInMode txid ShelleyEraInCardanoMode) =
  Consensus.HardForkGenTxId (Consensus.OneEraGenTxId (S (Z (Consensus.WrapGenTxId txid'))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardShelleyBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid

toConsensusTxId (TxIdInMode txid AllegraEraInCardanoMode) =
  Consensus.HardForkGenTxId (Consensus.OneEraGenTxId (S (S (Z (Consensus.WrapGenTxId txid')))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardAllegraBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid

toConsensusTxId (TxIdInMode txid MaryEraInCardanoMode) =
  Consensus.HardForkGenTxId (Consensus.OneEraGenTxId (S (S (S (Z (Consensus.WrapGenTxId txid'))))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardMaryBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid

toConsensusTxId (TxIdInMode txid AlonzoEraInCardanoMode) =
  Consensus.HardForkGenTxId (Consensus.OneEraGenTxId (S (S (S (S (Z (Consensus.WrapGenTxId txid')))))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardAlonzoBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid

toConsensusTxId (TxIdInMode txid BabbageEraInCardanoMode) =
  Consensus.HardForkGenTxId (Consensus.OneEraGenTxId (S (S (S (S (S (Z (Consensus.WrapGenTxId txid'))))))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardBabbageBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid

-- ----------------------------------------------------------------------------
-- Transaction validation errors in the context of eras and consensus modes
--

-- | The transaction validations errors that can occur from trying to submit a
-- transaction to a local node. The errors are specific to an era.
--
data TxValidationError era where

     ByronTxValidationError
       :: Consensus.ApplyTxErr Consensus.ByronBlock
       -> TxValidationError ByronEra

     ShelleyTxValidationError
       :: ShelleyBasedEra era
       -> Consensus.ApplyTxErr (Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era))
       -> TxValidationError era

-- The GADT in the ShelleyTxValidationError case requires a custom instance
instance Show (TxValidationError era) where
    showsPrec p (ByronTxValidationError err) =
      showParen (p >= 11)
        ( showString "ByronTxValidationError "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraShelley err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraShelley "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraAllegra err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraAllegra "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraMary err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraMary "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraAlonzo err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraAlonzo "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraBabbage err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraBabbage "
        . showsPrec 11 err
        )

-- | A 'TxValidationError' in one of the eras supported by a given protocol
-- mode.
--
-- This is used in the LocalStateQuery protocol.
--
data TxValidationErrorInMode mode where
     TxValidationErrorInMode :: TxValidationError era
                             -> EraInMode era mode
                             -> TxValidationErrorInMode mode

     TxValidationEraMismatch :: EraMismatch
                             -> TxValidationErrorInMode mode

deriving instance Show (TxValidationErrorInMode mode)


fromConsensusApplyTxErr :: ConsensusBlockForMode mode ~ block
                        => Consensus.LedgerSupportsProtocol
                             (Consensus.ShelleyBlock
                             (TPraos.TPraos Consensus.StandardCrypto)
                             (Consensus.ShelleyEra Consensus.StandardCrypto))
                        => ConsensusMode mode
                        -> Consensus.ApplyTxErr block
                        -> TxValidationErrorInMode mode
fromConsensusApplyTxErr ByronMode (Consensus.DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ByronTxValidationError err)
      ByronEraInByronMode

fromConsensusApplyTxErr ShelleyMode (Consensus.DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraShelley err)
      ShelleyEraInShelleyMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrByron err) =
    TxValidationErrorInMode
      (ByronTxValidationError err)
      ByronEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrShelley err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraShelley err)
      ShelleyEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrAllegra err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraAllegra err)
      AllegraEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrMary err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraMary err)
      MaryEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrAlonzo err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraAlonzo err)
      AlonzoEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrBabbage err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraBabbage err)
      BabbageEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrWrongEra err) =
    TxValidationEraMismatch err
