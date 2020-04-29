{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.Orphans () where

import           Cardano.Prelude (Eq, Generic, NFData, NoUnexpectedThunks)

import           Cardano.Slotting.Slot
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Shelley.Spec.Ledger.Keys
import           Shelley.Spec.Ledger.Scripts
import           Shelley.Spec.Ledger.TxData

deriving instance Eq (SKey TPraosStandardCrypto)


deriving instance Generic KeyDiscriminator
deriving instance NoUnexpectedThunks KeyDiscriminator
instance NFData KeyDiscriminator

deriving instance NFData (Addr TPraosStandardCrypto)

deriving instance NFData (Credential Ptr)
deriving instance NFData (Credential TPraosStandardCrypto)

deriving instance NFData (StakeReference Ptr)
deriving instance NFData (StakeReference TPraosStandardCrypto)

deriving instance Generic (ScriptHash TPraosStandardCrypto)
deriving instance NFData (ScriptHash TPraosStandardCrypto)

deriving instance Generic (DiscKeyHash 'Regular TPraosStandardCrypto)
deriving instance NFData (DiscKeyHash 'Regular TPraosStandardCrypto)

deriving instance Generic (DiscKeyHash 'Regular Ptr)
deriving instance NFData (DiscKeyHash 'Regular Ptr)

deriving instance NFData Ptr

deriving instance NFData SlotNo

deriving instance Generic (ScriptHash Ptr)
deriving instance NFData (ScriptHash Ptr)

