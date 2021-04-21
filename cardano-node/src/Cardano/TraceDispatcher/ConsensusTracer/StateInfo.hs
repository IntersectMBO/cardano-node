{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.ConsensusTracer.StateInfo
  (
    ForgeStateInfoDispatch(..)
  , severityStateInfoShelley
  , severityStateInfoByron
  , namesForStateInfoShelley
  , namesForStateInfoByron
  ) where

import           Data.Aeson (toJSON, (.=))

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Cardano.TraceDispatcher.ConsensusTracer.Formatting ()

import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import           Ouroboros.Consensus.Node.Tracers (TraceLabelCreds(..))
import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC)
import           Ouroboros.Consensus.Cardano.ShelleyHFC (ShelleyBlockHFC)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)


instance LogFormatting a => LogFormatting (TraceLabelCreds a) where
  forMachine dtal (TraceLabelCreds creds a)  =
    mkObject [ "credentials" .= toJSON creds
             , "val"         .= forMachine dtal a
            ]
-- TODO Trace lable creds as well
  forHuman (TraceLabelCreds _t a)         = forHuman a
  asMetrics (TraceLabelCreds _t a)        = asMetrics a

class ForgeStateInfoDispatch blk where
  severityStateInfo :: Proxy blk -> TraceLabelCreds (ForgeStateInfo blk) -> SeverityS
  namesForStateInfo :: Proxy blk -> TraceLabelCreds (ForgeStateInfo blk) -> [Text]
  humanFormatterStateInfo ::
       MonadIO m
    => Proxy blk
    -> Bool
    -> Text
    -> Trace m FormattedMessage
    -> m (Trace m (TraceLabelCreds (ForgeStateInfo blk)))

instance ForgeStateInfoDispatch ByronBlock where
    severityStateInfo _ (TraceLabelCreds _ v) =
      severityStateInfoByron v
    namesForStateInfo _ (TraceLabelCreds _ v) =
      namesForStateInfoByron v
    humanFormatterStateInfo _ colorize name baseTracer =
      humanFormatterStateInfoByron colorize name baseTracer

instance ForgeStateInfoDispatch (ShelleyBlock era) where
    severityStateInfo _ (TraceLabelCreds _ kesinfo) =
      severityStateInfoShelley kesinfo
    namesForStateInfo _ (TraceLabelCreds _ kesinfo) =
      namesForStateInfoShelley kesinfo
    humanFormatterStateInfo _ colorize name baseTracer =
      humanFormatterStateInfoShelley colorize name baseTracer


instance ForgeStateInfoDispatch (ShelleyBlockHFC era) where
    severityStateInfo _ (TraceLabelCreds _ _v) = Info
--      severityStateInfoShelleyHF kesinfo
    namesForStateInfo _ (TraceLabelCreds _ _v) = []
--      namesForStateInfoShelleyHF kesinfo
    humanFormatterStateInfo _ _colorize _name _baseTracer = undefined

instance ForgeStateInfoDispatch ByronBlockHFC where
    severityStateInfo _ (TraceLabelCreds _ _v) = Info
--      severityStateInfoByronHF kesinfo
    namesForStateInfo _ (TraceLabelCreds _ _v) = []
--      namesForStateInfoByronHF kesinfo
    humanFormatterStateInfo _ _colorize _name _baseTracer = undefined
--      humanFormatterStateInfoByronHF colorize name baseTracer

instance ForgeStateInfoDispatch (CardanoBlock crypt) where
    severityStateInfo _ (TraceLabelCreds _ _v) = Info
--      severityStateInfoCardano kesinfo
    namesForStateInfo _ (TraceLabelCreds _ _v) = []
--      namesForStateInfoCardano kesinfo
    humanFormatterStateInfo _ _colorize _name _baseTracer = undefined
--      humanFormatterStateInfoCardano colorize name baseTracer

humanFormatterStateInfoByron ::
     MonadIO m
  => Bool
  -> Text
  -> Trace m FormattedMessage
  -> m (Trace m (TraceLabelCreds ()))
humanFormatterStateInfoByron colorize name baseTracer =
    humanFormatter colorize name baseTracer

humanFormatterStateInfoShelley ::
     MonadIO m
  => Bool
  -> Text
  -> Trace m FormattedMessage
  -> m (Trace m (TraceLabelCreds HotKey.KESInfo))
humanFormatterStateInfoShelley colorize name baseTracer =
  humanFormatter colorize name baseTracer

severityStateInfoShelley :: HotKey.KESInfo -> SeverityS
severityStateInfoShelley _ki = Info

severityStateInfoByron :: () -> SeverityS
severityStateInfoByron _ = Info

namesForStateInfoShelley :: HotKey.KESInfo -> [Text]
namesForStateInfoShelley _ki = []

namesForStateInfoByron :: () -> [Text]
namesForStateInfoByron _ = []
