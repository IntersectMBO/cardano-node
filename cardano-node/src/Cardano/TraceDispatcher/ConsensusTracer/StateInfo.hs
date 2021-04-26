{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.ConsensusTracer.StateInfo
  (
    GetKESInfo(..)
  , severityStateInfo
  , namesForStateInfo
  , humanFormatterStateInfo
  ) where

import           Data.Aeson (toJSON, (.=))
import           Data.SOP.Strict

import           Cardano.Logging
import           Cardano.Prelude hiding (All, Show, show)
import           Cardano.TraceDispatcher.ConsensusTracer.Formatting ()

import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.HardFork.Combinator (HardForkBlock)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
                     (OneEraForgeStateInfo (..))
import           Ouroboros.Consensus.Node.Tracers (TraceLabelCreds (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import           Ouroboros.Consensus.TypeFamilyWrappers
                     (WrapForgeStateInfo (..))

class GetKESInfo blk where
  getKESInfoFromStateInfo :: Proxy blk -> ForgeStateInfo blk -> Maybe HotKey.KESInfo
  getKESInfoFromStateInfo _ _ = Nothing

instance GetKESInfo (ShelleyBlock era) where
  getKESInfoFromStateInfo _ fsi = Just fsi

instance GetKESInfo ByronBlock

instance All GetKESInfo xs => GetKESInfo (HardForkBlock xs) where
  getKESInfoFromStateInfo _ =
      hcollapse
    . hcmap (Proxy @GetKESInfo) getOne
    . getOneEraForgeStateInfo
   where
    getOne :: forall blk. GetKESInfo blk
           => WrapForgeStateInfo blk
           -> K (Maybe HotKey.KESInfo) blk
    getOne = K . getKESInfoFromStateInfo (Proxy @blk) . unwrapForgeStateInfo


instance LogFormatting a => LogFormatting (TraceLabelCreds a) where
  forMachine dtal (TraceLabelCreds creds a)  =
    mkObject [ "credentials" .= toJSON creds
             , "val"         .= forMachine dtal a
            ]
-- TODO Trace lable creds as well
  forHuman (TraceLabelCreds _t a)         = forHuman a
  asMetrics (TraceLabelCreds _t a)        = asMetrics a

traceAsMaybeKESInfo
  :: forall m blk . (GetKESInfo blk, MonadIO m)
  => Proxy blk
  -> Trace m (Maybe (TraceLabelCreds (HotKey.KESInfo)))
  -> Trace m (TraceLabelCreds (ForgeStateInfo blk))
traceAsMaybeKESInfo pr (Trace tr) = Trace $
  contramap
        (\(lc, mbC, (TraceLabelCreds c e)) ->
            case getKESInfoFromStateInfo pr e of
              Just kesi -> (lc, mbC, Just (TraceLabelCreds c kesi))
              Nothing   -> (lc, mbC, Nothing))
        tr

humanFormatterStateInfo ::
  (  MonadIO m
  ,  GetKESInfo blk)
  => Proxy blk
  -> Bool
  -> Text
  -> Trace m FormattedMessage
  -> m (Trace m (TraceLabelCreds (ForgeStateInfo blk)))
humanFormatterStateInfo pr withColor application tr = do
  tr' <- humanFormatter withColor application tr
  pure $ traceAsMaybeKESInfo pr (filterTraceMaybe tr')


severityStateInfo :: Proxy blk -> TraceLabelCreds (ForgeStateInfo blk) -> SeverityS
severityStateInfo pr (TraceLabelCreds _creds a) = severityStateInfo' pr a

severityStateInfo' :: Proxy blk -> ForgeStateInfo blk -> SeverityS
severityStateInfo' _pr _fsi = Info

namesForStateInfo :: Proxy blk -> TraceLabelCreds (ForgeStateInfo blk) -> [Text]
namesForStateInfo pr (TraceLabelCreds _creds a) = namesForStateInfo' pr a

namesForStateInfo' :: Proxy blk -> ForgeStateInfo blk -> [Text]
namesForStateInfo' _pr _fsi = []
