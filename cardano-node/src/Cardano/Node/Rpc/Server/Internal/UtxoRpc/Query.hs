{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Rpc.Server.Internal.UtxoRpc.Query
  (readChainConfigMethod
  , readDataMethod
  , readParamsMethod
  , readTxMethod
  , readUtxosMethod
  , searchUtxosMethod
  ) where


import           Cardano.Api
import           Cardano.Api.Consensus
import           Cardano.Api.Internal.Block (Hash (..))
import           Cardano.Api.Internal.IPC (AcquiringFailure)

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Node.Rpc.Proto.Api.UtxoRpc.Query as UtxoRpc
import           Cardano.Node.Rpc.Server.Internal.Error
import           Cardano.Node.Rpc.Server.Internal.Monad
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (VolatileTip))

import qualified Data.ByteString.Short as SBS
import           Data.ProtoLens (defMessage)
import           Data.ProtoLens.Field (field)
import           Lens.Micro
import           Network.GRPC.Spec

import           RIO

readParamsMethod :: MonadRpc e m => Proto UtxoRpc.ReadParamsRequest -> m (Proto UtxoRpc.ReadParamsResponse)
readParamsMethod req = do
  nodeConnInfo <- grab
  AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
  eon <- forEraInEon era (error "Minimum babbage era required") pure
  let sbe = convert eon

  let target = VolatileTip
  let qInMode = QueryInEra $ QueryInShelleyBasedEra sbe QueryProtocolParameters
  (pparams, chainPoint) <- liftIO . (throwEither =<<) $ executeLocalStateQueryExpr nodeConnInfo target $ do
    pparams <- throwEither =<< throwEither =<< queryExpr qInMode
    chainPoint <- throwEither =<< queryChainPoint
    pure (pparams, chainPoint)

  let (slotNo, blockHash) =
        case chainPoint of
          ChainPointAtGenesis -> (0, mempty) -- TODO figure out the genesis hash and slot here
          ChainPoint (SlotNo slot) (HeaderHash hash) -> (slot, SBS.fromShort hash)

  let cpub = babbageEraOnwardsConstraints eon $ pparams ^. L.ppCoinsPerUTxOByteL
      pparamsMsg = defMessage & #coinsPerUtxoByte .~ fromIntegral (L.unCoinPerByte cpub)
      chainPointMsg =
        defMessage
          & #slot .~ slotNo
          & #hash .~ blockHash
  pure $
    defMessage
      & #values .~ (defMessage & #cardano .~ pparamsMsg)
      & #ledgerTip .~ chainPointMsg

readChainConfigMethod
  :: MonadRpc e m => Proto UtxoRpc.ReadChainConfigRequest -> m (Proto UtxoRpc.ReadChainConfigResponse)
readChainConfigMethod = undefined

readTxMethod :: MonadRpc e m => Proto UtxoRpc.ReadTxRequest -> m (Proto UtxoRpc.ReadTxResponse)
readTxMethod = undefined

readUtxosMethod :: MonadRpc e m => Proto UtxoRpc.ReadUtxosRequest -> m (Proto UtxoRpc.ReadUtxosResponse)
readUtxosMethod = undefined

readDataMethod :: MonadRpc e m => Proto UtxoRpc.ReadDataRequest -> m (Proto UtxoRpc.ReadDataResponse)
readDataMethod = undefined

searchUtxosMethod :: MonadRpc e m => Proto UtxoRpc.SearchUtxosRequest -> m (Proto UtxoRpc.SearchUtxosResponse)
searchUtxosMethod = undefined


-- orphans to upstream
instance Error QueryConvenienceError where
  prettyError = pretty . renderQueryConvenienceError

instance Error AcquiringFailure where
  prettyError = fromString . show

instance Error UnsupportedNtcVersionError where
  prettyError = fromString . show

instance Error EraMismatch where
  prettyError = fromString . show
