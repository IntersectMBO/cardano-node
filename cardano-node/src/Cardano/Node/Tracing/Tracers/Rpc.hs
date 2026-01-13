{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Tracing.Tracers.Rpc () where

import           Cardano.Logging
import           Cardano.Rpc.Server (TraceRpc (..), TraceRpcSubmit (..))


instance LogFormatting TraceRpc where
  forMachine _dtal _ = undefined

instance MetaTrace TraceRpc where
  namespaceFor = undefined

  severityFor = undefined

  documentFor = undefined

  allNamespaces = undefined




