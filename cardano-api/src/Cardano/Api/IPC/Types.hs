{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.IPC.Types
  ( LocalStateQueryExpr
  , LocalStateQueryExprWithError
  ) where

import           Cardano.Api.IPC.Monad
import           Control.Monad.Except

{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

type LocalStateQueryExprWithError e block point query r m a
  = ExceptT e (LocalStateQueryExpr block point query r m) a
