{-# LANGUAGE FlexibleContexts #-}
module Cardano.Timeseries.Interface(execute) where

import           Cardano.Timeseries.Domain.Types (Timestamp)
import           Cardano.Timeseries.Elab (elab, initialSt)
import           Cardano.Timeseries.Interp (interp)
import           Cardano.Timeseries.Interp.Config (Config (..))
import           Cardano.Timeseries.Interp.Types (QueryError (ErrorMessage))
import           Cardano.Timeseries.Interp.Value (Value)
import           Cardano.Timeseries.Store
import qualified Cardano.Timeseries.Surface.Expr.Parser as Surface.Parser

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (evalState)
import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Megaparsec hiding (count)
import           Text.Megaparsec.Char (space)

execute :: Store s Double => s -> Config -> Timestamp -> Text -> Either QueryError Value
execute store interpCfg now stringQuery = do
 surfaceQuery <- first (ErrorMessage . Text.pack . errorBundlePretty) $
   parse (Surface.Parser.expr <* space <* eof) "input" stringQuery
 query <- first ErrorMessage $
   evalState (runExceptT (elab surfaceQuery)) initialSt
 evalState (runExceptT $ interp interpCfg store mempty query now) 0
