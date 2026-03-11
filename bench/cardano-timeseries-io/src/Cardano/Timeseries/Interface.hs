{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Cardano.Timeseries.Interface(ExecutionError(..), execute) where

import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Domain.Types (Timestamp)
import           Cardano.Timeseries.Elab (elab, initialSt)
import           Cardano.Timeseries.Interp (interp)
import           Cardano.Timeseries.Interp.Config (Config (..))
import qualified Cardano.Timeseries.Interp.Types as QueryError
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

data ExecutionError where
  ParsingError :: {message :: Text} -> ExecutionError
  ElabError :: {message :: Text} -> ExecutionError
  InterpError :: {message :: Text} -> ExecutionError


instance AsText ExecutionError where
  asText ParsingError{message} = "Parsing error: " <> message
  asText ElabError{message} = "Elaboration error: " <> message
  asText InterpError{message} = "Interpretation error: " <> message

execute :: Store s Double => s -> Config -> Timestamp -> Text -> Either ExecutionError Value
execute store interpCfg now stringQuery = do
 surfaceQuery <- first (ParsingError . Text.pack . errorBundlePretty) $
   parse (Surface.Parser.expr <* space <* eof) "input" stringQuery
 query <- first ElabError $
   evalState (runExceptT (elab surfaceQuery)) initialSt
 first (InterpError . QueryError.message) $
   evalState (runExceptT $ interp interpCfg store mempty query now) 0
