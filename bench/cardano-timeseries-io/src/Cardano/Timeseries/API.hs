{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Cardano.Timeseries.API
  ( ExecutionError(..)
  , execute
  , module Export
  ) where

import           Cardano.Timeseries.AsText as Export (AsText (..), showT)
import           Cardano.Timeseries.Domain.Types as Export (MetricIdentifier, Timestamp)
import           Cardano.Timeseries.Elab as Export (elab, initialSt)
import           Cardano.Timeseries.Elab.Expr.Parser as Export (Parser, expr)
import           Cardano.Timeseries.Interp as Export (interp)
import           Cardano.Timeseries.Interp.Config as Export (Config (..))
import           Cardano.Timeseries.Interp.Types as Export (InterpError (..), InterpM)
import           Cardano.Timeseries.Interp.Value as Export (Value (..))
import           Cardano.Timeseries.Store as Export (Store (..))
import           Cardano.Timeseries.Store.Flat as Export (Flat)
import           Cardano.Timeseries.Store.Flat.Parser as Export (double, point)
import           Cardano.Timeseries.Store.Tree as Export (Tree, fromFlat)

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (evalState)
import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Megaparsec hiding (count)
import           Text.Megaparsec.Char (space)

data ExecutionError where
  ParsingErrorWhileExecuting :: {message :: Text} -> ExecutionError
  ElabErrorWhileExecuting :: {message :: Text} -> ExecutionError
  InterpErrorWhileExecuting :: {message :: Text} -> ExecutionError deriving (Show)

instance AsText ExecutionError where
  asText ParsingErrorWhileExecuting{message} = "While executing, parsing error: " <> message
  asText ElabErrorWhileExecuting{message} = "While executing, elaboration error: " <> message
  asText InterpErrorWhileExecuting{message} = "While executing, interpretation error: " <> message

-- | A pure interface to the composite of parsing, elaboration and interpretation of timeseries queries.
execute :: Store s Double => s -> Config -> Timestamp -> Text -> Either ExecutionError Value
execute store interpCfg now stringQuery = do
 surfaceQuery <- first (ParsingErrorWhileExecuting . Text.pack . errorBundlePretty) $
   parse (expr <* space <* eof) "input" stringQuery
 query <- first ElabErrorWhileExecuting $
   evalState (runExceptT (elab surfaceQuery)) initialSt
 first (InterpErrorWhileExecuting . (.message)) $
   evalState (runExceptT $ interp interpCfg store mempty query now) 0
