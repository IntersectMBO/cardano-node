module Cardano.ReCon.Common.Types (NatValue, IntValue, VariableIdentifier, Parser, BinRel(..)) where

import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Void (Void)
import           Data.Word (Word64)
import           Text.Megaparsec (Parsec)

type NatValue = Word64

-- | The numeric type used for integer event property values and polynomial
--   arithmetic throughout the framework.
type IntValue = Int64

-- | The type of variable identifiers used throughout the framework
--   (LTL property variables and polynomial integer variables).
type VariableIdentifier = Text

type Parser = Parsec Void Text

-- | Binary comparison relation on an ordered type.
data BinRel = Eq | Lt | Lte | Gt | Gte deriving (Show, Eq, Ord)
