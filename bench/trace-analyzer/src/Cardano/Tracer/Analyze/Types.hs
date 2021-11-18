
module Cardano.Tracer.Analyze.Types
  (
    AnalyzerParams(..)
  , LineNumber
  , KindName
  , FileDBM(..)
  ) where

import           Data.Aeson (Value)
import           Data.Map
import           Data.Text

-- | Type for CLI parameters required for the service.
data AnalyzerParams = AnalyzerParams
    { apTrace1  :: FilePath
    , apTrace2  :: FilePath
    , apIsHuman :: Bool
    } deriving (Show)

type LineNumber = Int

type KindName = Text

data FileDBM = FileDBM {
      fdbFileName    :: Text
    , fdbLengthTotal :: Int
    , fdbKindMap     :: Map KindName [(LineNumber, Value)]
    , fdbErrors      :: [(LineNumber, String)]
    , fdbValues      :: Map LineNumber Value
  }
  deriving Show
