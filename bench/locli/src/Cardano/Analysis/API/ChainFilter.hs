{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
module Cardano.Analysis.API.ChainFilter (module Cardano.Analysis.API.ChainFilter) where

import Cardano.Prelude hiding (head)

import Data.Aeson
import Data.ByteString.Lazy.Char8       qualified as LBS
import Data.Text                        qualified as T
import Options.Applicative
import Options.Applicative              qualified as Opt
import System.FilePath.Posix                        (takeBaseName)

import Cardano.Util
import Cardano.Analysis.API.Ground


newtype JsonFilterFile
  = JsonFilterFile { unJsonFilterFile :: FilePath }
  deriving (Show, Eq)

newtype FilterName = FilterName { unFilterName :: Text }
  deriving (Eq, FromJSON, Generic, NFData, Show, ToJSON)

-- | Conditions for chain subsetting
data ChainFilter
  = CBlock BlockCond
  | CSlot  SlotCond
  deriving (Eq, FromJSON, Generic, NFData, Ord, Show, ToJSON)

-- | Block classification -- primary for validity as subjects of analysis.
data BlockCond
  = BUnitaryChainDelta            -- ^ All timings account for
                                  --    processing of a single block.
  | BFullnessGEq           Double -- ^ Block fullness is above fraction.
  | BFullnessLEq           Double -- ^ Block fullness is below fraction.
  | BSizeGEq               Word64
  | BSizeLEq               Word64
  | BMinimumAdoptions      Word64 -- ^ At least this many adoptions
  | BNonNegatives                 -- ^ Non-negative timings only
  deriving (Eq, FromJSON, Generic, NFData, Ord, Show, ToJSON)

data SlotCond
  = SlotGEq         SlotNo
  | SlotLEq         SlotNo
  | EpochGEq        EpochNo
  | EpochLEq        EpochNo
  | EpochSafeIntGEq EpochSafeInt  -- 10 per epoch for the standard setup of< Ouroboros Praos
  | EpochSafeIntLEq EpochSafeInt
  | EpSlotGEq       EpochSlot
  | EpSlotLEq       EpochSlot
  | SlotHasLeaders
  deriving (Eq, FromJSON, Generic, NFData, Ord, Show, ToJSON)

cfIsSlotCond, cfIsBlockCond :: ChainFilter -> Bool
cfIsSlotCond  = \case { CSlot{}  -> True; _ -> False; }
cfIsBlockCond = \case { CBlock{} -> True; _ -> False; }

catSlotFilters :: [ChainFilter] -> [SlotCond]
catSlotFilters = go [] where
  go :: [SlotCond] -> [ChainFilter] -> [SlotCond]
  go acc = \case
    [] -> reverse acc
    CSlot c:rest -> go (c:acc) rest
    _:rest       -> go    acc  rest

readChainFilter :: JsonFilterFile -> ExceptT String IO ([ChainFilter], FilterName)
readChainFilter (JsonFilterFile f) =
  fmap (, FilterName . T.pack $ takeBaseName f)
    . newExceptT
    $ eitherDecode @[ChainFilter] <$> LBS.readFile f

argChainFilterset :: String -> String -> Parser JsonFilterFile
argChainFilterset optname desc =
  fmap JsonFilterFile $
    Opt.option Opt.str
      $ long optname
      <> metavar "FILTERSET-FILE"
      <> help desc

argChainFilterExpr :: Parser ChainFilter
argChainFilterExpr =
  fmap (\arg ->
          either (error . mconcat . (["Error while parsing JSON filter expression __", arg, "__: "] <>) . (:[])) identity
        . eitherDecode @ChainFilter
        $ LBS.pack arg) $
    Opt.option Opt.str
      $ long "filter-expr"
      <> metavar "JSON"
      <> help "A directly specified filter JSON expression"

readFilters :: [JsonFilterFile] -> ExceptT Text IO ([ChainFilter], [FilterName])
readFilters fltfs = do
  xs <-
    forM fltfs $ \f@(JsonFilterFile fp) ->
      firstExceptT (\x -> T.pack $ "Failed to parse chain filter " <> fp <> ": " <> x)
        (readChainFilter f)
  pure (mconcat $ fst <$> xs, snd <$> xs)
