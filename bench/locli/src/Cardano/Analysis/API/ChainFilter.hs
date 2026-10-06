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

    -- Leios. An endorser block's whole life spans ONE chain edge: block N
    -- announces it, the cluster votes, block N+1 certifies it. So there are
    -- three facts about it -- announced, reached quorum, certified -- and each
    -- is visible from both ends of that edge. These are the six, this block's
    -- own endorser block first, then its predecessor's. Conjoin them to ask
    -- anything else.

    -- | This block announced an endorser block of its own, which a forger does
    --   whenever its mempool is non-empty: under load that is nearly every
    --   block. Its use is conjunction, telling 'BOwnEbHasQuorum' False
    --   (announced, and the tally failed) apart from announcing nothing.
  | BAnnouncesOwnEb

    -- | A certificate was (True) or was not (False) assembled for the endorser
    --   block THIS block announced, meaning the cluster's votes passed the
    --   threshold. Says nothing about the successor then carrying it: the
    --   2026-10-01 run measured 1,033 quorums against 541 blocks that did.
  | BOwnEbHasQuorum               Bool

    -- | The block AFTER this one did (True) or did not (False) certify the
    --   endorser block THIS one announced, which is whether that endorser
    --   block reached the chain at all. False does not say why:
    --   'BOwnEbHasQuorum' tells a failed vote from a quorum the successor did
    --   not carry.
  | BSuccessorCertifiesOwnEb      Bool

    -- | The block BEFORE this one announced an endorser block, so this block
    --   was that endorser block's one chance: only the direct successor may
    --   certify it. Necessary, not sufficient -- 'minCertificationGap' also
    --   wants slots elapsed since the announcement (14 at the v12-preview
    --   periods and 1 s slots), the certificate to exist, and the closure to
    --   be downloaded.
  | BPredecessorAnnouncesEb

    -- | A certificate was (True) or was not (False) assembled for the endorser
    --   block the block BEFORE this one announced, so whether one was there to
    --   be carried. Conjoined with 'BCertifiesPredecessorEb' False it isolates
    --   the blocks that had a certificate available and went out without it,
    --   which is the only way to tell a lost quorum from a vote that never
    --   reached one.
  | BPredecessorEbHasQuorum       Bool

    -- | This block did (True) or did not (False) certify the endorser block
    --   its PREDECESSOR announced. Conjoined with 'BPredecessorAnnouncesEb'
    --   the two values split the blocks that had the chance into the ones that
    --   took it and the ones that did not.
    --   True is the only Leios work attributable to a block: carrying the
    --   certificate and applying the endorser block's transactions. Fetching,
    --   validating and voting is not -- that happens in every node, per
    --   announced endorser block, and belongs to no block.
  | BCertifiesPredecessorEb       Bool
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
