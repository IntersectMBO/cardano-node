{-# OPTIONS_GHC -Wwarn #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}

module Cardano.Benchmarking.FundSet
where
import           Prelude

import           Data.IxSet.Typed as IxSet
import           Data.Proxy

import           Control.Applicative ((<|>))
import           Cardano.Api as Api

-- Outputs that are available for spending.
-- When building a new TX they provide the TxIn parts.

data FundInEra era = FundInEra {
    _fundTxIn :: !TxIn
  , _fundVal  :: !(TxOutValue era)
  , _fundSigningKey :: !(SigningKey PaymentKey)
  , _fundVariant :: !Variant
  , _fundValidity :: !Validity
  } deriving (Show)

data Variant
  = PlainOldFund
  -- maybe better use the script itself instead of the filePath
  | PlutusScriptFund !FilePath !ScriptData
  -- A collateralFund is just a regular (PlainOldFund) on the chain,
  -- but tagged in the wallet so that it is not selected for spending.
  | CollateralFund
  deriving  (Show, Eq, Ord)

data Validity
  = Confirmed
  | InFlight !Target !SeqNumber
  deriving  (Show, Eq, Ord)

newtype Target = Target String
  deriving  (Show, Eq, Ord)

newtype SeqNumber = SeqNumber Int
  deriving  (Show, Eq, Ord, Enum)

newtype Fund = Fund {unFund :: InAnyCardanoEra FundInEra}

getFundVariant :: Fund -> Variant
getFundVariant (Fund (InAnyCardanoEra _ a)) = _fundVariant a

getFundTxIn :: Fund -> TxIn
getFundTxIn (Fund (InAnyCardanoEra _ a)) = _fundTxIn a

getFundKey :: Fund -> SigningKey PaymentKey
getFundKey (Fund (InAnyCardanoEra _ a)) = _fundSigningKey a

getFundValidity :: Fund -> Validity
getFundValidity (Fund (InAnyCardanoEra _ a)) = _fundValidity a

getFundLovelace :: Fund -> Lovelace
getFundLovelace (Fund (InAnyCardanoEra _ a)) = case _fundVal a of
  TxOutAdaOnly _era l -> l
  TxOutValue _era v -> selectLovelace v

data IsConfirmed = IsConfirmed | IsNotConfirmed
  deriving  (Show, Eq, Ord)

isConfirmed :: Fund -> IsConfirmed
isConfirmed f = case getFundValidity f of
  Confirmed -> IsConfirmed
  InFlight _ _ -> IsNotConfirmed

instance Show Fund where
  show (Fund (InAnyCardanoEra _ f)) = show f

-- TxIn/fundTxOut is the primary key.
-- There must be no two entries for the same TxIn !.

instance Eq Fund where
  (==) a b = getFundTxIn a == getFundTxIn b

instance Ord Fund where
  compare a b = compare (getFundTxIn a) (getFundTxIn b)

type FundIndices = '[ TxIn, IsConfirmed, Target, SeqNumber, Lovelace, Variant ]
type FundSet = IxSet FundIndices Fund

instance Indexable FundIndices Fund where
  indices = ixList
    (ixFun $ \f -> [ getFundTxIn f ])
    (ixFun $ \f -> [ isConfirmed f ])
    (ixFun $ \f -> case getFundValidity f of
      Confirmed -> []
      InFlight t _ -> [t]
    )
    (ixFun $ \f -> case getFundValidity f of
      Confirmed -> [SeqNumber (-1) ] -- Confirmed Txs get SeqNumber -1
      InFlight _ n -> [ n ]
    )
    (ixFun $ \f -> [ getFundLovelace f ])
    (ixFun $ \f -> [ getFundVariant f ])

emptyFunds :: FundSet
emptyFunds = IxSet.empty

insertFund :: FundSet -> Fund -> FundSet
insertFund s f = updateIx (getFundTxIn f) f s

deleteFund :: FundSet -> Fund -> FundSet
deleteFund s f = deleteIx (getFundTxIn f) s

liftAnyEra :: ( forall era. IsCardanoEra era => f1 era -> f2 era ) -> InAnyCardanoEra f1 -> InAnyCardanoEra f2
liftAnyEra f x = case x of
  InAnyCardanoEra ByronEra a   ->   InAnyCardanoEra ByronEra $ f a
  InAnyCardanoEra ShelleyEra a ->   InAnyCardanoEra ShelleyEra $ f a
  InAnyCardanoEra AllegraEra a ->   InAnyCardanoEra AllegraEra $ f a
  InAnyCardanoEra MaryEra a    ->   InAnyCardanoEra MaryEra $ f a
  InAnyCardanoEra AlonzoEra a  ->   InAnyCardanoEra AlonzoEra $ f a

type FundSelector = FundSet -> Either String [Fund]
type FundSource = IO (Either String [Fund])
type FundToStore = [Fund] -> IO ()

-- Select Funds to cover a minimum value.
-- TODO:
-- This fails unless there is a single fund with the required value
-- Extend this to really return a list of funds.
selectMinValue :: Lovelace -> FundSet -> Either String [Fund]
selectMinValue minValue fs = case coins of
    [] -> Left $ "findSufficientCoin: no single coin with min value >= " ++ show minValue
    (c:_) -> Right [c]
    where coins = toAscList ( Proxy :: Proxy Lovelace) (fs @=PlainOldFund @= IsConfirmed @>= minValue)

selectCollateral :: FundSet -> Either String [Fund]
selectCollateral fs = case coins of
  [] -> Left "no matching none-Plutus fund found"
  (c:_) -> Right [c]
 where
  coins = toAscList ( Proxy :: Proxy Lovelace) (fs @=CollateralFund @= IsConfirmed )

data AllowRecycle
  = UseConfirmedOnly
  | ReuseSameTarget
-- ReuseAny can cause none-deterministic runtime errors !
-- The problematic case is the reuse of an UTxO/Tx that is not yet confirmed
-- and still waits in the mempool of an other target-node.
  | ReuseAny
  | ConfirmedBeforeReuse -- usefull for testing
  deriving (Eq, Ord, Enum, Show)

-- There are many possible heuristics to implement the selectInputs function.
-- TODO: Check that the complexity of selectInputs is good enough.
selectInputs ::
     AllowRecycle
  -> Int
  -> Lovelace
  -> Variant
  -> Target
  -> FundSet
  -> Either String [Fund]
selectInputs allowRecycle count minTotalValue variant targetNode fs
  = case allowRecycle of
    UseConfirmedOnly     -> selectConfirmed
    ReuseSameTarget      -> reuseSameTarget <|> selectConfirmed
    ReuseAny             -> reuseSameTarget <|> selectConfirmed <|> reuseAnyCoin
    ConfirmedBeforeReuse -> selectConfirmed <|> reuseSameTarget
  where
  selectConfirmed = selectConfirmedSmallValue <|> selectConfirmedBigValue

  isSufficiantCoins coins = length coins == count && sum (map getFundLovelace coins) >= minTotalValue

  checkCoins :: String -> [Fund] -> Either String [Fund]
  checkCoins err coins
    = if isSufficiantCoins coins then Right coins else Left err

  -- Share intermediate results for variantIxSet confirmedIxSet and targetIxSet
  -- TODO: it unclear if this helps on the complexity or it it is even harmful.
  variantIxSet   = fs @= variant
  confirmedIxSet = variantIxSet @= IsConfirmed
  targetIxSet    = variantIxSet @= targetNode

  confirmedBigValueList = toDescList (Proxy :: Proxy Lovelace) confirmedIxSet
  sameTargetList = toAscList (Proxy :: Proxy SeqNumber) targetIxSet

  selectConfirmedSmallValue
    = checkCoins
        "selectConfirmedSmall: not enought coins available"
        (take count $ toAscList (Proxy :: Proxy Lovelace) confirmedIxSet)

  selectConfirmedBigValue
    = checkCoins
        "selectConfirmedSmall: not enought coins available"
        (take count confirmedBigValueList)

  -- reuseSameTargetStrict is problematic: It fails if the coins in the queues are too small. But it will never consume the small coins.
  -- therefore: (reuseSameTargetStrict <|> reuseSameTargetWithBackup)
  reuseSameTargetStrict
    = checkCoins
        "reuseSameTargetStrict: not enought coins available"
        (take count sameTargetList)

  -- reuseSameTargetWithBackup can collect some dust.
  -- reuseSameTargetWithBackup works fine if there is at least one sufficiant confirmed UTxO available.
  reuseSameTargetWithBackup = checkCoins "reuseSameTargetWithBackup: not enought coins available" (backupCoin ++ targetCoins)
    where
      -- targetCoins and backupCoins must be disjoint.
      -- This is case because IsConfirmed \= InFlight target.
      backupCoin = take 1 $ toAscList (Proxy :: Proxy Lovelace) (confirmedIxSet @> minTotalValue)
      targetCoins = take (count - 1) sameTargetList

  reuseSameTarget = reuseSameTargetStrict <|> reuseSameTargetWithBackup

  -- reuseAnyCoin is the last resort !
  reuseAnyCoin
    = checkCoins
        "reuseAnyTarget: not enought coins available"
        (take count $ confirmedBigValueList ++ inFlightCoins)
    where
      -- inFlightCoins and confirmedCoins are disjoint
      inFlightCoins = toAscList (Proxy :: Proxy SeqNumber) (variantIxSet @=IsNotConfirmed)

selectToBuffer ::
     Int
  -> Lovelace
  -> Variant
  -> FundSet
  -> Either String [Fund]
selectToBuffer count minValue variant fs
  = if length coins < count
    then Left $ concat
      [ "selectToBuffer: not enough coins found: count: ", show count
      , " minValue: ", show minValue
      , " variant: ", show variant
      ]
    else Right coins
 where
  coins = take count $ toAscList ( Proxy :: Proxy Lovelace) (fs @=variant @= IsConfirmed @>= minValue)

-- Todo: check sufficant funds and minimumValuePerUtxo
inputsToOutputsWithFee :: Lovelace -> Int -> [Lovelace] -> [Lovelace]
inputsToOutputsWithFee fee count inputs = map (quantityToLovelace . Quantity) outputs
  where
    (Quantity totalAvailable) = lovelaceToQuantity $ sum inputs - fee
    (out, rest) = divMod totalAvailable (fromIntegral count)
    outputs = (out + rest) : replicate (count-1) out
