{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Interp() where
import           Data.Query.Lang (Expr (..), Function (..))
import           Data.Store.Common (Metric, Timestamp, Labelled, Interval)
import           Data.Store.Generic (Store (..))

type Error = String

data Series a where
  SVariable :: Metric -> Series a


srecent :: Series a -> [Labelled String] -> Maybe ([Labelled String], Timestamp, a)
srecent = _

swithin :: Series a -> [Labelled String] -> Interval -> [([Labelled String], Timestamp, a)]
swithin = _

data Value where
  VNumber :: Integer -> Value
  VSeries :: Series Integer -> Value

expectSeries :: Value -> Series a
expectSeries (VSeries t) = t

interp :: Store s Integer => s -> Expr -> Timestamp -> Either Error Value
interp store (Number x) now = Right (VNumber x)
interp store (Variable x) now =
  case recent store x [] of
    Nothing -> Left $ "Can't find " <> x <> " in the store"
    Just (_, _, x) -> Right (VSeries (SVariable x))
interp store (Application Mean [series, Application Min [t]]) now =
  -- We need intermediate (anonymous) series
  Right . show . mean $ (\(_, _, x) -> x) <$> within (interp store series now) [] (Interval (now - t * 60 * 1000) now)
interp store expr now = _
