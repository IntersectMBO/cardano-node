module Test.Cardano.Crypto.Seed.Gen
  ( genSeed
  , genSeedForKey
  ) where

import           Cardano.Api (AsType, Key)
import           Cardano.Crypto.Seed (Seed)
import           Data.Functor ((<$>))
import           Data.Int (Int)
import           Hedgehog (MonadGen, Range)
import           Prelude (fromIntegral)

import qualified Cardano.Api as API
import qualified Cardano.Crypto.Seed as C
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

genSeed :: MonadGen m => Range Int -> m Seed
genSeed r = C.mkSeedFromBytes <$> G.bytes r

genSeedForKey :: (Key key, MonadGen m) => AsType key -> m Seed
genSeedForKey keyRole = genSeed (R.singleton (fromIntegral (API.deterministicSigningKeySeedSize keyRole)))
