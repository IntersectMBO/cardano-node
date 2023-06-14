{-|
Module       : Cardano.TxGenerator.FundQueue
Description  : A queue data structure specialized to `Fund`.
-}
module Cardano.TxGenerator.FundQueue
       ( module Cardano.TxGenerator.FundQueue
       , module Cardano.TxGenerator.Fund
       )
      where

import           Cardano.TxGenerator.Fund
import qualified Cardano.TxGenerator.Internal.Fifo as Fifo


-- | A type alias for the specialized queue type.
type FundQueue = Fifo.Fifo Fund


-- | This just restricts the type of the generic `Fifo.emptyFifo`.
emptyFundQueue :: FundQueue
emptyFundQueue = Fifo.emptyFifo

-- | Converting to a list provides a different, non-FIFO access order.
toList :: FundQueue -> [Fund]
toList =  Fifo.toList

-- | This just restricts the type of the generic `Fifo.insert` function.
insertFund :: FundQueue -> Fund -> FundQueue
insertFund = Fifo.insert

-- | This just restricts the type of the generic `Fifo.remove` function.
removeFund :: FundQueue -> Maybe (FundQueue, Fund)
removeFund = Fifo.remove

-- | This just restricts the type of the generic `Fifo.removeN` function.
removeFunds :: Int -> FundQueue -> Maybe (FundQueue, [Fund])
removeFunds = Fifo.removeN

-- | This is really just `toList` paired with an `emptyFundQueue`.
removeAllFunds :: FundQueue -> (FundQueue, [Fund])
removeAllFunds queue = (emptyFundQueue, toList queue)
