module Cardano.TxGenerator.FundQueue
       ( module Cardano.TxGenerator.FundQueue
       , module Cardano.TxGenerator.Fund
       )
      where

import           Cardano.TxGenerator.Fund
import           Cardano.TxGenerator.Internal.Fifo as Fifo


type FundQueue = Fifo Fund


emptyFundQueue :: FundQueue
emptyFundQueue = Fifo.emptyFifo

toList :: FundQueue -> [Fund]
toList =  Fifo.toList

insertFund :: FundQueue -> Fund -> FundQueue
insertFund = Fifo.insert

removeFund :: FundQueue -> Maybe (FundQueue, Fund)
removeFund = Fifo.remove

removeFunds :: Int -> FundQueue -> Maybe (FundQueue, [Fund])
removeFunds = Fifo.removeN
