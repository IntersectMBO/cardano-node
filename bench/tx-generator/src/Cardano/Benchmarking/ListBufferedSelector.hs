-- todo: remove
module Cardano.Benchmarking.ListBufferedSelector
(
  mkBufferedSource
)
where
import           Prelude

import           Control.Concurrent.MVar

import           Cardano.Benchmarking.FundSet as FundSet
import           Cardano.Benchmarking.Wallet as Wallet

mkBufferedSource ::
     WalletRef
  -> Int
  -> IO (FundSource IO)
mkBufferedSource walletRef munch = do
  funds <- askWalletRef walletRef walletFunds
  buffer <- newMVar funds
  return $ listSource buffer munch

listSource :: MVar [Fund] -> Int -> IO (Either String [Fund])
listSource mvar count = modifyMVarMasked mvar popFunds
  where
    popFunds funds
      = if length sel == count
           then return (rest, Right sel)
           else return (funds, Left "ListSource: out of funds")
      where (sel, rest) = splitAt count funds
