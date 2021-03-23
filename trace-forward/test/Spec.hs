import           Test.Hspec

import           Test.GetLogObjects

main :: IO ()
main = hspec $ do
  describe "LogObjects forwarding, via local pipe" $
    it "request of N items"
      getLogObjectsViaPipe
  describe "LogObjects forwarding, via remote socket" $
    it "request of N items"
      getLogObjectsViaSocket
