import           Hedgehog.Main (defaultMain)

import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Test.Golden.Byron.SigningKeys
import qualified Test.Golden.Byron.Tx
import qualified Test.Golden.Byron.UpdateProposal
import qualified Test.Golden.Byron.Vote
import qualified Test.Golden.Key
import qualified Test.Golden.Shelley
import qualified Test.Golden.TxView

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  defaultMain
    [ Test.Golden.Byron.SigningKeys.tests
    , Test.Golden.Byron.Tx.txTests
    , Test.Golden.Byron.UpdateProposal.updateProposalTest
    , Test.Golden.Byron.Vote.voteTests
    , Test.Golden.Key.keyTests
    , Test.Golden.Shelley.keyTests
    , Test.Golden.Shelley.certificateTests
    , Test.Golden.Shelley.keyConversionTests
    , Test.Golden.Shelley.metadataTests
    , Test.Golden.Shelley.multiSigTests
    , Test.Golden.Shelley.txTests
    , Test.Golden.TxView.txViewTests
    ]
