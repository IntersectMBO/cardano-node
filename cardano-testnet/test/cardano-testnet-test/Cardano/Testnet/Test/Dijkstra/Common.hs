{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Small CLI helpers shared by the on-chain Dijkstra smoke properties.
module Cardano.Testnet.Test.Dijkstra.Common where

import           Cardano.Api hiding (label)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Ledger as L
import           Cardano.Testnet
import           Cardano.Testnet.Test.Dijkstra.NestedTransaction (UtxoEntry (..), queryUtxos)
import           Cardano.Testnet.Test.Node.DijkstraEra (assertNodeInDijkstra, hardForkToDijkstra)

import           Control.Monad (void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default.Class (def)
import           Data.List (maximumBy)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Lens.Micro ((^.))
import           System.FilePath ((</>))

import           Testnet.Components.Configuration (getShelleyGenesisHash)
import           Testnet.Components.Query
import           Testnet.Defaults (defaultGenesisFilepath)
import           Testnet.Process.Run (execCli, execCli', mkExecConfig)
import           Testnet.Property.Util (integrationRetryWorkspace)
import           Testnet.Start.Cardano (liftToIntegration)
import           Testnet.Start.Types
import           Testnet.Types

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- The first wallet pays top-level fees and deposits; the second owns sub-inputs.
-- Keeping them separate also tests that sub-witnesses survive embedding.
data DijkstraTest = DijkstraTest
  { exec :: H.ExecConfig
  , epochView :: EpochStateView
  , directory :: FilePath
  , payer :: PaymentKeyInfo
  , owner :: PaymentKeyInfo
  , collateralWallet :: PaymentKeyInfo
  , protocolParams :: FilePath
  , collateral :: String
  , v4Script :: FilePath
  , testNetwork :: NetworkId
  }

data ScriptSource = ViaFile | ViaReference deriving (Eq, Show)

-- | The same one-argument always-succeeding V4 program used by the CLI tests.
-- It intentionally tests witness plumbing, not business logic in the validator.
writeV4 :: FilePath -> H.Integration ()
writeV4 path = H.writeFile path
  "{\"type\":\"PlutusScriptV4\",\"description\":\"Always succeeds\",\"cborHex\":\"46450101002499\"}"

withDijkstra :: String -> Bool -> (DijkstraTest -> H.Integration ()) -> Property
withDijkstra name guardrail action = integrationRetryWorkspace 2 name $ \tmp -> H.runWithDefaultWatchdog_ $ do
  conf@Conf{tempAbsPath} <- mkConf tmp
  work <- H.createDirectoryIfMissing $ tmp </> "work"
  let script = work </> "always-succeeds-v4.plutus"
      options = def
        { creationEra = AnyShelleyBasedEra ShelleyBasedEraConway
        , creationGenesisOptions = def { genesisEpochLength = 300, genesisActiveSlotsCoeff = 0.3 }
        }
  writeV4 script
  liftToIntegration $ createTestnetEnv options conf
  -- Set the constitution once, before startup, rather than enacting a separate
  -- constitution in each governance scenario. Hard forks do not run guardrails.
  if guardrail then do
    guardrailHash <- trim <$> execCli ["hash", "script", "--script-file", script]
    let genesis = tmp </> defaultGenesisFilepath ConwayEra
    constitution <- H.readJsonFileOk genesis >>= field "constitution"
    H.rewriteJsonFile genesis $ KM.insert "constitution"
      (A.Object $ KM.insert "script" (A.String $ Text.pack guardrailHash) constitution)
    hash <- getShelleyGenesisHash genesis "ConwayGenesisHash"
    H.rewriteJsonFile (tmp </> "configuration.yaml") (hash <>)
  else pure ()
  runtime <- liftToIntegration $ cardanoTestnet (creationNodes options) def conf
  let node = NEL.head $ testnetNodes runtime
  config <- mkExecConfig (makeTmpBaseAbsPath tempAbsPath) (nodeSprocket node) (testnetMagic runtime)
  view <- getEpochStateView (configurationFile runtime) (nodeSocketPath node)
  (batcher:alice:bob:_) <- pure $ wallets runtime
  hardForkToDijkstra config view tempAbsPath work batcher alice
  assertNodeInDijkstra config
  let pp = work </> "protocol-parameters.json"
  void $ execCli' config ["query", "protocol-parameters", "--out-file", pp]
  -- A dedicated collateral output is never selected as an ordinary input.
  let ctx = DijkstraTest config view work batcher alice bob pp "" script
        (Testnet $ NetworkMagic $ fromIntegral $ testnetMagic runtime)
  txid <- fund ctx "collateral" [(address bob, 50_000_000, [])]
  action ctx{collateral = txid <> "#0"}

trim :: String -> String
trim = filter (`notElem` ['\n', '\r'])

address :: PaymentKeyInfo -> String
address = Text.unpack . paymentKeyInfoAddr

skey :: PaymentKeyInfo -> FilePath
skey = signingKeyFp . paymentKeyInfoPair

cli :: DijkstraTest -> [String] -> H.Integration String
cli ctx = execCli' (exec ctx)

queryJson :: DijkstraTest -> [String] -> H.Integration A.Value
queryJson ctx args = cli ctx (["dijkstra", "query"] ++ args) >>= H.leftFail . A.eitherDecode . B.pack

field :: A.FromJSON a => String -> A.Value -> H.Integration a
field key = H.leftFail . A.parseEither (A.withObject key (A..: K.fromString key))

parameter :: DijkstraTest -> String -> H.Integration Integer
parameter ctx key = H.readJsonFileOk (protocolParams ctx) >>= field key

largest :: DijkstraTest -> PaymentKeyInfo -> H.Integration UtxoEntry
largest ctx wallet = do
  entries <- queryUtxos (exec ctx) "" "" (address wallet)
  H.assert $ not $ null entries
  pure $ maximumBy (comparing ueLovelace) entries

-- | Wait for a particular output, not merely for the next block.
waitTx :: DijkstraTest -> String -> H.Integration ()
waitTx ctx txid = void $ retryUntilM (epochView ctx) (WaitForBlocks 10)
  (queryJson ctx ["utxo", "--tx-in", txid <> "#0", "--output-json"])
  (\v -> case v of A.Object o -> not (KM.null o); _ -> False)

assertSpent :: DijkstraTest -> String -> H.Integration ()
assertSpent ctx input = do
  value <- queryJson ctx ["utxo", "--tx-in", input, "--output-json"]
  value H.=== A.Object mempty

assertReference :: DijkstraTest -> String -> H.Integration ()
assertReference ctx input = do
  value <- queryJson ctx ["utxo", "--tx-in", input, "--output-json"]
  H.assert $ case value of A.Object o -> KM.member (K.fromString input) o; _ -> False

fee :: Integer
fee = 5_000_000

-- | Ordinary key-only setup transaction. Return its confirmed id.
fund :: DijkstraTest -> String -> [(String, Integer, [String])] -> H.Integration String
fund ctx label outputs = do
  input <- largest ctx (payer ctx)
  let body = directory ctx </> label <> ".body"
      amount = ueLovelace input - sum [n | (_, n, _) <- outputs] - fee
  H.assert $ amount > 0
  void $ cli ctx $
    ["dijkstra", "transaction", "build-raw", "--tx-in", ueTxIn input]
    ++ concat [["--tx-out", addr <> "+" <> show n] ++ extras | (addr, n, extras) <- outputs]
    ++ ["--tx-out", address (payer ctx) <> "+" <> show amount, "--fee", show fee, "--out-file", body]
  signSubmit ctx body [skey $ payer ctx]

signSubmit :: DijkstraTest -> FilePath -> [FilePath] -> H.Integration String
signSubmit ctx body keys = do
  signed <- signBody ctx body keys
  submitSigned ctx signed

signBody :: DijkstraTest -> FilePath -> [FilePath] -> H.Integration FilePath
signBody ctx body keys = do
  let signed = body <> ".signed"
  void $ cli ctx $ ["dijkstra", "transaction", "sign", "--tx-body-file", body]
    ++ concatMap (\key -> ["--signing-key-file", key]) keys ++ ["--out-file", signed]
  pure signed

submitSigned :: DijkstraTest -> FilePath -> H.Integration String
submitSigned ctx signed = do
  txid <- trim <$> cli ctx ["dijkstra", "transaction", "txid", "--tx-file", signed, "--output-text"]
  void $ cli ctx ["dijkstra", "transaction", "submit", "--tx-file", signed]
  waitTx ctx txid
  pure txid

-- | Build/sign a sub-transaction and embed it in a separately signed top-level
-- transaction. Balance adjustment is withdrawals/refunds minus deposits; the
-- caller balances the sub-inputs, outputs and mint separately.
submitSub :: DijkstraTest -> String -> [String] -> [FilePath] -> Integer -> H.Integration String
submitSub ctx label args keys adjustment = do
  H.note_ $ "Submit sub-transaction: " <> label
  signed <- buildSignedSub ctx label args keys
  input <- largest ctx (payer ctx)
  body <- buildOuter ctx label signed input adjustment []
  txid <- signSubmit ctx body [skey $ payer ctx, skey $ collateralWallet ctx]
  -- The batcher's confirmed change also checks deposit/refund/withdrawal accounting.
  changeOutput <- queryJson ctx ["utxo", "--tx-in", txid <> "#0", "--output-json"] >>= field (txid <> "#0")
  actual <- field "value" changeOutput >>= field "lovelace"
  actual H.=== ueLovelace input - fee + adjustment
  assertSpent ctx (ueTxIn input)
  pure txid

buildSignedSub :: DijkstraTest -> String -> [String] -> [FilePath] -> H.Integration FilePath
buildSignedSub ctx label args keys = do
  let unsigned = directory ctx </> label <> ".unsigned"
      signed = directory ctx </> label <> ".signed"
  void $ cli ctx $ ["dijkstra", "sub-transaction", "build-raw"] ++ args
    ++ ["--protocol-params-file", protocolParams ctx, "--out-file", unsigned]
  before <- cli ctx ["dijkstra", "sub-transaction", "txid", "--sub-tx-file", unsigned, "--output-text"]
  void $ cli ctx $ ["dijkstra", "sub-transaction", "sign", "--sub-tx-file", unsigned]
    ++ concatMap (\key -> ["--signing-key-file", key]) keys ++ ["--out-file", signed]
  after <- cli ctx ["dijkstra", "sub-transaction", "txid", "--signed-sub-tx-file", signed, "--output-text"]
  before H.=== after
  pure signed

-- | Keep input selection separate so rejection tests can retry the same inputs.
buildOuter :: DijkstraTest -> String -> FilePath -> UtxoEntry -> Integer -> [String] -> H.Integration FilePath
buildOuter ctx label signed input adjustment extras = do
  let body = directory ctx </> label <> ".body"
      change = ueLovelace input - fee + adjustment
  H.assert $ change > 0
  void $ cli ctx $
    [ "dijkstra", "transaction", "build-raw", "--tx-in", ueTxIn input
    , "--tx-in-collateral", collateral ctx
    , "--tx-out", address (payer ctx) <> "+" <> show change
    , "--fee", show fee, "--signed-sub-tx-file", signed
    , "--protocol-params-file", protocolParams ctx, "--out-file", body
    ] ++ extras
  pure body

keyInput :: DijkstraTest -> H.Integration [String]
keyInput ctx = do
  input <- largest ctx (owner ctx)
  pure ["--tx-in", ueTxIn input, "--tx-out", address (owner ctx) <> "+" <> show (ueLovelace input)]

scriptHash :: DijkstraTest -> FilePath -> H.Integration String
scriptHash ctx path = trim <$> cli ctx ["hash", "script", "--script-file", path]

scriptAddress :: DijkstraTest -> FilePath -> H.Integration String
scriptAddress ctx path = do
  -- The era-independent CLI address builder reads native scripts as Conway and
  -- crashes on RequireGuard (and rejects V4). Construct the setup address with the API;
  -- building, signing and submitting the tested sub-transactions still uses CLI.
  hash <- scriptHash ctx path >>= H.evalEither . deserialiseFromRawBytesHex . BS.pack
  pure $ Text.unpack $ serialiseAddress $
    makeShelleyAddress (testNetwork ctx) (PaymentCredentialByScript hash) NoStakeAddress

referenceOutput :: DijkstraTest -> String -> FilePath -> H.Integration String
referenceOutput ctx label script = do
  txid <- fund ctx label [(address $ collateralWallet ctx, 20_000_000, ["--tx-out-reference-script-file", script])]
  pure $ txid <> "#0"

-- CLI tuples are (steps, memory). Enough for the tiny program, below protocol limits.
units :: String
units = "(100000000,1000000)"

v4Witness :: ScriptSource -> String -> FilePath -> String -> [String]
v4Witness ViaFile purpose script _ =
  ["--" <> purpose <> "-script-file", script, "--" <> purpose <> "-redeemer-value", "0"
  , "--" <> purpose <> "-execution-units", units]
v4Witness ViaReference purpose _ ref =
  let prefix = if purpose == "tx-in" then "spending" else purpose
  in ["--" <> prefix <> "-tx-in-reference", ref, "--" <> prefix <> "-plutus-script-v4"
     , "--" <> prefix <> "-reference-tx-in-redeemer-value", "0"
     , "--" <> prefix <> "-reference-tx-in-execution-units", units]

-- Guard references use an ordinary read-only input and the script hash.
guardArgs :: ScriptSource -> FilePath -> String -> String -> [String]
guardArgs ViaFile script _ _ = ["--guard-script-file", script]
guardArgs ViaReference _ hash ref = ["--read-only-tx-in-reference", ref, "--guard-script-hash", hash]

-- | Reference scenarios must not accidentally pass by embedding the script too.
assertReferenceOnly :: DijkstraTest -> String -> H.Integration ()
assertReferenceOnly ctx label = do
  Exp.UnsignedSubTx tx <- H.evalEither =<< H.evalIO
    (readFileTextEnvelope (File $ directory ctx </> label <> ".unsigned"))
  H.assert $ Map.null (tx ^. L.witsTxL . L.scriptTxWitsL)
  H.assert $ not $ Set.null (tx ^. L.bodyTxL . L.referenceInputsTxBodyL)

stakeAddress :: DijkstraTest -> FilePath -> H.Integration String
stakeAddress ctx script = trim <$> cli ctx ["dijkstra", "stake-address", "build", "--stake-script-file", script]

registerStake :: DijkstraTest -> ScriptSource -> String -> FilePath -> String -> H.Integration String
registerStake ctx source label script ref = do
  deposit <- parameter ctx "stakeAddressDeposit"
  let cert = directory ctx </> label <> ".regcert"
  void $ cli ctx ["dijkstra", "stake-address", "registration-certificate", "--stake-script-file", script
    , "--key-reg-deposit-amt", show deposit, "--out-file", cert]
  input <- keyInput ctx
  void $ submitSub ctx label (input ++ ["--certificate-file", cert] ++ v4Witness source "certificate" script ref)
    [skey $ owner ctx] (-deposit)
  addr <- stakeAddress ctx script
  entries <- stakeInfo ctx addr
  length entries H.=== 1
  pure addr

stakeInfo :: DijkstraTest -> String -> H.Integration [A.Value]
stakeInfo ctx addr = do
  value <- queryJson ctx ["stake-address-info", "--address", addr]
  H.evalEither $ A.parseEither A.parseJSON value

rewardBalance :: DijkstraTest -> String -> H.Integration Integer
rewardBalance ctx addr = do
  [entry] <- stakeInfo ctx addr
  field "rewardAccountBalance" entry

-- Read a specific proposal using its sub-transaction id. Nested governance
-- actions belong to that body, not to the batcher's top-level transaction id.
subId :: DijkstraTest -> String -> H.Integration String
subId ctx label = trim <$> cli ctx ["dijkstra", "sub-transaction", "txid"
  , "--signed-sub-tx-file", directory ctx </> label <> ".signed", "--output-text"]

proposalState :: DijkstraTest -> String -> H.Integration A.Value
proposalState ctx txid = do
  proposals <- queryJson ctx ["gov-state"] >>= field "proposals"
  H.nothingFail $ List.find matches (proposals :: [A.Value])
 where
  matches (A.Object o) = case KM.lookup "actionId" o of
    Just (A.Object action) -> KM.lookup "txId" action == Just (A.String $ Text.pack txid)
    _ -> False
  matches _ = False

anchorArgs :: [String]
anchorArgs = ["--anchor-url", "https://example.com/dijkstra-smoke", "--anchor-data-hash", replicate 64 '0']
