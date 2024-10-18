{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Cardano.Benchmarking.Script.Selftest
Description : Run self-tests using statically-defined data.

The statically-defined data is the action list to execute, 'testScript'.
It actually does use a protocol file taken in from IO.
-}
module Cardano.Benchmarking.Script.Selftest
where

import           Cardano.Api hiding (Env)
import           Cardano.Api.Shelley (Vote (..))

import           Cardano.Benchmarking.LogTypes (EnvConsts (..))
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (prettyPrint)
import           Cardano.Benchmarking.Script.Env as Env (Env (..))
import qualified Cardano.Benchmarking.Script.Env as Env (Error, runActionMEnv, setBenchTracers)
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Tracer (initNullTracers)
import qualified Cardano.Crypto.DSIGN as Crypto
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Crypto as L
import qualified Cardano.Ledger.Keys as L
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Types

import           Cardano.Prelude (encodeUtf8)
import           Prelude

import qualified Control.Concurrent.STM as STM (atomically, readTVar)
import           Control.Exception (AssertionFailed (..), throw)
import           Control.Monad.Extra (maybeM)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either (fromRight)
import           Data.IORef (newIORef)
import qualified Data.List as List (unwords)
import           Data.Maybe (fromJust)
import           Data.String
import           System.FilePath ((</>))

import           Paths_tx_generator

-- | 'runSelftest' is the interface to actually run the self-test.
-- @iom@ is the IO manager from "Ouroboros.Network.IOManager".
-- @outFile@ is the file to output to;
-- 'Cardano.Benchmarking.Script.Core.evalGenerator' returns a
-- transaction 'Streaming.Stream' that
-- 'Cardano.Benchmarking.Script.Core.submitInEra'
-- does 'show' and 'writeFile' on.
runSelftest :: Env -> EnvConsts -> Bool -> Maybe FilePath -> IO (Either Env.Error ())
runSelftest env envConsts@EnvConsts { .. } doVoting outFile
  | submitMode <- DiscardTX `maybe` DumpToFile $ outFile
  , abcException <- AssertionFailed $ List.unwords
      [ "Cardano.Benchmarking.Script.Selftest.runSelftest:"
      , "thread state spuriously initialized" ]
  = do protocolFile <-  getDataFileName $
         "data" </> if doVoting then "protocol-parameters-conway-voting.json"
                                else "protocol-parameters.json"
       gsIORef <- GovStateIORef . Just <$> newIORef undefined
       let useThisScript
            | not doVoting = testScript protocolFile submitMode
            | otherwise = testScriptVoting protocolFile submitMode <>
                [QuiesceGovState gsIORef]
       (result, Env {  }, ()) <- flip (Env.runActionMEnv env) envConsts do
             Env.setBenchTracers initNullTracers
             mapM_ action useThisScript
             -- In principle, one could construct votes according to
             -- GovActionIds here. The approach taken instead was to
             -- refer to proposals by their indices into the govProposals
             -- list of the envGovStateSummary in the Env.
             action . Submit anyConwayEra submitMode txParams . Take nrProposals . Cycle $ Sequence
               [Vote genesisWallet payMode k Yes drepCred Nothing | k <- [0 .. nrProposals - 1]]
       pure result `maybeM` const (throw abcException) $ STM.atomically $ STM.readTVar envThreads

-- | 'printJSON' prints out the list of actions using Aeson.
-- It has no callers within @cardano-node@.
printJSON :: IO ()
printJSON = BSL.putStrLn $ prettyPrint $ testScript "/dev/zero" DiscardTX

-- | 'testScript' is a static list of 'Action' parametrised with a
-- file name and a mode indicating how to submit a transaction in
-- 'SubmitMode' passed along as a parameter within a 'Submit' action.
testScript :: FilePath -> SubmitMode -> [Action]
testScript protocolFile submitMode =
  [ SetProtocolParameters (UseLocalProtocolFile protocolFile)
  , SetNetworkId (Testnet (NetworkMagic {unNetworkMagic = 42}))
  , InitWallet genesisWallet
  , InitWallet splitWallet1
  , InitWallet splitWallet2
  , InitWallet splitWallet3
  , InitWallet doneWallet
  , DefineSigningKey key skey
  , AddFund era genesisWallet
    (TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (TxIx 0))
    (L.Coin 90000000000000) key
  , createChange genesisWallet splitWallet1 1 10
  , createChange splitWallet1 splitWallet2 10 30 -- 10 TXs with 30 outputs -> in total 300 outputs
  , createChange splitWallet2 splitWallet3 300 30
{-
  , createChange genesisWallet splitWallet3 1 10
  -- Fifo implementation should also work fine when sourceWallet==destWallet
  , createChange splitWallet3 splitWallet3 10 30
  , createChange splitWallet3 splitWallet3 300 30
-}

  , Submit era submitMode txParams $ Take 4000 $ Cycle
      $ NtoM splitWallet3 (PayToAddr key doneWallet) 2 2 Nothing Nothing
  ]
  where
    era = AnyCardanoEra AllegraEra
    skey = error "could not parse hardcoded signing key" `fromRight`
      parsePaymentKeyTE TextEnvelope
          { teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
          , teDescription = fromString "Genesis Initial UTxO Signing Key"
          , teRawCBOR =  "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h"
                      <> "\153\171\SI/m\186\242D\228\NAK\182(&\162" }
    splitWallet1 = "SplitWallet-1"
    splitWallet2 = "SplitWallet-2"
    splitWallet3 = "SplitWallet-3"
    doneWallet = "doneWallet"
    key = "pass-partout"
    createChange :: String -> String -> Int -> Int -> Action
    createChange src dest txCount outputs
      = Submit anyConwayEra submitMode txParams $ Take txCount $ Cycle $ SplitN src (PayToAddr key dest) outputs

drepKey :: SigningKey DRepKey
drepKey = error "could not parse hardcoded drep key" `fromRight`
  parseDRepKeyBase16 "5820aa7f780a2dcd099762ebc31a43860c1373970c2e2062fcd02cceefe682f39ed8"

drepCred :: forall crypto . ()
         => L.Crypto crypto
         => L.DSIGN crypto ~ Crypto.Ed25519DSIGN
         => L.Credential L.DRepRole crypto
drepCred
  | DRepSigningKey unDRepSigningKey <- drepKey
  , drepVerKey <- Crypto.deriveVerKeyDSIGN unDRepSigningKey
  = L.KeyHashObj . L.hashKey $ L.VKey drepVerKey

nrProposals :: Int
nrProposals = 4
payMode :: PayMode
payMode = PayToAddr "pass-partout" genesisWallet
anyConwayEra :: AnyCardanoEra
anyConwayEra = AnyCardanoEra ConwayEra
txParams :: TxGenTxParams
txParams = defaultTxGenTxParams {txParamFee = 1000000}

type Wallet = String
genesisWallet :: Wallet
genesisWallet = "genesisWallet"

testScriptVoting :: FilePath -> SubmitMode -> [Action]
testScriptVoting protocolFile submitMode =
  [ SetProtocolParameters (UseLocalProtocolFile protocolFile)
  , SetNetworkId (Testnet (NetworkMagic {unNetworkMagic = 42}))
  , InitWallet genesisWallet
  , DefineSigningKey key skey
  , AddFund anyConwayEra genesisWallet
    (TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (TxIx 0))
    fundCoins key

  , DefineStakeKey stakeKey

  -- manually inject an (unnamed) DRep key into the Env by means of an Action constructor
  , DefineDRepKey drepKey

  , Submit anyConwayEra submitMode txParams . Take nrProposals . Cycle $
      Propose genesisWallet payMode proposalCoins stakeCred anchor
  ]
  where
    fundAmount = 90000000000000
    fundCoins  = L.Coin fundAmount
    proposalCoins = L.Coin $ fundAmount `div` fromIntegral nrProposals
    skey :: SigningKey PaymentKey
    skey = error "could not parse hardcoded signing key" `fromRight`
      parsePaymentKeyTE TextEnvelope
          { teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
          , teDescription = fromString "Genesis Initial UTxO Signing Key"
          , teRawCBOR = "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h" <>
                        "\153\171\SI/m\186\242D\228\NAK\182(&\162" }

    stakeCred :: L.Credential 'L.Staking L.StandardCrypto
    stakeCred = L.KeyHashObj . L.hashKey $ unStakeVerificationKey stakeKey

    stakeKey :: VerificationKey StakeKey
    stakeKey = error "could not parse hardcoded stake key" `fromRight`
      parseStakeKeyBase16 "5820bbbfe3f3b71b00d1d61f4fe2a82526597740f61a0aa06f1324557925803c7d3e"

    anchor :: L.Anchor L.StandardCrypto
    anchor = L.Anchor
      { anchorUrl = fromJust $ L.textToUrl 999 "example.com"
      , anchorDataHash = L.hashAnchorData . L.AnchorData . encodeUtf8 . L.urlToText $ L.anchorUrl anchor }

    key = "pass-partout"
