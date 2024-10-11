{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Cardano.Benchmarking.Script.Selftest
Description : Run self-tests using statically-defined data.

The statically-defined data is the action list to execute, 'testScript'.
It actually does use a protocol file taken in from IO.
-}
module Cardano.Benchmarking.Script.Selftest
where

import           Cardano.Api hiding (Env)

import           Cardano.Benchmarking.LogTypes (EnvConsts (..))
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (prettyPrint)
import           Cardano.Benchmarking.Script.Env as Env (Env (..))
import qualified Cardano.Benchmarking.Script.Env as Env (Error, runActionMEnv, setBenchTracers)
import           Cardano.Benchmarking.Script.Types
import           Cardano.Benchmarking.Tracer (initNullTracers)
import qualified Cardano.Ledger.Coin as L
import           Cardano.TxGenerator.Setup.SigningKey
import           Cardano.TxGenerator.Types

import           Prelude

import qualified Control.Concurrent.STM as STM (atomically, readTVar)
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either (fromRight)
import qualified Data.List as List (unwords)
import           Data.String

import           Paths_tx_generator

-- | 'runSelftest' is the interface to actually run the self-test.
-- @iom@ is the IO manager from "Ouroboros.Network.IOManager".
-- @outFile@ is the file to output to;
-- 'Cardano.Benchmarking.Script.Core.evalGenerator' returns a
-- transaction 'Streaming.Stream' that
-- 'Cardano.Benchmarking.Script.Core.submitInEra'
-- does 'show' and 'writeFile' on.
runSelftest :: Env -> EnvConsts -> Bool -> Maybe FilePath -> IO (Either Env.Error ())
runSelftest env envConsts@EnvConsts { .. } doVoting outFile = do
  protocolFile <-  getDataFileName pparamFile
  let
    submitMode = maybe DiscardTX DumpToFile outFile
    fullScript = do
        Env.setBenchTracers initNullTracers
        forM_ (useThisScript protocolFile submitMode) action
  (result, Env {  }, ()) <- Env.runActionMEnv env fullScript envConsts
  abcMaybe <- STM.atomically $ STM.readTVar envThreads
  case abcMaybe of
    Just _ -> error $
          List.unwords
              [ "Cardano.Benchmarking.Script.Selftest.runSelftest:"
              , "thread state spuriously initialized" ]
    Nothing  -> pure result
  where
    pparamFile    = "data/" ++ if doVoting then "protocol-parameters-conway-voting.json" else "protocol-parameters.json"
    useThisScript =  if doVoting then testScriptVoting else testScript

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
    skey = fromRight (error "could not parse hardcoded signing key") $
      parsePaymentKeyTE $
        TextEnvelope {
            teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
          , teDescription = fromString "Genesis Initial UTxO Signing Key"
          , teRawCBOR = "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h\153\171\SI/m\186\242D\228\NAK\182(&\162"
          }
    era = AnyCardanoEra AllegraEra
    txParams = defaultTxGenTxParams {txParamFee = 1000000}
    genesisWallet = "genesisWallet"
    splitWallet1 = "SplitWallet-1"
    splitWallet2 = "SplitWallet-2"
    splitWallet3 = "SplitWallet-3"
    doneWallet = "doneWallet"
    key = "pass-partout"
    createChange :: String -> String -> Int -> Int -> Action
    createChange src dest txCount outputs
      = Submit era submitMode txParams $ Take txCount $ Cycle $ SplitN src (PayToAddr key dest) outputs

testScriptVoting :: FilePath -> SubmitMode -> [Action]
testScriptVoting protocolFile submitMode =
  [ SetProtocolParameters (UseLocalProtocolFile protocolFile)
  , SetNetworkId (Testnet (NetworkMagic {unNetworkMagic = 42}))
  , InitWallet genesisWallet
  , DefineSigningKey key skey
  , AddFund era genesisWallet
    (TxIn "900fc5da77a0747da53f7675cbb7d149d46779346dea2f879ab811ccc72a2162" (TxIx 0))
    (L.Coin 90000000000000) key

  , DefineStakeKey stakeKey

  -- TODO: manually inject an (unnamed) DRep key into the Env by means of a new Action constructor
  -- DefineDRepKey _drepKey

  , Submit era submitMode txParams
      EmptyStream
      -- TODO: instead, create 4(?) proposal transactions using the new constructor for Generator
      -- $ Take 4 $ Cycle $ <Proposal constructor>

  , Submit era submitMode txParams
      EmptyStream
      -- TODO: instead, create 8(?) vote transactions using the new constructor for Generator
      -- $ Take 8 $ Cycle $ <Vote constructor>

  ]
  where
    skey :: SigningKey PaymentKey
    skey = fromRight (error "could not parse hardcoded signing key") $
      parsePaymentKeyTE $
        TextEnvelope {
            teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
          , teDescription = fromString "Genesis Initial UTxO Signing Key"
          , teRawCBOR = "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h\153\171\SI/m\186\242D\228\NAK\182(&\162"
          }

    _drepKey :: SigningKey DRepKey
    _drepKey = fromRight (error "could not parse hardcoded drep key") $
      parseDRepKeyBase16 "5820aa7f780a2dcd099762ebc31a43860c1373970c2e2062fcd02cceefe682f39ed8"

    stakeKey :: VerificationKey StakeKey
    stakeKey = fromRight (error "could not parse hardcoded stake key") $
      parseStakeKeyBase16 "5820bbbfe3f3b71b00d1d61f4fe2a82526597740f61a0aa06f1324557925803c7d3e"

    era = AnyCardanoEra ConwayEra
    txParams = defaultTxGenTxParams {txParamFee = 1000000}
    genesisWallet = "genesisWallet"
    key = "pass-partout"
