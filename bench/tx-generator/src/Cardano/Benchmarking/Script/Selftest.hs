{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
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
import qualified Cardano.Crypto.Hash.Class as Crypto
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
import           Control.Monad (forM_, replicateM)
import           Control.Monad.Extra (maybeM, whileM)
import           Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST (runST)
import           Control.Monad.Trans.RWS (RWST)
import qualified Control.Monad.Trans.RWS as RWS (ask, asks, evalRWST, get, gets, put, modify, tell)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Either (fromRight)
import qualified Data.Foldable as Fold (toList)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (empty, insert, keysSet, null, restrictKeys, split, update)
import qualified Data.IntSet as IntSet (null)
import qualified Data.List as List (unwords)
import           Data.Maybe (fromJust)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq (singleton)
import           Data.String
import qualified Data.Tuple.Extra as Tuple (uncurry3)
import           Numeric.Natural (Natural)
import           System.FilePath ((</>))

import           Paths_tx_generator


skey :: SigningKey PaymentKey
skey = error "could not parse hardcoded signing key" `fromRight`
  parsePaymentKeyTE TextEnvelope
    { teType = TextEnvelopeType "GenesisUTxOSigningKey_ed25519"
    , teDescription = fromString "Genesis Initial UTxO Signing Key"
    , teRawCBOR =  "X \vl1~\182\201v(\152\250A\202\157h0\ETX\248h"
                <> "\153\171\SI/m\186\242D\228\NAK\182(&\162" }

drepKey :: SigningKey DRepKey
drepKey = error "could not parse hardcoded drep key" `fromRight`
  parseDRepKeyBase16 ("5820aa7f780a2dcd099762ebc31a43860c"
                   <> "1373970c2e2062fcd02cceefe682f39ed8")

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

txParams :: TxGenTxParams
txParams = defaultTxGenTxParams { txParamFee = 1000000 }

type Wallet = String
genesisWallet :: Wallet
genesisWallet = "genesisWallet"

type KeyName = String
key :: KeyName
key = "pass-partout"

fundTxId :: TxId
fundTxId = TxId . fromJust . Crypto.hashFromBytesAsHex
                $ "900fc5da77a0747da53f7675cbb7d149"
               <> "d46779346dea2f879ab811ccc72a2162"

fundTxIn :: TxIn
fundTxIn = fundTxId `TxIn` TxIx 0

fundAmount :: Integer
fundAmount = 90000000000000

fundCoins :: L.Coin
fundCoins  = L.Coin fundAmount

-- | 'printJSON' prints out the list of actions using Aeson.
-- It has no callers within @cardano-node@.
printJSON :: IO ()
printJSON = BSL.putStrLn . prettyPrint $ testScript "/dev/zero" DiscardTX

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
  = do protocolFile <- getDataFileName $
         "data" </> if doVoting then "protocol-parameters-conway-voting.json"
                                else "protocol-parameters.json"
       let useThisScript
            | not doVoting = testScript protocolFile submitMode
            | otherwise = testScriptVoting protocolFile submitMode
       (result, Env {  }, ()) <- flip (Env.runActionMEnv env) envConsts do
             Env.setBenchTracers initNullTracers
             mapM_ action useThisScript
       pure result `maybeM` const (throw abcException) $
         STM.atomically do STM.readTVar envThreads

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
  , AddFund anyAllegraEra genesisWallet fundTxIn fundCoins key
  , createChange genesisWallet splitWallet1 1 10
  -- 10 TXs with 30 outputs -> in total 300 outputs
  , createChange splitWallet1 splitWallet2 10 30
  , createChange splitWallet2 splitWallet3 300 30
{-
  , createChange genesisWallet splitWallet3 1 10
  -- Fifo implementation should also work fine when sourceWallet==destWallet
  , createChange splitWallet3 splitWallet3 10 30
  , createChange splitWallet3 splitWallet3 300 30
-}

  , Submit anyAllegraEra submitMode txParams $ Take 4000 $ Cycle
      $ NtoM splitWallet3 (PayToAddr key doneWallet) 2 2 Nothing Nothing
  ]
  where
    anyAllegraEra :: AnyCardanoEra
    anyAllegraEra = AnyCardanoEra AllegraEra
    splitWallet1, splitWallet2, splitWallet3, doneWallet :: Wallet
    splitWallet1 = "SplitWallet-1"
    splitWallet2 = "SplitWallet-2"
    splitWallet3 = "SplitWallet-3"
    doneWallet = "doneWallet"
    createChange :: String -> String -> Int -> Int -> Action
    createChange src dest txCount outputs
      = Submit anyAllegraEra submitMode txParams . Take txCount . Cycle $
          SplitN src (PayToAddr key dest) outputs

data GAScrEnv = GAScrEnv
  { gaseIdxCtr :: Natural
  , gaseIdxLive :: IntMap Natural
  } deriving (Eq, Ord, Read, Show)

data GAScrConsts = GAScrConsts
  { gascBatch :: Natural
  , gascQuorum :: Natural
  } deriving (Eq, Ord, Read, Show)

type GAScrMonad monad t = RWST GAScrConsts (Seq Generator) GAScrEnv monad t

gaScrConsts :: GAScrConsts
gaScrConsts = GAScrConsts
  { gascBatch = 5
  , gascQuorum = 10 }

gaScrEnvInit :: GAScrEnv
gaScrEnvInit = GAScrEnv
  { gaseIdxCtr = 0
  , gaseIdxLive = IntMap.empty }

gaScrTell :: Monad monad => Generator -> GAScrMonad monad ()
gaScrTell = RWS.tell . Seq.singleton

evalGAScr :: Monad monad => GAScrMonad monad t -> monad (t, Seq Generator)
evalGAScr = Tuple.uncurry3 RWS.evalRWST . (, gaScrConsts, gaScrEnvInit)

mkGovActGen :: Natural -> Generator
mkGovActGen batch = Sequence . Fold.toList . snd . ST.runST $ stGen batch where
  batchSize = fromIntegral $ gascBatch gaScrConsts
  n' = (n + batch - 1) `quot` batch
  stGen :: Natural -> forall s . ST s ([[Natural]], Seq Generator)
  stGen (fromIntegral -> batch) = evalGAScr do
    mkProposalBatch
    mkVoteBatch batch

mkProposalBatch :: Monad monad => GAScrMonad monad [Natural]
mkProposalBatch = do
  batch <- fromIntegral <$> RWS.asks gascBatch
  replicateM batch mkProposal

mkProposal :: Monad monad => GAScrMonad monad Natural
mkProposal = do
  quorum <- RWS.asks gascQuorum
  GAScrEnv { gaseIdxCtr = idx, gaseIdxLive = refCounts } <- RWS.get
  let idx' = fromIntegral idx
  RWS.put GAScrEnv
    { gaseIdxCtr = idx + 1
    , gaseIdxLive = IntMap.insert idx' quorum refCounts }
  gaScrTell $ Propose genesisWallet payMode proposalCoins stakeCred anchor
  pure idx

-- This is intended to accommodate future less-rigid ordering of
-- quorum-reaching.
mkVoteBatch :: Monad monad => Natural -> GAScrMonad monad ()
mkVoteBatch (fromIntegral -> batch) = do
  GAScrConsts {..} <- RWS.ask
  GAScrEnv {..} <- RWS.get
  let trimLow
        | batch > 0
        = snd $ IntMap.split (batch * gascBatch' - 1) gaseIdxLive
        | otherwise = gaseIdxLive
      trimHi = fst $ IntMap.split ((batch + 1) * gascBatch') trimLow
      gascBatch' = fromIntegral gascBatch
      idxs = IntMap.keysSet trimHi
  whenM (not . IntSet.null . flip IntMap.restrictKeys idxs <$> RWS.gets gaseIdxLive) do
    forM_ idxs \idx -> do
      gaScrTell $
        Vote genesisWallet payMode idx Yes drepCred Nothing
      RWS.modify \gase@GAScrEnv { gaseIdxLive = liveIdxs } ->
        gase { gaseIdxLive = Tuple.uncurry3 IntMap.update $
          (, idx, liveIdxs) \case refCount
                                     | refCount == 0
                                     -> error $ "gaseMkVoteBatch: refCount underflow"
                                     | refCount == 1
                                     -> Nothing
                                     | otherwise
                                     -> Just $ refCount - 1 }
 
anchor :: L.Anchor L.StandardCrypto
anchor = L.Anchor {..} where
  anchorUrl = fromJust $ L.textToUrl 999 "example.com"
  anchorDataHash = L.hashAnchorData . L.AnchorData . encodeUtf8 $
    L.urlToText anchorUrl

anyConwayEra :: AnyCardanoEra
anyConwayEra = AnyCardanoEra ConwayEra

payMode :: PayMode
payMode = PayToAddr key genesisWallet

proposalCoins :: L.Coin
proposalCoins = L.Coin $ fundAmount `div` fromIntegral nrProposals

stakeCred :: L.Credential 'L.Staking L.StandardCrypto
stakeCred = L.KeyHashObj . L.hashKey $ unStakeVerificationKey stakeKey

stakeKey :: VerificationKey StakeKey
stakeKey = error "could not parse hardcoded stake key" `fromRight`
  parseStakeKeyBase16 ("5820bbbfe3f3b71b00d1d61f4fe2a82526"
                    <> "597740f61a0aa06f1324557925803c7d3e")

testScriptVoting :: FilePath -> SubmitMode -> [Action]
testScriptVoting protocolFile submitMode =
  [ SetProtocolParameters (UseLocalProtocolFile protocolFile)
  , SetNetworkId (Testnet (NetworkMagic {unNetworkMagic = 42}))
  , InitWallet genesisWallet
  , DefineSigningKey key skey
  , AddFund anyConwayEra genesisWallet fundTxIn fundCoins key

  , DefineStakeKey stakeKey

  -- manually inject an (unnamed) DRep key into the Env by means of an Action constructor
  , DefineDRepKey drepKey

  , Submit anyConwayEra submitMode txParams . Sequence . replicate nrProposals $
      Propose genesisWallet payMode proposalCoins stakeCred anchor

  -- In principle, one could construct votes according to
  -- GovActionIds here. The approach taken instead was to
  -- refer to proposals by their indices into the govProposals
  -- list of the envGovStateSummary in the Env.
  , Delay 60.0

  -- There will be more point to this once there is more of a way to cast
  -- votes as several distinct credential-holders.
  , Submit anyConwayEra submitMode txParams . Take nrProposals $ RoundRobin
      [Sequence . replicate nrProposals $
         Vote genesisWallet payMode k Yes drepCred Nothing | k <- [0 .. nrProposals - 1]]
  ]
