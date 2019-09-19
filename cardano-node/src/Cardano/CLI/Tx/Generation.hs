{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Tx.Generation
  ( NumberOfTxs(..)
  , NumberOfOutputsPerTx(..)
  , FeePerTx(..)
  , TPSRate(..)
  , genesisBenchmarkRunner
  ) where

import           Prelude

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad (forM_, mapM, when)
import qualified Control.Monad.Class.MonadSTM as MSTM

import           Data.Bifunctor (bimap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.Foldable (find, foldl', foldr, toList)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (Maybe (..), fromMaybe, listToMaybe, mapMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word32, Word64)

import           Network.Socket (AddrInfo (..),
                     AddrInfoFlag (..), Family (..), SocketType (Stream),
                     addrFamily,addrFlags, addrSocketType, defaultHints,
                     getAddrInfo)

import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.Binary (Annotated (..), ToCBOR (..), reAnnotate)
import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (appendName)
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Config.Logging (LoggingLayer (..), Trace)
import           Cardano.Config.Types (CardanoConfiguration(..))
import qualified Cardano.Crypto as Crypto
import           Cardano.Node.Configuration.Topology (NetworkTopology (..),
                                                      TopologyInfo (..),
                                                      readTopologyFile)
import           Cardano.CLI.Tx (txSpendGenesisUTxOByronPBFT)
import           Cardano.CLI.Tx.BenchmarkingTxSubmission (ROEnv (..),
                                                          TraceBenchTxSubmit (..),
                                                          bulkSubmission)
import           Cardano.CLI.Tx.Submission (submitTx)
import           Cardano.CLI.Tx.BenchmarkingNodeToNode (BenchmarkTxSubmitTracers (..),
                                                        SendRecvConnect,
                                                        SendRecvTxSubmission,
                                                        benchmarkConnectTxSubmit)
import           Cardano.CLI.Tx.BenchmarkingTxSubClient
import           Control.Tracer (Tracer, contramap, traceWith)

import           Ouroboros.Consensus.Demo.Run (RunDemo)
import           Ouroboros.Consensus.Node.Run (RunNode)
import Ouroboros.Consensus.Block(BlockProtocol)
import           Ouroboros.Consensus.Ledger.Byron.Config (ByronConfig,
                                                          ByronEBBExtNodeConfig,
                                                          pbftProtocolMagic)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..),
                                                        ProtocolInfo (..),
                                                        protocolInfo)
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Ledger.Byron (ByronBlockOrEBB (..),
                                                   GenTx (..))
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Protocol.ExtNodeConfig (encNodeConfigExt)
import           Ouroboros.Consensus.Protocol.WithEBBs (NodeConfig (WithEBBNodeConfig))

newtype NumberOfTxs =
  NumberOfTxs Word64
  deriving (Eq, Ord, Show)

newtype NumberOfOutputsPerTx =
  NumberOfOutputsPerTx Int
  deriving (Eq, Ord, Show)

newtype FeePerTx =
  FeePerTx Word64
  deriving (Eq, Ord, Show)

newtype TPSRate =
  TPSRate Int
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------------------
-- | Genesis benchmark runner (we call it in 'Run.runNode').
--
--   Using a _richman_ (from genesis block) to supply some initial
--   amount of funds for disbursment.
-----------------------------------------------------------------------------------------
genesisBenchmarkRunner
  :: RunDemo (ByronBlockOrEBB ByronConfig)
  => LoggingLayer
  -> CardanoConfiguration
  -> Consensus.Protocol (ByronBlockOrEBB ByronConfig)
  -> TopologyInfo
  -> NumberOfTxs
  -> NumberOfOutputsPerTx
  -> FeePerTx
  -> TPSRate
  -> [FilePath]
  -> IO ()
genesisBenchmarkRunner loggingLayer
                       cc
                       protocol@(Consensus.ProtocolRealPBFT genesisConfig _ _ _ _)
                       topologyInfo
                       numOfTxs@(NumberOfTxs rawNumOfTxs)
                       numOfOutsPerTx
                       txFee
                       tpsRate
                       signingKeyFiles = do
  let (benchTracer, connectTracer, submitTracer, lowLevelSubmitTracer) = createTracers loggingLayer

  ProtocolInfo{pInfoConfig} <- prepareProtocolInfo protocol topologyInfo

  when (length signingKeyFiles < 3) $
    error "Tx generator: please provide at least 3 signing key files."

  -- 'genesisKey' is for genesis address with initial amount of money (1.4 billion ADA for now).
  -- 'sourceKey' is for source address that we'll use as a source of money for next transactions.
  -- 'recepientKey' is for recipient address that we'll use as an output for next transactions.
  (genesisKey:sourceKey:recepientKey:_) <- prepareSigningKeys signingKeyFiles

  let genesisUtxo = extractGenesisFunds genesisConfig [genesisKey]

  let genesisAddress   = mkAddressForKey pInfoConfig genesisKey
      sourceAddress    = mkAddressForKey pInfoConfig sourceKey
      recipientAddress = mkAddressForKey pInfoConfig recepientKey

  -- We have to prepare an initial funds (it's the money we'll send from 'genesisAddress' to
  -- 'sourceAddress'), this will be our very first transaction.
  prepareInitialFunds benchTracer
                      lowLevelSubmitTracer
                      cc
                      genesisConfig
                      pInfoConfig
                      topologyInfo
                      genesisUtxo
                      genesisAddress
                      sourceAddress

  -- Check if no transactions needed...
  when (rawNumOfTxs > 0) $
    runBenchmark benchTracer
                 connectTracer
                 submitTracer
                 lowLevelSubmitTracer
                 cc
                 pInfoConfig
                 sourceAddress
                 sourceKey
                 recipientAddress
                 topologyInfo
                 numOfTxs
                 numOfOutsPerTx
                 txFee
                 tpsRate

{-------------------------------------------------------------------------------
  Main logic
-------------------------------------------------------------------------------}

-- | The store of the unspent funds (available transaction outputs)
type AvailableFunds = Set FundValueStatus

data FundValueStatus = FundValueStatus
  { status    :: SettledStatus
  , txDetails :: !TxDetails
  } deriving (Show)

type TxDetails = (CC.UTxO.TxIn, CC.UTxO.TxOut)

instance Eq FundValueStatus where
  (==) FundValueStatus{txDetails = a} FundValueStatus{txDetails = b} = (==) a b

instance Ord FundValueStatus where
  compare FundValueStatus{txDetails = a} FundValueStatus{txDetails = b} = compare a b

-- | Used to mark a transasction that is known to be (or, in some
--   cases due to passage of time, assumed to be) committed on chain.
data SettledStatus
  = Unknown
  | Seen
  deriving (Eq, Show)

-- TBC
class (Ord r) => FiscalRecipient r where

instance FiscalRecipient Int where

prepareProtocolInfo
  :: forall blk. Consensus.Protocol blk
  -> TopologyInfo
  -> IO (ProtocolInfo blk)
prepareProtocolInfo protocol topologyInfo = do
  NetworkTopology nodeSetups <- readTopologyFile (topologyFile topologyInfo) >>=
    \case
      Left e  -> fail e
      Right t -> return t

  nodeId <-
    case node topologyInfo of
      RelayId{}  -> fail "Only core nodes are supported targets"
      CoreId nid -> return nid

  return $ protocolInfo (NumCoreNodes (length nodeSetups))
                        (CoreNodeId nodeId)
                        protocol

-----------------------------------------------------------------------------------------
-- Tracers.
-----------------------------------------------------------------------------------------

createTracers
  :: LoggingLayer
  -> ( Tracer IO (TraceBenchTxSubmit (Byron.GenTxId (ByronBlockOrEBB ByronConfig)))
     , Tracer IO SendRecvConnect
     , Tracer IO (SendRecvTxSubmission (ByronBlockOrEBB ByronConfig))
     , Tracer IO String
     )
createTracers loggingLayer =
  (benchTracer, connectTracer, submitTracer, lowLevelSubmitTracer)
 where
  basicTr :: Trace IO Text
  basicTr = llBasicTrace loggingLayer

  tr :: Trace IO Text
  tr = llAppendName loggingLayer "cli" basicTr

  tr' :: Trace IO Text
  tr' = appendName "generate-txs" tr

  trBenchTotext :: TraceBenchTxSubmit (Byron.GenTxId (ByronBlockOrEBB ByronConfig)) -> Text
  trBenchTotext = T.pack . show
  benchTracer :: Tracer IO (TraceBenchTxSubmit (Byron.GenTxId (ByronBlockOrEBB ByronConfig)))
  benchTracer = contramap trBenchTotext (toLogObject (appendName "benchmark" tr'))

  trConnectTotext :: SendRecvConnect -> Text
  trConnectTotext = T.pack . show
  connectTracer :: Tracer IO SendRecvConnect
  connectTracer = contramap trConnectTotext (toLogObject (appendName "connect" tr'))

  trSubmitTotext :: SendRecvTxSubmission (ByronBlockOrEBB ByronConfig) -> Text
  trSubmitTotext = T.pack . show
  submitTracer :: Tracer IO (SendRecvTxSubmission (ByronBlockOrEBB ByronConfig))
  submitTracer = contramap trSubmitTotext (toLogObject (appendName "submit" tr'))

  trStringTotext :: String -> Text
  trStringTotext = T.pack
  lowLevelSubmitTracer :: Tracer IO String
  lowLevelSubmitTracer = contramap trStringTotext (toLogObject (appendName "llSubmit" tr'))

-----------------------------------------------------------------------------------------
-- | Prepare signing keys and addresses for transactions.
-----------------------------------------------------------------------------------------

-- | We take files with secret keys and create signing keys from them,
--   we need it to be able to spend money from corresponding addresses.
prepareSigningKeys :: [FilePath] -> IO [Crypto.SigningKey]
prepareSigningKeys =
  mapM $ \file -> kmoDeserialiseDelegateKey <$> LB.readFile file
 where
  kmoDeserialiseDelegateKey =
    Crypto.SigningKey
    . snd
    . either (error . show) id
    . deserialiseFromBytes Crypto.fromCBORXPrv

mkAddressForKey
  :: NodeConfig ByronEBBExtNodeConfig
  -> Crypto.SigningKey
  -> CC.Common.Address
mkAddressForKey _pInfoConfig =
  CC.Common.makeVerKeyAddress networkMagic . Crypto.toVerification
 where
  -- The value is taken from a result of
  -- script 'issue-genesis-utxo-expenditure.sh'.
  networkMagic = CC.Common.NetworkTestnet 459045235

-----------------------------------------------------------------------------------------
-- Extract access to the Genesis funds.
-----------------------------------------------------------------------------------------

-- Extract the UTxO in the genesis block for the rich men
extractGenesisFunds
  :: CC.Genesis.Config
  -> [Crypto.SigningKey]
  -> Map Int (TxDetails, Crypto.SigningKey)
extractGenesisFunds genesisConfig signingKeys =
    Map.fromList
  . mapMaybe (\(inp, out) -> mkEntry (inp, out) <$> isRichman out)
  . fromCompactTxInTxOutList
  . Map.toList
  . CC.UTxO.unUTxO
  . CC.UTxO.genesisUtxo
  $ genesisConfig
 where
  mkEntry
    :: TxDetails
    -> (Int, Crypto.SigningKey)
    -> (Int, (TxDetails, Crypto.SigningKey))
  mkEntry txd (richman, key) = (richman, (txd, key))

  isRichman :: CC.UTxO.TxOut -> Maybe (Int, Crypto.SigningKey)
  isRichman out = listToMaybe $ filter (isValidKey . snd) richmen
    where
      isValidKey :: Crypto.SigningKey -> Bool
      isValidKey key =
        CC.Common.checkVerKeyAddress (Crypto.toVerification key) (CC.UTxO.txOutAddress out)

  richmen :: [(Int, Crypto.SigningKey)]
  richmen = zip [0..] signingKeys

  fromCompactTxInTxOutList
    :: [(CC.UTxO.CompactTxIn, CC.UTxO.CompactTxOut)]
    -> [TxDetails]
  fromCompactTxInTxOutList =
    map (bimap CC.UTxO.fromCompactTxIn CC.UTxO.fromCompactTxOut)

-----------------------------------------------------------------------------------------
-- Work with initial funds.
-----------------------------------------------------------------------------------------

-- Prepare and submit our first transaction: send money from 'initialAddress' to 'sourceAddress'
-- (latter corresponds to 'targetAddress' here) and "remember" it in 'availableFunds'.
prepareInitialFunds
  :: RunDemo (ByronBlockOrEBB ByronConfig)
  => Tracer IO (TraceBenchTxSubmit (Byron.GenTxId (ByronBlockOrEBB ByronConfig)))
  -> Tracer IO String
  -> CardanoConfiguration
  -> CC.Genesis.Config
  -> NodeConfig ByronEBBExtNodeConfig
  -> TopologyInfo
  -> Map Int ((CC.UTxO.TxIn, CC.UTxO.TxOut), Crypto.SigningKey)
  -> CC.Common.Address
  -> CC.Common.Address
  -> IO ()
prepareInitialFunds _tracer
                    llTracer
                    cc
                    genesisConfig
                    pInfoConfig
                    topologyInfo
                    genesisUtxo
                    genesisAddress
                    targetAddress = do
  let (mFunds, _, _initGenTx) = extractInitialFunds pInfoConfig
                                                    (genesisUtxo Map.! 0) -- corresponds to 'genesisAddress'.
                                                    targetAddress

  let ((_, _), signingKey) = genesisUtxo Map.! 0
      outBig = assumeBound $ CC.Common.mkLovelace 863000000000000
      outForBig = CC.UTxO.TxOut
        { CC.UTxO.txOutAddress = targetAddress
        , CC.UTxO.txOutValue   = outBig
        }

  let genesisTx :: GenTx (ByronBlockOrEBB ByronConfig)
      genesisTx = txSpendGenesisUTxOByronPBFT genesisConfig
                                              signingKey
                                              genesisAddress
                                              (NE.fromList [outForBig])

  submitTx cc pInfoConfig (node topologyInfo) genesisTx llTracer

  -- Done, the first transaction 'initGenTx' is submitted,
  -- now 'sourceAddress' has a lot of money.
  case mFunds of
    Nothing             -> return ()
    Just (txInIndex, _) -> do
      let txIn  = CC.UTxO.TxInUtxo (Byron.byronTxId genesisTx) txInIndex
          txOut = outForBig
      addToAvailableFunds (txIn, txOut)
    -- Now we can use these money for further transactions.

-- | Take the initial funds from a single richhombre to a specific address.
extractInitialFunds :: NodeConfig ByronEBBExtNodeConfig
                    -> (TxDetails, Crypto.SigningKey)
                    -> CC.Common.Address                    -- the address to send the funds
                    -> ( Maybe (Word32, CC.Common.Lovelace) -- the index and value of 'funds'
                       , CC.Common.Lovelace                 -- the associated fees
                       , GenTx (ByronBlockOrEBB cfg)
                       )
extractInitialFunds cfg input address =
  (funds, fees, initGenTx)
 where
  (funds, fees, _, initGenTx) =
    mkTransaction cfg
                  input
                  (Just address)
                  (Set.empty :: Set (Int, CC.UTxO.TxOut))

-- | Single input to multiple (in sequence) output transaction (special case for benchmarking).
mkTransaction
  :: (FiscalRecipient r)
  => NodeConfig ByronEBBExtNodeConfig
  -> (TxDetails, Crypto.SigningKey)       -- TxIn, TxOut that will be used as
                                          -- input and the key to spend the
                                          -- associated value
  -> Maybe CC.Common.Address              -- the address to associate with the 'change',
                                          -- if different from that of the first argument
  -> Set (r, CC.UTxO.TxOut)               -- each recipient and their payment details
  -> ( Maybe (Word32, CC.Common.Lovelace) -- the 'change' index and value (if any)
     , CC.Common.Lovelace                 -- the associated fees
     , Map r Word32                       -- the offset map in the transaction below
     , GenTx (ByronBlockOrEBB cfg)
     )
mkTransaction cfg ((txinFrom, txoutFrom), signingKey) mChangeAddress payments =
  (mChange, fees, offsetMap, genTx)
 where
  paymentsList = toList payments
  txOuts       = map snd paymentsList

  inpValue      = CC.UTxO.txOutValue txoutFrom
  totalOutValue = foldl' (\s txout -> s `addLovelace` CC.UTxO.txOutValue txout)
                         (assumeBound $ CC.Common.mkLovelace 0)
                         txOuts
  fees          = assumeBound $ CC.Common.mkLovelace 1000000
  changeValue   = inpValue `subLoveLace` (totalOutValue `addLovelace` fees)

      -- change the order of comparisons first check emptiness of txouts AND remove appendr after
  (txOutputs, mChange) =
    if (CC.Common.unsafeGetLovelace changeValue) > 0
    then
      let changeAddress = fromMaybe (CC.UTxO.txOutAddress txoutFrom) mChangeAddress
          changeTxOut   = CC.UTxO.TxOut {
                                      CC.UTxO.txOutAddress = changeAddress
                                    , CC.UTxO.txOutValue   = changeValue
                                    }
          changeIndex   = fromIntegral $ length txOuts -- 0-based index
      in
        (appendr txOuts (changeTxOut :| []), Just (changeIndex, changeValue))
    else
      case txOuts of
        []                 -> error "change is zero and txouts is empty"
        txout0: txoutsRest -> (txout0 :| txoutsRest, Nothing)

  -- TxOuts of recipients are placed at the first positions
  offsetMap = Map.fromList $ zipWith (\payment index -> (fst payment, index))
                                     paymentsList
                                     [0..]

  tx :: CC.UTxO.Tx
  tx = CC.UTxO.UnsafeTx
         { CC.UTxO.txInputs     = txinFrom :| []
         , CC.UTxO.txOutputs    = txOutputs
         , CC.UTxO.txAttributes = CC.Common.mkAttributes ()
         }

  genTx = generalizeTx cfg tx signingKey

-- | Append a non-empty list to a list.
-- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
appendr :: [a] -> NonEmpty a -> NonEmpty a
appendr l nel = foldr NE.cons nel l

-- | Annotate and sign transaction before submitting.
generalizeTx
  :: NodeConfig ByronEBBExtNodeConfig
  -> CC.UTxO.Tx
  -> Crypto.SigningKey -- signingKey for spending the input
  -> GenTx (ByronBlockOrEBB cfg)
generalizeTx (WithEBBNodeConfig config) tx signingKey =
  Byron.mkByronTx $ CC.UTxO.ATxAux (annotate tx) (annotate witness)
 where
  annotate
    :: forall a. ToCBOR a
    => a
    -> Annotated a ByteString
  annotate x = reAnnotate $ Annotated x ()
  witness = V.fromList
    [ CC.UTxO.VKWitness
        (Crypto.toVerification signingKey)
        (Crypto.sign
          (Crypto.getProtocolMagicId . pbftProtocolMagic . encNodeConfigExt $ config)
          -- provide ProtocolMagicId so as not to calculate it every time
          Crypto.SignTx
          signingKey
          (CC.UTxO.TxSigData (Crypto.hash tx))
        )
    ]

-----------------------------------------------------------------------------------------
-- Helpers for work with lovelaces.
-----------------------------------------------------------------------------------------

assumeBound :: Either CC.Common.LovelaceError CC.Common.Lovelace
            -> CC.Common.Lovelace
assumeBound (Left err) = error ("TxGeneration: " ++ show err)
assumeBound (Right ll) = ll

subLoveLace :: CC.Common.Lovelace -> CC.Common.Lovelace -> CC.Common.Lovelace
subLoveLace a b = assumeBound $ CC.Common.subLovelace a b

addLovelace :: CC.Common.Lovelace -> CC.Common.Lovelace -> CC.Common.Lovelace
addLovelace a b = assumeBound $ CC.Common.addLovelace a b

-----------------------------------------------------------------------------------------
-- | Work with funds. We store a set of tx information in a 'TVar' to be able to work
--   with it atomically. There to support potential multithreaded use.
-----------------------------------------------------------------------------------------

-- | Set of available tx outputs (for new transactions).
availableFunds :: TVar.TVar AvailableFunds
availableFunds = unsafePerformIO $ TVar.newTVarIO Set.empty

-- | Add to the available funds.
addToAvailableFunds
  :: TxDetails
  -> IO ()
addToAvailableFunds txDetails = do
  let fundValueStatus = FundValueStatus
                          { status    = Unknown
                          , txDetails = txDetails
                          }
  STM.atomically $
    TVar.modifyTVar' availableFunds (Set.insert fundValueStatus)

-- | Find a source of available funds, removing it from the availableFunds.
findAvailablefunds
  :: CC.Common.Lovelace      -- with at least this associated value
  -> (SettledStatus -> Bool) -- predicate for selecting on status
  -> IO (Maybe FundValueStatus)
findAvailablefunds valueThreshold predStatus =
  STM.atomically $ do
    funds <- TVar.readTVar availableFunds
    case find predFVS (Set.toList funds) of
      Nothing                  -> return Nothing
      j@(Just fundValueStatus) -> do
        -- Remove it from set of available funds (to prevent double spending).
        TVar.modifyTVar' availableFunds (Set.delete fundValueStatus)
        return j
 where
  -- Find the first tx output that contains sufficient amount of money.
  predFVS :: FundValueStatus -> Bool
  predFVS FundValueStatus{status, txDetails} =
       predStatus status
    && (CC.UTxO.txOutValue . snd $ txDetails) >= valueThreshold

-----------------------------------------------------------------------------------------
-- | Run benchmark using top level tracers..
-----------------------------------------------------------------------------------------

-- | Please note that there's a difference between Cardano tx and fiscal tx:
--   1. Cardano tx is a transaction from Cardano blockchain's point of view.
--   2. Fiscal tx is a transaction from recipient's point of view.
--   So if one Cardano tx contains 10 outputs (with addresses of 10 recipients),
--   we have 1 Cardano tx and 10 fiscal txs.
runBenchmark
  :: RunDemo (ByronBlockOrEBB ByronConfig)
  => Tracer IO (TraceBenchTxSubmit (Byron.GenTxId (ByronBlockOrEBB ByronConfig)))
  -> Tracer IO SendRecvConnect
  -> Tracer IO (SendRecvTxSubmission (ByronBlockOrEBB ByronConfig))
  -> Tracer IO String
  -> CardanoConfiguration
  -> NodeConfig ByronEBBExtNodeConfig
  -> CC.Common.Address
  -> Crypto.SigningKey
  -> CC.Common.Address
  -> TopologyInfo
  -> NumberOfTxs
  -> NumberOfOutputsPerTx
  -> FeePerTx
  -> TPSRate
  -> IO ()
runBenchmark benchTracer
             connectTracer
             submitTracer
             lowLevelSubmitTracer
             cc
             pInfoConfig
             _sourceAddress
             sourceKey
             recipientAddress
             topologyInfo
             numOfTxs
             numOfOutsPerTx
             txFee
             tpsRate = do
  traceWith benchTracer . TraceBenchTxSubDebug $ "******* Tx generator, phase 1: make enough available UTxO entries *******"
  createMoreFundCoins lowLevelSubmitTracer
                        cc
                        pInfoConfig
                        sourceKey
                        txFee
                        topologyInfo
                        numOfTxs

  traceWith benchTracer . TraceBenchTxSubDebug $ "******* Tx generator, phase 2: pay to recipients *******"
  let benchmarkTracers :: BenchmarkTxSubmitTracers IO (ByronBlockOrEBB ByronConfig)
      benchmarkTracers = BenchmarkTracers
                           { trSendRecvConnect      = connectTracer
                           , trSendRecvTxSubmission = submitTracer
                           }

  let localAddr :: Maybe Network.Socket.AddrInfo
      localAddr = Nothing

  let hints :: AddrInfo
      hints = defaultHints
        { addrFlags      = [AI_PASSIVE]
        , addrFamily     = AF_INET6
        , addrSocketType = Stream
        , addrCanonName  = Nothing
        }
  -- Corresponds to a node 0 (used by default).
  (remoteAddr:_) <- getAddrInfo (Just hints) (Just "::1") (Just "3000")

  let updROEnv
        :: ROEnv (Byron.GenTxId (ByronBlockOrEBB ByronConfig)) (GenTx (ByronBlockOrEBB ByronConfig))
        -> ROEnv (Byron.GenTxId (ByronBlockOrEBB ByronConfig)) (GenTx (ByronBlockOrEBB ByronConfig))
      updROEnv defaultROEnv =
        ROEnv { targetBacklog     = targetBacklog defaultROEnv
              , txNumServiceTime  = Nothing
              , txSizeServiceTime = Nothing
              }

  -- Run generator.
  txGenerator benchTracer
              cc
              pInfoConfig
              (node topologyInfo)
              recipientAddress
              sourceKey
              txFee
              numOfTxs
              numOfOutsPerTx
              tpsRate

  -- Launch tx submission threads.
  launchTxPeer benchTracer
               benchmarkTracers
               txSubmissionTerm
               pInfoConfig
               localAddr
               remoteAddr
               updROEnv
               txsForSubmission

-- | At this moment 'sourceAddress' contains a huge amount of money (lets call it A).
--   Now we have to split this amount to N equal parts, as a result we'll have
--   N UTxO entries, and alltogether these entries will contain the same amount A.
--   E.g. (1 entry * 1000 ADA) -> (10 entries * 100 ADA).
--   Technically all splitting transactions will send money back to 'sourceAddress'.
createMoreFundCoins
  :: RunDemo (ByronBlockOrEBB ByronConfig)
  => Tracer IO String
  -> CardanoConfiguration
  -> NodeConfig ByronEBBExtNodeConfig
  -> Crypto.SigningKey
  -> FeePerTx
  -> TopologyInfo
  -> NumberOfTxs
  -> IO ()
createMoreFundCoins llTracer cc pInfoConfig sourceKey (FeePerTx txFee) topologyInfo (NumberOfTxs numOfTxs) = do
  let feePerTx              = assumeBound . CC.Common.mkLovelace $ txFee
      numSplittingTxOuts    = numOfTxs -- number of splitting txout entries
      numOutsPerSplittingTx = 60 :: Word64 -- near the upper bound so as not to exceed the tx size limit

  -- Now we have to find the first output with sufficient amount of money.
  -- But since we made only one single transaction, there is only one
  -- 'FundValueStatus' in 'availableFunds', and it definitely contains a
  -- huge amount of money, so we can pick any amount.
  let someMoney = assumeBound . CC.Common.mkLovelace $ 10000000 -- 10 ADA
  findAvailablefunds someMoney (const True) >>=
    \case
      Nothing ->
        error "Impossible happened: cannot find the single output in 'availableFunds'!"
      Just FundValueStatus{txDetails} -> do
        let sourceAddress = CC.UTxO.txOutAddress . snd $ txDetails
            sourceValue   = CC.UTxO.txOutValue   . snd $ txDetails
            -- Split the funds to 'numSplittingTxOuts' equal parts, subtracting the possible fees.
            -- a safe number for fees is numSplittingTxOuts * feePerTx.
            splittedValue = assumeBound . CC.Common.mkLovelace $
                              ceiling (
                                ((fromIntegral (CC.Common.unsafeGetLovelace sourceValue)) :: Double)
                                / ((fromIntegral numSplittingTxOuts) :: Double)
                              ) - CC.Common.unsafeGetLovelace feePerTx
            -- The same output for all splitting transaction: send the same 'splittedValue'
            -- to the same 'sourceAddress'.
            !txOut        = CC.UTxO.TxOut
                              { CC.UTxO.txOutAddress = sourceAddress
                              , CC.UTxO.txOutValue   = splittedValue
                              }
            -- Create and sign splitting txs.
            splittingTxs  = createSplittingTxs pInfoConfig
                                                   (txDetails, sourceKey)
                                                   numSplittingTxOuts
                                                   numOutsPerSplittingTx
                                                   42
                                                   txOut
                                                   []

        forM_ splittingTxs $ \(tx, txDetailsList) -> do
          submitTx cc pInfoConfig (node topologyInfo) tx llTracer
          -- Update available fundValueStatus to reuse the numSplittingTxOuts TxOuts.
          forM_ txDetailsList addToAvailableFunds
 where
  -- create txs which split the funds to numTxOuts equal parts
  createSplittingTxs
    :: NodeConfig ByronEBBExtNodeConfig
    -> (TxDetails, Crypto.SigningKey)
    -> Word64
    -> Word64
    -> Int
    -> CC.UTxO.TxOut
    -> [(GenTx (ByronBlockOrEBB cfg), [TxDetails])]
    -> [(GenTx (ByronBlockOrEBB cfg), [TxDetails])]
  createSplittingTxs config details numTxOuts maxOutsPerInitTx identityIndex txOut acc
    | numTxOuts <= 0 = reverse acc
    | otherwise =
        let numOutsPerInitTx = min maxOutsPerInitTx numTxOuts
            sourceAddress = CC.UTxO.txOutAddress . snd . fst $ details
            -- same TxOut for all
            outs = Set.fromList $ zip [identityIndex .. identityIndex + (fromIntegral numOutsPerInitTx) - 1]
                                      (repeat txOut)
            (mFunds, _fees, outIndices, genTx) = mkTransaction config
                                                               details
                                                               Nothing
                                                               outs
            !txId = Byron.byronTxId genTx
            txDetailsList = (flip map) (Map.toList outIndices) $
                \(_, txInIndex) ->
                  let !txIn  = CC.UTxO.TxInUtxo txId txInIndex
                  in (txIn, txOut)
        in
          case mFunds of
            Nothing                 -> reverse $ (genTx, txDetailsList) : acc
            Just (txInIndex, value) ->
              let !txInChange  = CC.UTxO.TxInUtxo (Byron.byronTxId genTx) txInIndex
                  !txOutChange = CC.UTxO.TxOut
                                   { CC.UTxO.txOutAddress = sourceAddress
                                   , CC.UTxO.txOutValue   = value
                                   }
                  details' = ((txInChange, txOutChange), snd details)
              in
                -- from the change create the next tx with numOutsPerInitTx UTxO entries
                createSplittingTxs config
                                   details'
                                   (numTxOuts - numOutsPerInitTx)
                                   numOutsPerInitTx
                                   (identityIndex + fromIntegral numOutsPerInitTx)
                                   txOut
                                   ((genTx, txDetailsList) : acc)

-----------------------------------------------------------------------------------------
-- | Work with tx generator thread (for Phase 2).
-----------------------------------------------------------------------------------------

txGenerator
  :: Tracer IO (TraceBenchTxSubmit (Byron.GenTxId (ByronBlockOrEBB ByronConfig)))
  -> CardanoConfiguration
  -> NodeConfig ByronEBBExtNodeConfig
  -> NodeId
  -> CC.Common.Address
  -> Crypto.SigningKey
  -> FeePerTx
  -> NumberOfTxs
  -> NumberOfOutputsPerTx
  -> TPSRate
  -> IO ()
txGenerator benchTracer
            _cc
            cfg
            _nodeId
            recipientAddress
            sourceKey
            (FeePerTx txFee)
            (NumberOfTxs numOfTransactions)
            (NumberOfOutputsPerTx numOfOutsPerTx)
            _tps = do
  traceWith benchTracer . TraceBenchTxSubDebug $ " Prepare to generate, total number of transactions " ++ show numOfTransactions
  forM_ [1 .. numOfTransactions] $ \_ -> do
    -- TODO: currently we take the first available output, but don't check the 'status'.
    -- Later it should be changed: we have to use outputs from 'Seen' transactions only.
    findAvailablefunds totalValue (const True) >>=
      \case
        Nothing ->
          error "Cannot find sufficient amount for recipient's tx!"
        Just FundValueStatus{txDetails} -> do
          let txIn = (txDetails, sourceKey)
          let (_, _, _, tx :: GenTx (ByronBlockOrEBB ByronConfig)) = mkTransaction cfg
                                                                                   txIn
                                                                                   (Just addressForChange)
                                                                                   recipients
          -- Write newly generated tx in the list.
          -- It will be handled by 'bulkSubmission' function.
          writeTxInList tx

  traceWith benchTracer . TraceBenchTxSubDebug $ " Done, " ++ show numOfTransactions ++ " were generated and written in a list."
 where
  -- Num of recipients is equal to 'numOuts', so we think of
  -- recipients as the people we're going to pay to.
  recipients = Set.fromList $ zip [initRecipientIndex .. initRecipientIndex + numOfOutsPerTx - 1]
                                  (repeat txOut)
  initRecipientIndex = 0 :: Int
  -- The same output for all transactions.
  !txOut = CC.UTxO.TxOut
             { CC.UTxO.txOutAddress = recipientAddress
             , CC.UTxO.txOutValue   = valueForRecipient
             }
  valueForRecipient = assumeBound . CC.Common.mkLovelace $ 100000000 -- 100 ADA, discuss this value.
  totalValue = valueForRecipient `addLovelace` txFeeInLovelaces
  txFeeInLovelaces = assumeBound . CC.Common.mkLovelace $ txFee
  -- Send possible change to the same 'recipientAddress'.
  addressForChange = recipientAddress

---------------------------------------------------------------------------------------------------
-- TVar for submitter termination.
---------------------------------------------------------------------------------------------------

txSubmissionTerm :: MSTM.LazyTVar IO Bool
txSubmissionTerm = unsafePerformIO $ STM.newTVarIO False

-- stopTxSubmission :: IO ()
-- stopTxSubmission = STM.atomically $
--     STM.modifyTVar' txSubmissionTerm (const True)

---------------------------------------------------------------------------------------------------
-- Txs for submission.
---------------------------------------------------------------------------------------------------

-- | Generator is producing transactions and writes them in the list.
--   Later sumbitter is reading and submitting these transactions.
txsForSubmission :: forall tx . MSTM.LazyTMVar IO [tx]
txsForSubmission = unsafePerformIO $ STM.newTMVarIO []

writeTxInList :: forall tx . tx -> IO ()
writeTxInList tx = STM.atomically $
  STM.tryTakeTMVar txsForSubmission >>=
    \case
      Nothing      -> STM.putTMVar txsForSubmission [tx]
      Just curList -> STM.putTMVar txsForSubmission $ curList ++ [tx]

-- | To get higher performance we need to hide latency of getting and
-- forwarding (in sufficient numbers) transactions.

launchTxPeer
  :: forall m block txid tx.
     ( RunNode block
     , m ~ IO
     , txid ~ Byron.GenTxId block
     , tx ~ GenTx block
     )
  => Tracer m (TraceBenchTxSubmit txid)
  -- Tracer carrying the benchmarking events
  -> BenchmarkTxSubmitTracers m block
  -- tracer for lower level connection and details of
  -- protocol interactisn, intended for debugging
  -- associated issues.
  -> MSTM.LazyTVar m Bool
  -- a "global" stop variable, set to True to force shutdown
  -> NodeConfig (Ouroboros.Consensus.Block.BlockProtocol block)
  -- the configuration
  -> Maybe Network.Socket.AddrInfo
  -- local address binding (if wanted)
  -> Network.Socket.AddrInfo
  -- Remote address
  -> (ROEnv txid tx -> ROEnv txid tx)
  -- modifications to the submission engine enviroment to
  -- control rate etc
  -> MSTM.LazyTMVar m [tx]
  -- give this peer 1 or more transactions, empty list
  -- signifies stop this peer
  -> m ()
launchTxPeer tr1 tr2 termTM nc localAddr remoteAddr updROEnv txInChan = do
  tmv <- MSTM.newEmptyTMVarM
  remoteA <- async $ benchmarkConnectTxSubmit tr2 nc localAddr remoteAddr (txSubmissionClient tmv)
  txEngA <- async $ bulkSubmission updROEnv tr1 termTM txInChan tmv
  _ <- waitBoth remoteA txEngA
  pure ()
