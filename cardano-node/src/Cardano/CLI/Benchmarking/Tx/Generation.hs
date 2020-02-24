{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}

module Cardano.CLI.Benchmarking.Tx.Generation
  ( NumberOfTxs(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , FeePerTx(..)
  , TPSRate(..)
  , TxAdditionalSize(..)
  , ExplorerAPIEnpoint(..)
  , TxGenError
  , genesisBenchmarkRunner
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Monad (forM, forM_, mapM, when)
import qualified Control.Monad.Class.MonadSTM as MSTM
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left, right)
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as LB
import           Data.Either (isLeft)
import           Data.Foldable (find, foldl', foldr, toList)
import qualified Data.IP as IP
import           Data.List ((!!), last)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (Maybe (..), fromMaybe, listToMaybe, mapMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import           Data.Word (Word8, Word32, Word64)
import           Network.HTTP.Client (Request (..), RequestBody (..),
                     defaultManagerSettings, httpLbs,
                     newManager, parseRequest, responseBody,
                     responseStatus)
import           Network.HTTP.Types.Status (statusCode)
import           Network.Socket (AddrInfo (..),
                     AddrInfoFlag (..), Family (..), SocketType (Stream),
                     addrFamily,addrFlags, addrSocketType, defaultHints,
                     getAddrInfo)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (appendName)
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Config.Logging (LoggingLayer (..), Trace)
import           Cardano.Config.Types (NodeAddress (..), NodeHostAddress(..),
                                       SocketPath)
import qualified Cardano.Crypto as Crypto
import           Cardano.CLI.Ops
import           Cardano.CLI.Tx (toCborTxAux, txSpendGenesisUTxOByronPBFT,
                     normalByronTxToGenTx)
import           Cardano.CLI.Benchmarking.Tx.TxSubmission (ROEnv (..),
                     TraceBenchTxSubmit (..),
                     bulkSubmission)
import           Cardano.Node.Submission (TraceLowLevelSubmit, submitTx)
import           Cardano.CLI.Benchmarking.Tx.NodeToNode (BenchmarkTxSubmitTracers (..),
                     SendRecvConnect,
                     SendRecvTxSubmission,
                     benchmarkConnectTxSubmit)
import           Cardano.Node.TxSubClient
import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.NodeToClient (AssociateWithIOCP)

import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Block(BlockProtocol)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..),
                                                   GenTx (..),
                                                   ByronConsensusProtocol,
                                                   getGenesisConfig)
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)

newtype NumberOfTxs =
  NumberOfTxs Word64
  deriving (Eq, Ord, Show)

newtype NumberOfInputsPerTx =
  NumberOfInputsPerTx Int
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

-- | This parameter specifies additional size (in bytes) of transaction.
--   Since 1 transaction is ([input] + [output] + attributes), its size
--   is defined by its inputs and outputs. We want to have an ability to
--   increase transaction's size without increasing the number of inputs/
--   outputs. Such a big transaction will give us more real-world results
--   of benchmarking.
--   Technically this parameter specifies the size of attribute we'll
--   add to transaction (by default attributes are empty, so if this
--   parameter is skipped, attributes will remain empty).
newtype TxAdditionalSize =
  TxAdditionalSize Int
  deriving (Eq, Ord, Show)

-- | This parameter specifies Explorer's API endpoint we use to submit
--   transaction. This parameter is an optional one, and if it's defined -
--   generator won't submit transactions to 'ouroboros-network', instead it
--   will submit transactions to that endpoint, using POST-request.
newtype ExplorerAPIEnpoint =
  ExplorerAPIEnpoint String
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------------------
-- | Genesis benchmark runner (we call it in 'Run.runNode').
--
--   Using a _richman_ (from genesis block) to supply some initial
--   amount of funds for disbursment.
-----------------------------------------------------------------------------------------
genesisBenchmarkRunner
  :: LoggingLayer
  -> AssociateWithIOCP
  -> SocketPath
  -> Consensus.Protocol ByronBlock
  -> NonEmpty NodeAddress
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> FeePerTx
  -> TPSRate
  -> Maybe TxAdditionalSize
  -> Maybe ExplorerAPIEnpoint
  -> [FilePath]
  -> ExceptT TxGenError IO ()
genesisBenchmarkRunner loggingLayer
                       iocp
                       socketFp
                       protocol@(Consensus.ProtocolRealPBFT genesisConfig _ _ _ _)
                       targetNodeAddresses
                       numOfTxs@(NumberOfTxs rawNumOfTxs)
                       numOfInsPerTx
                       numOfOutsPerTx
                       txFee
                       tpsRate
                       txAdditionalSize
                       explorerAPIEndpoint
                       signingKeyFiles = do
  when (length signingKeyFiles < 3) $
    left $ NeedMinimumThreeSigningKeyFiles signingKeyFiles

  let (benchTracer, connectTracer, submitTracer, lowLevelSubmitTracer) = createTracers loggingLayer

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, tracers are ready *******"

  -- 'genesisKey' is for genesis address with initial amount of money (1.4 billion ADA for now).
  -- 'sourceKey' is for source address that we'll use as a source of money for next transactions.
  -- 'recepientKey' is for recipient address that we'll use as an output for next transactions.
  (genesisKey:sourceKey:recepientKey:_) <- prepareSigningKeys signingKeyFiles

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, signing keys are ready *******"

  let genesisUtxo = extractGenesisFunds genesisConfig [genesisKey]

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, genesis UTxO is ready *******"

  let ProtocolInfo{pInfoConfig} = Consensus.protocolInfo protocol
      genesisAddress   = mkAddressForKey pInfoConfig genesisKey
      sourceAddress    = mkAddressForKey pInfoConfig sourceKey
      recipientAddress = mkAddressForKey pInfoConfig recepientKey

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, addresses are ready *******"

  -- We have to prepare an initial funds (it's the money we'll send from 'genesisAddress' to
  -- 'sourceAddress'), this will be our very first transaction.
  fundsWithGenesisMoney <- liftIO $
    prepareInitialFunds benchTracer
                        lowLevelSubmitTracer
                        iocp
                        socketFp
                        genesisConfig
                        pInfoConfig
                        genesisUtxo
                        genesisAddress
                        sourceAddress
                        txFee
                        explorerAPIEndpoint

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, initial funds are prepared (sent to sourceAddress) *******"

  -- Check if no transactions needed...
  when (rawNumOfTxs > 0) $
    runBenchmark benchTracer
                 connectTracer
                 submitTracer
                 lowLevelSubmitTracer
                 iocp
                 socketFp
                 pInfoConfig
                 sourceKey
                 recipientAddress
                 targetNodeAddresses
                 numOfTxs
                 numOfInsPerTx
                 numOfOutsPerTx
                 txFee
                 tpsRate
                 txAdditionalSize
                 explorerAPIEndpoint
                 fundsWithGenesisMoney

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

-----------------------------------------------------------------------------------------
-- Tracers.
-----------------------------------------------------------------------------------------

createTracers
  :: LoggingLayer
  -> ( Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))
     , Tracer IO SendRecvConnect
     , Tracer IO (SendRecvTxSubmission ByronBlock)
     , Tracer IO TraceLowLevelSubmit
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

  benchTracer :: Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))
  benchTracer = toLogObjectVerbose (appendName "benchmark" tr')

  connectTracer :: Tracer IO SendRecvConnect
  connectTracer = toLogObjectVerbose (appendName "connect" tr')

  submitTracer :: Tracer IO (SendRecvTxSubmission ByronBlock)
  submitTracer = toLogObjectVerbose (appendName "submit" tr')

  lowLevelSubmitTracer :: Tracer IO TraceLowLevelSubmit
  lowLevelSubmitTracer = toLogObjectVerbose (appendName "llSubmit" tr')

-----------------------------------------------------------------------------------------
-- | Prepare signing keys and addresses for transactions.
-----------------------------------------------------------------------------------------

-- | We take files with secret keys and create signing keys from them,
--   we need it to be able to spend money from corresponding addresses.
prepareSigningKeys :: [FilePath] -> ExceptT TxGenError IO [Crypto.SigningKey]
prepareSigningKeys skeys = do
  bsList <- liftIO $ mapM readSecretKey skeys
  when (any isLeft bsList) $
    left . SecretKeyReadError . show $ filter isLeft bsList

  let desKeys = map (deserialiseFromBytes Crypto.fromCBORXPrv) $ rights bsList

  when (any isLeft desKeys) $
    left . SecretKeyDeserialiseError . show . (fmap . fmap) fst $ filter isLeft desKeys

  pure . map (Crypto.SigningKey . snd) $ rights desKeys

mkAddressForKey
  :: NodeConfig ByronConsensusProtocol
  -> Crypto.SigningKey
  -> CC.Common.Address
mkAddressForKey _pInfoConfig =
  CC.Common.makeVerKeyAddress networkMagic . Crypto.toVerification
 where
  -- The value is taken from a result of
  -- script 'issue-genesis-utxo-expenditure.sh'.
  networkMagic = CC.Common.NetworkTestnet 459045235

readSecretKey :: FilePath -> IO (Either String LB.ByteString)
readSecretKey skFp = do
  eBs <- Exception.try $ LB.readFile skFp
  case eBs of
    Left e -> pure . Left $ handler e
    Right lbs -> pure $ Right lbs
 where
  handler :: IOException -> String
  handler e = "Cardano.CLI.Benchmarking.Tx.Generation.readSecretKey: "
              ++ displayException e
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
  :: Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))
  -> Tracer IO TraceLowLevelSubmit
  -> AssociateWithIOCP 
  -> SocketPath
  -> CC.Genesis.Config
  -> NodeConfig ByronConsensusProtocol
  -> Map Int ((CC.UTxO.TxIn, CC.UTxO.TxOut), Crypto.SigningKey)
  -> CC.Common.Address
  -> CC.Common.Address
  -> FeePerTx
  -> Maybe ExplorerAPIEnpoint
  -> IO AvailableFunds
prepareInitialFunds benchTracer
                    llTracer
                    iocp
                    socketFp
                    genesisConfig
                    pInfoConfig
                    genesisUtxo
                    genesisAddress
                    targetAddress
                    (FeePerTx txFee)
                    explorerAPIEndpoint = do
  let ((_, out), signingKey) = genesisUtxo Map.! 0 -- Currently there's only 1 element.
      feePerTx = assumeBound . CC.Common.mkLovelace $ txFee
      outBig = CC.UTxO.txOutValue out `subLoveLace` feePerTx
      outForBig = CC.UTxO.TxOut
        { CC.UTxO.txOutAddress = targetAddress
        , CC.UTxO.txOutValue   = outBig
        }

  let genesisTx :: CC.UTxO.ATxAux ByteString
      genesisTx = txSpendGenesisUTxOByronPBFT genesisConfig
                                              signingKey
                                              genesisAddress
                                              (NE.fromList [outForBig])
      genesisTxGeneral :: GenTx ByronBlock
      genesisTxGeneral = normalByronTxToGenTx genesisTx

  case explorerAPIEndpoint of
    Nothing -> do
      -- There's no Explorer's API endpoint specified, submit genesis
      -- transaction to the target nodes via 'ouroboros-network'.
      submitTx iocp socketFp pInfoConfig genesisTxGeneral llTracer
    Just (ExplorerAPIEnpoint endpoint) -> do
      -- Explorer's API endpoint is specified, submit genesis
      -- transaction to that endpoint using POST-request.
      initialRequest <- liftIO $ parseRequest endpoint
      postTx benchTracer initialRequest $ toCborTxAux genesisTx

  -- Done, the first transaction 'initGenTx' is submitted, now 'sourceAddress' has a lot of money.

  let txIn  = CC.UTxO.TxInUtxo (getTxIdFromGenTx genesisTxGeneral) 0
      txOut = outForBig

  -- Form availableFunds with a single value, it will be used for further (splitting) transactions.
  let genesisTxValueStatus = FundValueStatus
                               { status    = Unknown
                               , txDetails = (txIn, txOut)
                               }
  return $ Set.singleton genesisTxValueStatus

-- | Get 'TxId' from 'GenTx'. Since we generate transactions by ourselves -
--   we definitely know that it's 'ByronTx' only.
getTxIdFromGenTx
  :: GenTx ByronBlock
  -> CC.UTxO.TxId
getTxIdFromGenTx (ByronTx txId _) = txId
getTxIdFromGenTx _ = panic "Impossible happened: generated transaction is not a ByronTx!"

-- | One or more inputs -> one or more outputs.
mkTransaction
  :: (FiscalRecipient r)
  => NodeConfig ByronConsensusProtocol
  -> NonEmpty (TxDetails, Crypto.SigningKey)
  -- ^ Non-empty list of (TxIn, TxOut) that will be used as
  -- inputs and the key to spend the associated value
  -> Maybe CC.Common.Address
  -- ^ The address to associate with the 'change',
  -- if different from that of the first argument
  -> Set (r, CC.UTxO.TxOut)
  -- ^ Each recipient and their payment details
  -> Maybe TxAdditionalSize
  -- ^ Optional size of additional binary blob in transaction (as 'txAttributes')
  -> Word64
  -- ^ Tx fee.
  -> ( Maybe (Word32, CC.Common.Lovelace) -- The 'change' index and value (if any)
     , CC.Common.Lovelace                 -- The associated fees
     , Map r Word32                       -- The offset map in the transaction below
     , CC.UTxO.ATxAux ByteString
     )
mkTransaction cfg inputs mChangeAddress payments txAdditionalSize txFee =
  (mChange, fees, offsetMap, txAux)
 where
  -- Each input contains the same 'signingKey' and the same 'txOutAddress',
  -- so pick the first one.
  ((_, firstTxOutFrom), signingKey) = NE.head inputs
  -- Take all txoutFrom's.
  allTxOutFrom  = NE.map (snd . fst) inputs

  paymentsList  = toList payments
  txOuts        = map snd paymentsList

  totalInpValue = foldl' (\s txoutFrom -> s `addLovelace` CC.UTxO.txOutValue txoutFrom)
                         (assumeBound $ CC.Common.mkLovelace 0)
                         allTxOutFrom

  totalOutValue = foldl' (\s txout -> s `addLovelace` CC.UTxO.txOutValue txout)
                         (assumeBound $ CC.Common.mkLovelace 0)
                         txOuts
  fees          = assumeBound $ CC.Common.mkLovelace txFee
  changeValue   = totalInpValue `subLoveLace` (totalOutValue `addLovelace` fees)

      -- change the order of comparisons first check emptiness of txouts AND remove appendr after

  (txOutputs, mChange) =
    if (CC.Common.unsafeGetLovelace changeValue) > 0
    then
      let changeAddress = fromMaybe (CC.UTxO.txOutAddress firstTxOutFrom) mChangeAddress
          changeTxOut   = CC.UTxO.TxOut {
                                      CC.UTxO.txOutAddress = changeAddress
                                    , CC.UTxO.txOutValue   = changeValue
                                    }
          changeIndex   = fromIntegral $ length txOuts -- 0-based index
      in
          (appendr txOuts (changeTxOut :| []), Just (changeIndex, changeValue))
    else
      case txOuts of
        []                 -> panic "change is zero and txouts is empty"
        txout0: txoutsRest -> (txout0 :| txoutsRest, Nothing)

  -- TxOuts of recipients are placed at the first positions
  offsetMap = Map.fromList $ zipWith (\payment index -> (fst payment, index))
                                     paymentsList
                                     [0..]

  -- Take all actual inputs.
  pureInputs = NE.map (\((txIn, _), _) -> txIn) inputs

  tx :: CC.UTxO.Tx
  tx = CC.UTxO.UnsafeTx
         { CC.UTxO.txInputs     = pureInputs
         , CC.UTxO.txOutputs    = txOutputs
         , CC.UTxO.txAttributes = createTxAttributes txAdditionalSize
         }

  txAux = createTxAux cfg tx signingKey

-- | If this transaction should contain additional binary blob -
--   we have to create attributes of the corresponding size.
--   TxAttributes contains a map from 1-byte integer to arbitrary bytes which
--   will be used as a binary blob to increase the size of the transaction.
createTxAttributes
  :: Maybe TxAdditionalSize
  -> CC.UTxO.TxAttributes
createTxAttributes txAdditionalSize =
  case txAdditionalSize of
    Nothing -> emptyAttributes
    Just (TxAdditionalSize size) -> blobAttributes size
 where
  emptyAttributes :: CC.UTxO.TxAttributes
  emptyAttributes = CC.Common.mkAttributes ()

  blobAttributes :: Int -> CC.UTxO.TxAttributes
  blobAttributes aSize =
    emptyAttributes {
      CC.Common.attrRemain = CC.Common.UnparsedFields $
        Map.singleton k $ LB.replicate (finalSize aSize) byte
    }

  k :: Word8
  k = 1 -- Arbitrary key.

  -- Fill an attribute by the same arbitrary byte in each element.
  byte :: Word8
  byte = 0

  sizeOfKey :: Int
  sizeOfKey = 1

  -- Please note that actual binary size of attributes will be a little bit
  -- bigger than the size defined by user (via CLI argument), because size of
  -- singleton 'Map k v' isn't equal to the size of ('k' + 'v').
  finalSize :: Int -> Int64
  finalSize userDefinedSize = fromIntegral (userDefinedSize - sizeOfKey)

-- | Append a non-empty list to a list.
-- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
appendr :: [a] -> NonEmpty a -> NonEmpty a
appendr l nel = foldr NE.cons nel l

-- | Annotate and sign transaction before submitting.
-- generalizeTx
--   :: NodeConfig ByronConsensusProtocol
--   -> CC.UTxO.Tx
--   -> Crypto.SigningKey -- signingKey for spending the input
--   -> GenTx ByronBlock
-- generalizeTx config tx signingKey =
--   Byron.fromMempoolPayload $ CC.Mempool.MempoolTx $ createTxAux config tx signingKey

-- | ...
createTxAux
  :: NodeConfig ByronConsensusProtocol
  -> CC.UTxO.Tx
  -> Crypto.SigningKey
  -> CC.UTxO.ATxAux ByteString
createTxAux config tx signingKey = CC.UTxO.annotateTxAux $ CC.UTxO.mkTxAux tx witness
 where
  witness = pure $
      CC.UTxO.VKWitness
        (Crypto.toVerification signingKey)
        (Crypto.sign
          (CC.Genesis.configProtocolMagicId (getGenesisConfig config))
          -- provide ProtocolMagicId so as not to calculate it every time
          Crypto.SignTx
          signingKey
          (CC.UTxO.TxSigData (Crypto.hash tx))
        )

-----------------------------------------------------------------------------------------
-- Helpers for work with lovelaces.
-----------------------------------------------------------------------------------------

assumeBound :: Either CC.Common.LovelaceError CC.Common.Lovelace
            -> CC.Common.Lovelace
assumeBound (Left err) = panic $ T.pack ("TxGeneration: " ++ show err)
assumeBound (Right ll) = ll

subLoveLace :: CC.Common.Lovelace -> CC.Common.Lovelace -> CC.Common.Lovelace
subLoveLace a b = assumeBound $ CC.Common.subLovelace a b

addLovelace :: CC.Common.Lovelace -> CC.Common.Lovelace -> CC.Common.Lovelace
addLovelace a b = assumeBound $ CC.Common.addLovelace a b

-----------------------------------------------------------------------------------------
-- | Run benchmark using top level tracers..
-----------------------------------------------------------------------------------------

-- | Please note that there's a difference between Cardano tx and fiscal tx:
--   1. Cardano tx is a transaction from Cardano blockchain's point of view.
--   2. Fiscal tx is a transaction from recipient's point of view.
--   So if one Cardano tx contains 10 outputs (with addresses of 10 recipients),
--   we have 1 Cardano tx and 10 fiscal txs.
runBenchmark
  :: Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))
  -> Tracer IO SendRecvConnect
  -> Tracer IO (SendRecvTxSubmission ByronBlock)
  -> Tracer IO TraceLowLevelSubmit
  -> AssociateWithIOCP
  -> SocketPath
  -> NodeConfig ByronConsensusProtocol
  -> Crypto.SigningKey
  -> CC.Common.Address
  -> NonEmpty NodeAddress
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> FeePerTx
  -> TPSRate
  -> Maybe TxAdditionalSize
  -> Maybe ExplorerAPIEnpoint
  -> AvailableFunds
  -> ExceptT TxGenError IO ()
runBenchmark benchTracer
             connectTracer
             submitTracer
             lowLevelSubmitTracer
             iocp
             socketFp
             pInfoConfig
             sourceKey
             recipientAddress
             targetNodeAddresses
             numOfTxs
             numOfInsPerTx
             numOfOutsPerTx
             txFee
             tpsRate
             txAdditionalSize
             explorerAPIEndpoint
             fundsWithGenesisMoney = do
  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 1: make enough available UTxO entries *******"
  fundsWithSufficientCoins <-
      createMoreFundCoins benchTracer
                          lowLevelSubmitTracer
                          iocp
                          socketFp
                          pInfoConfig
                          sourceKey
                          txFee
                          numOfTxs
                          numOfInsPerTx
                          fundsWithGenesisMoney
                          explorerAPIEndpoint

  -- sleep for 20 s; subsequent txs enter new block
  liftIO $ threadDelay (20*1000*1000)

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 2: pay to recipients *******"
  let benchmarkTracers :: BenchmarkTxSubmitTracers IO ByronBlock
      benchmarkTracers = BenchmarkTracers
                           { trSendRecvConnect      = connectTracer
                           , trSendRecvTxSubmission = submitTracer
                           }

  let localAddr :: Maybe Network.Socket.AddrInfo
      localAddr = Nothing

  remoteAddresses <- forM targetNodeAddresses $ \targetNodeAddress -> do
    let (anAddrFamily, targetNodeHost) =
          case getAddress $ naHostAddress targetNodeAddress of
            Just (IP.IPv4 ipv4) -> (AF_INET,  show ipv4)
            Just (IP.IPv6 ipv6) -> (AF_INET6, show ipv6)
            Nothing -> panic "Target node's IP-address is undefined!"

    let targetNodePort = show $ naPort targetNodeAddress

    let hints :: AddrInfo
        hints = defaultHints
          { addrFlags      = [AI_PASSIVE]
          , addrFamily     = anAddrFamily
          , addrSocketType = Stream
          , addrCanonName  = Nothing
          }

    (remoteAddr:_) <- liftIO $ getAddrInfo (Just hints) (Just targetNodeHost) (Just targetNodePort)
    return remoteAddr

  let updROEnv
        :: ROEnv (Mempool.GenTxId ByronBlock) (GenTx ByronBlock)
        -> ROEnv (Mempool.GenTxId ByronBlock) (GenTx ByronBlock)
      updROEnv defaultROEnv =
        ROEnv { targetBacklog     = targetBacklog defaultROEnv
              , txNumServiceTime  = Just $ minimalTPSRate tpsRate
              , txSizeServiceTime = Nothing
              }

  -- List of 'TMVar's with lists of transactions for submitting.
  -- The number of these lists corresponds to the number of target nodes.
  txsListsForTargetNodes :: MSTM.TMVar IO [MSTM.TMVar IO [CC.UTxO.ATxAux ByteString]]
    <- liftIO $ STM.newTMVarIO []

  -- Run generator.
  txGenerator benchTracer
              pInfoConfig
              recipientAddress
              sourceKey
              txFee
              (NE.length targetNodeAddresses)
              numOfTxs
              numOfInsPerTx
              numOfOutsPerTx
              txAdditionalSize
              fundsWithSufficientCoins
              txsListsForTargetNodes

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, launch submission threads... *******"

  -- TVar for termination.
  txSubmissionTerm :: MSTM.TVar IO Bool <- liftIO $ STM.newTVarIO False

  txsLists :: [MSTM.TMVar IO [CC.UTxO.ATxAux ByteString]]
    <- liftIO $ STM.atomically $ STM.takeTMVar txsListsForTargetNodes

  case explorerAPIEndpoint of
    Nothing ->
      -- There's no Explorer's API endpoint specified, submit transactions
      -- to the target nodes via 'ouroboros-network'.
      liftIO $ do
        let targetNodesAddrsAndTxsLists = zip (NE.toList remoteAddresses) txsLists
        allAsyncs <- forM targetNodesAddrsAndTxsLists $ \(remoteAddr, txsList) -> do
          -- Launch connection and submission threads for a peer
          -- (corresponding to one target node).
          txAuxes :: [CC.UTxO.ATxAux ByteString] <- STM.atomically $ STM.takeTMVar txsList
          let generalTxs :: [GenTx ByronBlock]
              generalTxs = map normalByronTxToGenTx txAuxes

          txsListGeneral :: MSTM.TMVar IO [GenTx ByronBlock] <- liftIO $ STM.newTMVarIO generalTxs

          launchTxPeer benchTracer
                       benchmarkTracers
                       iocp
                       txSubmissionTerm
                       pInfoConfig
                       localAddr
                       remoteAddr
                       updROEnv
                       txsListGeneral
        let allAsyncs' = intercalate [] [[c, s] | (c, s) <- allAsyncs]
        -- Just wait for all threads to complete.
        mapM_ (void . wait) allAsyncs'
    Just (ExplorerAPIEnpoint endpoint) ->
      -- Explorer's API endpoint is specified, submit transactions
      -- to that endpoint using POST-request.
      liftIO $ do
        initialRequest <- parseRequest endpoint
        txsList <- concat <$> mapM (STM.atomically . STM.takeTMVar) txsLists
        submitTxsToExplorer benchTracer initialRequest txsList tpsRate

submitTxsToExplorer
  :: Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))
  -> Request
  -> [CC.UTxO.ATxAux ByteString]
  -> TPSRate
  -> IO ()
submitTxsToExplorer benchTracer initialRequest allTxs (TPSRate rate) =
  forM_ allTxs $ \txAux -> do
    postTx benchTracer initialRequest $ toCborTxAux txAux
    threadDelay delayBetweenSubmits
 where
  delayBetweenSubmits = oneSecond `div` rate
  oneSecond = 1000000 :: Int

postTx
  :: Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))
  -> Request
  -> LB.ByteString
  -> IO ()
postTx benchTracer initialRequest serializedTx = do
  manager <- newManager defaultManagerSettings
  let request = initialRequest
                  { method = "POST"
                  , requestHeaders = [("Content-Type", "application/cbor")]
                  , requestBody = RequestBodyLBS serializedTx
                  }
  responseFromExplorer <- httpLbs request manager
  traceWith benchTracer . TraceBenchTxSubDebug
    $ "Response from Explorer WebAPI: status code: "
      <> (show $ statusCode $ responseStatus responseFromExplorer)
      <> ", body: "
      <> (show $ responseBody responseFromExplorer)

-- | At this moment 'sourceAddress' contains a huge amount of money (lets call it A).
--   Now we have to split this amount to N equal parts, as a result we'll have
--   N UTxO entries, and alltogether these entries will contain the same amount A.
--   E.g. (1 entry * 1000 ADA) -> (10 entries * 100 ADA).
--   Technically all splitting transactions will send money back to 'sourceAddress'.
createMoreFundCoins
  :: Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))
  -> Tracer IO TraceLowLevelSubmit
  -> AssociateWithIOCP
  -> SocketPath
  -> NodeConfig ByronConsensusProtocol
  -> Crypto.SigningKey
  -> FeePerTx
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> AvailableFunds
  -> Maybe ExplorerAPIEnpoint
  -> ExceptT TxGenError IO AvailableFunds
createMoreFundCoins benchTracer
                    llTracer
                    iocp
                    socketFp
                    pInfoConfig
                    sourceKey
                    (FeePerTx txFee)
                    (NumberOfTxs numOfTxs)
                    (NumberOfInputsPerTx numOfInsPerTx)
                    fundsWithGenesisMoney
                    explorerAPIEndpoint = do
  let feePerTx              = assumeBound . CC.Common.mkLovelace $ txFee
      -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      numSplittingTxOuts    = numOfTxs * (fromIntegral numOfInsPerTx)
      numOutsPerSplittingTx = 60 :: Word64 -- near the upper bound so as not to exceed the tx size limit

  -- Now we have to find the first output with sufficient amount of money.
  -- But since we made only one single transaction, there is only one
  -- 'FundValueStatus' in 'availableFunds', and it definitely contains a
  -- huge amount of money.
  let genesisTxDetails = txDetails $ (Set.toList fundsWithGenesisMoney) !! 0
      sourceAddress = CC.UTxO.txOutAddress . snd $ genesisTxDetails
      sourceValue   = CC.UTxO.txOutValue   . snd $ genesisTxDetails
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
                                         (genesisTxDetails, sourceKey)
                                         numSplittingTxOuts
                                         numOutsPerSplittingTx
                                         42
                                         txOut
                                         []
  -- Submit all splitting transactions sequentially.
  case explorerAPIEndpoint of
    Nothing ->
      -- There's no Explorer's API endpoint specified, submit splitting
      -- transactions to the target nodes via 'ouroboros-network'.
      liftIO $ forM_ splittingTxs $ \(txAux, _) -> do
        let splittingTxGeneral :: GenTx ByronBlock
            splittingTxGeneral = normalByronTxToGenTx txAux
        submitTx iocp socketFp pInfoConfig splittingTxGeneral llTracer
    Just (ExplorerAPIEnpoint endpoint) -> do
      -- Explorer's API endpoint is specified, submit splitting
      -- transactions to that endpoint using POST-request.
      initialRequest <- liftIO $ parseRequest endpoint
      liftIO $ forM_ splittingTxs $ \(txAux, _) ->
        postTx benchTracer initialRequest $ toCborTxAux txAux

  -- Re-create availableFunds with information about all splitting transactions
  -- (it will be used for main transactions).
  right $ reCreateAvailableFunds splittingTxs
 where
  -- create txs which split the funds to numTxOuts equal parts
  createSplittingTxs
    :: NodeConfig ByronConsensusProtocol
    -> (TxDetails, Crypto.SigningKey)
    -> Word64
    -> Word64
    -> Int
    -> CC.UTxO.TxOut
    -> [(CC.UTxO.ATxAux ByteString, [TxDetails])]
    -> [(CC.UTxO.ATxAux ByteString, [TxDetails])]
  createSplittingTxs config details numTxOuts maxOutsPerInitTx identityIndex txOut acc
    | numTxOuts <= 0 = reverse acc
    | otherwise =
        let numOutsPerInitTx = min maxOutsPerInitTx numTxOuts
            sourceAddress = CC.UTxO.txOutAddress . snd . fst $ details
            -- same TxOut for all
            outs = Set.fromList $ zip [identityIndex .. identityIndex + (fromIntegral numOutsPerInitTx) - 1]
                                      (repeat txOut)
            (mFunds, _fees, outIndices, txAux) = mkTransaction config
                                                               (details :| [])
                                                               Nothing
                                                               outs
                                                               Nothing
                                                               txFee
            genTx = normalByronTxToGenTx txAux
            !txId = getTxIdFromGenTx genTx
            txDetailsList = (flip map) (Map.toList outIndices) $
                \(_, txInIndex) ->
                  let !txIn  = CC.UTxO.TxInUtxo txId txInIndex
                  in (txIn, txOut)
        in
          case mFunds of
            Nothing                 -> reverse $ (txAux, txDetailsList) : acc
            Just (txInIndex, value) ->
              let !txInChange  = CC.UTxO.TxInUtxo (getTxIdFromGenTx genTx) txInIndex
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
                                   ((txAux, txDetailsList) : acc)
  reCreateAvailableFunds
    :: [(CC.UTxO.ATxAux ByteString, [TxDetails])]
    -> AvailableFunds
  reCreateAvailableFunds splittingTxs =
    Set.fromList $
      [ FundValueStatus { status    = Unknown
                        , txDetails = txDetails
                        }
      | txDetails <- allTxDetails
      ]
   where
    allTxDetails = concat $ map snd splittingTxs

-----------------------------------------------------------------------------------------
-- | Work with tx generator thread (for Phase 2).
-----------------------------------------------------------------------------------------

-- | It represents the earliest time at which another tx will be sent.
minimalTPSRate :: TPSRate -> DiffTime
minimalTPSRate (TPSRate tps) = picosecondsToDiffTime timeInPicoSecs
 where
  timeInPicoSecs = picosecondsIn1Sec `div` fromIntegral tps
  picosecondsIn1Sec = 1000000000000 :: Integer

txGenerator
  :: Tracer IO (TraceBenchTxSubmit (Mempool.GenTxId ByronBlock))
  -> NodeConfig ByronConsensusProtocol
  -> CC.Common.Address
  -> Crypto.SigningKey
  -> FeePerTx
  -> Int
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> Maybe TxAdditionalSize
  -> AvailableFunds
  -> MSTM.TMVar IO [MSTM.TMVar IO [CC.UTxO.ATxAux ByteString]]
  -> ExceptT TxGenError IO ()
txGenerator benchTracer
            cfg
            recipientAddress
            sourceKey
            (FeePerTx txFee)
            numOfTargetNodes
            (NumberOfTxs numOfTransactions)
            (NumberOfInputsPerTx numOfInsPerTx)
            (NumberOfOutputsPerTx numOfOutsPerTx)
            txAdditionalSize
            fundsWithSufficientCoins
            txsListsForTargetNodes = do
  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ " Prepare to generate, total number of transactions " ++ show numOfTransactions

  -- Generator is producing transactions and writes them in the list.
  -- Later sumbitter is reading and submitting these transactions.
  txsForSubmission :: MSTM.TMVar IO [CC.UTxO.ATxAux ByteString] <- liftIO $ STM.newTMVarIO []

  -- Prepare a number of lists for transactions, for all target nodes.
  -- Later we'll write generated transactions in these lists,
  -- and they will be received and submitted by 'bulkSubmission' function.
  forM_ [1 .. numOfTargetNodes] $ \_ ->
    liftIO $ addTxsListForTargetNode txsListsForTargetNodes txsForSubmission

  txs <- createMainTxs numOfTransactions numOfInsPerTx fundsWithSufficientCoins

  -- Now we have to split all transactions to parts for every target node.
  -- These parts will be written to corresponding lists and submitted to the node.
  let numOfTxsForOneTargetNode = (fromIntegral numOfTransactions) `div` numOfTargetNodes
      subLists = divListToSublists txs numOfTxsForOneTargetNode
      subListsAndIndices = zip subLists [0 .. numOfTargetNodes - 1]
  forM_ subListsAndIndices $ \(txsForOneTargetNode, nodeIx) ->
    -- Write newly generated txs in the list for particular node.
    -- These txs will be handled by 'bulkSubmission' function and will be sent to
    -- corresponding target node.
    liftIO $ writeTxsInListForTargetNode txsListsForTargetNodes txsForOneTargetNode nodeIx

  -- It's possible that we have a remaining transactions, in the latest sublist
  -- (if 'numOfTransactions' cannot be divided by 'numOfTargetNodes'). In this case
  -- just send these transactions to the first node. TODO: should we change this behaviour?
  when (length subLists > numOfTargetNodes) $ do
    let stillUnsentTxs = last subLists
    liftIO $ writeTxsInListForTargetNode txsListsForTargetNodes stillUnsentTxs 0

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ " Done, " ++ show numOfTransactions ++ " were generated and written in a list."
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

  -- Create all main transactions, using available funds.
  createMainTxs
    :: Word64
    -> Int
    -> AvailableFunds
    -> ExceptT TxGenError IO [CC.UTxO.ATxAux ByteString]
  createMainTxs 0 _ _ = right []
  createMainTxs txsNum insNumPerTx funds = do
    (txInputs, updatedFunds) <- getTxInputs insNumPerTx funds
    let (_, _, _, txAux :: CC.UTxO.ATxAux ByteString) =
          mkTransaction cfg
                        (NE.fromList txInputs)
                        (Just addressForChange)
                        recipients
                        txAdditionalSize
                        txFee
    (txAux :) <$> createMainTxs (txsNum - 1) insNumPerTx updatedFunds

  -- Get inputs for one main transaction, using available funds.
  getTxInputs
    :: Int
    -> AvailableFunds
    -> ExceptT TxGenError IO ( [(TxDetails, Crypto.SigningKey)]
                             , AvailableFunds
                             )
  getTxInputs 0 funds = right ([], funds)
  getTxInputs insNumPerTx funds = do
    (fvs, updatedFunds) <- findAvailableFunds funds totalValue (const True)
    (inputs, updatedFunds') <- getTxInputs (insNumPerTx - 1) updatedFunds
    right ((txDetails fvs, sourceKey) : inputs, updatedFunds')

  -- Find a source of available funds, removing it from the availableFunds
  -- for preventing of double spending.
  findAvailableFunds
    :: AvailableFunds          -- funds we are trying to find in
    -> CC.Common.Lovelace      -- with at least this associated value
    -> (SettledStatus -> Bool) -- predicate for selecting on status
    -> ExceptT TxGenError IO (FundValueStatus, AvailableFunds)
  findAvailableFunds funds valueThreshold predStatus =
    case find predFVS (Set.toList funds) of
      Nothing    -> left InsufficientFundsForRecipientTx
      Just found -> right (found, Set.delete found funds)
   where
    -- Find the first tx output that contains sufficient amount of money.
    predFVS :: FundValueStatus -> Bool
    predFVS FundValueStatus{status, txDetails} =
         predStatus status
      && (CC.UTxO.txOutValue . snd $ txDetails) >= valueThreshold

-- | Takes a list 'L' and a number 'D'. Returns a list of sublists which contains
--   parts of 'L' with length that is equal to 'D'. Remaining part (if exists) will
--   be stored as last sublist. For example:
--   >>> divListToSublists [0 .. 7] 3 = [[0 .. 2], [3 .. 5], [6 .. 7]]
--   >>> divListToSublists [0 .. 8] 3 = [[0 .. 2], [3 .. 5], [6 .. 8]]
--   >>> divListToSublists [0 .. 9] 3 = [[0 .. 2], [3 .. 5], [6 .. 8], [9]]
divListToSublists :: [a] -> Int -> [[a]]
divListToSublists [] _ = []
divListToSublists l  d =
  let (subl, remain) = splitAt d l
  in subl : divListToSublists remain d

---------------------------------------------------------------------------------------------------
-- Txs for submission.
---------------------------------------------------------------------------------------------------


-- | Adds a list for transactions, for particular target node.
addTxsListForTargetNode
  :: MSTM.TMVar IO [MSTM.TMVar IO [CC.UTxO.ATxAux ByteString]]
  -> MSTM.TMVar IO [CC.UTxO.ATxAux ByteString]
  -> IO ()
addTxsListForTargetNode txsListsForTargetNodes listForOneTargetNode = STM.atomically $
  STM.tryTakeTMVar txsListsForTargetNodes >>=
    \case
      Nothing      -> STM.putTMVar txsListsForTargetNodes [listForOneTargetNode]
      Just curList -> STM.putTMVar txsListsForTargetNodes $ curList ++ [listForOneTargetNode]

-- | Writes list of generated transactions to the list, for particular target node.
--   For example, if we have 3 target nodes and write txs to the list 0,
--   these txs will later be sent to the first target node.
writeTxsInListForTargetNode
  :: MSTM.TMVar IO [MSTM.TMVar IO [CC.UTxO.ATxAux ByteString]]
  -> [CC.UTxO.ATxAux ByteString]
  -> Int
  -> IO ()
writeTxsInListForTargetNode txsListsForTargetNodes txs listIndex = STM.atomically $ do
  txsLists <- STM.takeTMVar txsListsForTargetNodes
  -- We shouldn't check 'listIndex' - we control both list and index, it cannot be out-of-range.
  let txsListForTargetNode = txsLists !! listIndex
  STM.tryTakeTMVar txsListForTargetNode >>=
    \case
      Nothing      -> STM.putTMVar txsListForTargetNode txs
      Just txsList -> STM.putTMVar txsListForTargetNode $ txsList ++ txs
  -- Put updated content back.
  STM.putTMVar txsListsForTargetNodes txsLists

-- | To get higher performance we need to hide latency of getting and
-- forwarding (in sufficient numbers) transactions.
--
-- TODO: transform comments into haddocks.
--
launchTxPeer
  :: forall m block txid tx.
     ( RunNode block
     , m ~ IO
     , txid ~ Mempool.GenTxId block
     , tx ~ GenTx block
     )
  => Tracer m (TraceBenchTxSubmit txid)
  -- Tracer carrying the benchmarking events
  -> BenchmarkTxSubmitTracers m block
  -- tracer for lower level connection and details of
  -- protocol interactisn, intended for debugging
  -- associated issues.
  -> AssociateWithIOCP
  -- ^ associate a file descriptor with IO completion port
  -> MSTM.TVar m Bool
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
  -> MSTM.TMVar m [tx]
  -- give this peer 1 or more transactions, empty list
  -- signifies stop this peer
  -> m (Async (), Async ())
launchTxPeer tr1 tr2 iocp termTM nc localAddr remoteAddr updROEnv txInChan = do
  tmv <- MSTM.newEmptyTMVarM
  (,) <$> (async $ benchmarkConnectTxSubmit iocp tr2 nc localAddr remoteAddr (txSubmissionClient tmv))
      <*> (async $ bulkSubmission updROEnv tr1 termTM txInChan tmv)
