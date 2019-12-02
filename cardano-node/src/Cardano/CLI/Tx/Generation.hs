{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}

module Cardano.CLI.Tx.Generation
  ( NumberOfTxs(..)
  , NumberOfInputsPerTx(..)
  , NumberOfOutputsPerTx(..)
  , FeePerTx(..)
  , TPSRate(..)
  , TxAdditionalSize(..)
  , genesisBenchmarkRunner
  ) where

import           Cardano.Prelude
import           Prelude (String)

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
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
import           Network.Socket (AddrInfo (..),
                     AddrInfoFlag (..), Family (..), SocketType (Stream),
                     addrFamily,addrFlags, addrSocketType, defaultHints,
                     getAddrInfo)

import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (appendName)
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.MempoolPayload as CC.Mempool
import qualified Cardano.Chain.UTxO as CC.UTxO
import           Cardano.Config.Logging (LoggingLayer (..), Trace)
import           Cardano.Config.Types (SocketFile)
import qualified Cardano.Crypto as Crypto
import           Cardano.Config.Topology (NodeAddress (..),
                                          NodeHostAddress(..))
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

import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Block(BlockProtocol)
import           Ouroboros.Consensus.Ledger.Byron.Config (pbftProtocolMagic)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..),
                                                        protocolInfo)
import qualified Ouroboros.Consensus.Protocol as Consensus
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock (..),
                                                   GenTx (..),
                                                   ByronConsensusProtocol)
import qualified Ouroboros.Consensus.Ledger.Byron as Byron
import           Ouroboros.Consensus.NodeId (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Protocol.PBFT (pbftExtConfig)

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

data TxGenError = CurrentlyCannotSendTxToRelayNode FilePath
                -- ^ Relay nodes cannot currently be transaction recipients.
                | InsufficientFundsForRecipientTx
                -- ^ Error occurred while creating the target node address.
                | NeedMinimumThreeSigningKeyFiles [FilePath]
                -- ^ Need at least 3 signing key files.
                | SecretKeyDeserialiseError String
                | SecretKeyReadError String
                deriving Show
-----------------------------------------------------------------------------------------
-- | Genesis benchmark runner (we call it in 'Run.runNode').
--
--   Using a _richman_ (from genesis block) to supply some initial
--   amount of funds for disbursment.
-----------------------------------------------------------------------------------------
genesisBenchmarkRunner
  :: LoggingLayer
  -> SocketFile
  -> Consensus.Protocol ByronBlock
  -> NonEmpty NodeAddress
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> FeePerTx
  -> TPSRate
  -> Maybe TxAdditionalSize
  -> [FilePath]
  -> ExceptT TxGenError IO ()
genesisBenchmarkRunner loggingLayer
                       socketFp
                       protocol@(Consensus.ProtocolRealPBFT genesisConfig _ _ _ _)
                       targetNodeAddresses
                       numOfTxs@(NumberOfTxs rawNumOfTxs)
                       numOfInsPerTx
                       numOfOutsPerTx
                       txFee
                       tpsRate
                       txAdditionalSize
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

  let ProtocolInfo{pInfoConfig} = protocolInfo protocol
      genesisAddress   = mkAddressForKey pInfoConfig genesisKey
      sourceAddress    = mkAddressForKey pInfoConfig sourceKey
      recipientAddress = mkAddressForKey pInfoConfig recepientKey

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, addresses are ready *******"

  -- We have to prepare an initial funds (it's the money we'll send from 'genesisAddress' to
  -- 'sourceAddress'), this will be our very first transaction.
  liftIO $ prepareInitialFunds
             lowLevelSubmitTracer
             socketFp
             genesisConfig
             pInfoConfig
             genesisUtxo
             genesisAddress
             sourceAddress
             txFee

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, initial funds are prepared (sent to sourceAddress) *******"

  -- Check if no transactions needed...
  when (rawNumOfTxs > 0) $
    runBenchmark benchTracer
                 connectTracer
                 submitTracer
                 lowLevelSubmitTracer
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
  -> ( Tracer IO (TraceBenchTxSubmit (Byron.GenTxId ByronBlock))
     , Tracer IO SendRecvConnect
     , Tracer IO (SendRecvTxSubmission ByronBlock)
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

  trBenchTotext :: TraceBenchTxSubmit (Byron.GenTxId ByronBlock) -> Text
  trBenchTotext = T.pack . show
  benchTracer :: Tracer IO (TraceBenchTxSubmit (Byron.GenTxId ByronBlock))
  benchTracer = contramap trBenchTotext (toLogObject (appendName "benchmark" tr'))

  trConnectTotext :: SendRecvConnect -> Text
  trConnectTotext = T.pack . show
  connectTracer :: Tracer IO SendRecvConnect
  connectTracer = contramap trConnectTotext (toLogObject (appendName "connect" tr'))

  trSubmitTotext :: SendRecvTxSubmission ByronBlock -> Text
  trSubmitTotext = T.pack . show
  submitTracer :: Tracer IO (SendRecvTxSubmission ByronBlock)
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
  handler e = "Cardano.CLI.Tx.Generation.readSecretKey: "
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
  :: Tracer IO String
  -> SocketFile
  -> CC.Genesis.Config
  -> NodeConfig ByronConsensusProtocol
  -> Map Int ((CC.UTxO.TxIn, CC.UTxO.TxOut), Crypto.SigningKey)
  -> CC.Common.Address
  -> CC.Common.Address
  -> FeePerTx
  -> IO ()
prepareInitialFunds llTracer
                    socketFp
                    genesisConfig
                    pInfoConfig
                    genesisUtxo
                    genesisAddress
                    targetAddress
                    (FeePerTx txFee) = do
  let ((_, out), signingKey) = genesisUtxo Map.! 0 -- Currently there's only 1 element.
      feePerTx = assumeBound . CC.Common.mkLovelace $ txFee
      outBig = CC.UTxO.txOutValue out `subLoveLace` feePerTx
      outForBig = CC.UTxO.TxOut
        { CC.UTxO.txOutAddress = targetAddress
        , CC.UTxO.txOutValue   = outBig
        }

  let genesisTx :: GenTx ByronBlock
      genesisTx = txSpendGenesisUTxOByronPBFT genesisConfig
                                              signingKey
                                              genesisAddress
                                              (NE.fromList [outForBig])

  submitTx socketFp pInfoConfig (CoreId 0) genesisTx llTracer
  -- Done, the first transaction 'initGenTx' is submitted, now 'sourceAddress' has a lot of money.

  let txIn  = CC.UTxO.TxInUtxo (getTxIdFromGenTx genesisTx) 0
      txOut = outForBig
  addToAvailableFunds (txIn, txOut)
  -- Now we can use these money for further transactions.

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
  -> ( Maybe (Word32, CC.Common.Lovelace) -- The 'change' index and value (if any)
     , CC.Common.Lovelace                 -- The associated fees
     , Map r Word32                       -- The offset map in the transaction below
     , GenTx ByronBlock
     )
mkTransaction cfg inputs mChangeAddress payments txAdditionalSize =
  (mChange, fees, offsetMap, genTx)
 where
  -- Take the first input to get signingKey and txoutFrom value.
  ((_, txoutFrom), signingKey) = NE.head inputs
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

  genTx = generalizeTx cfg tx signingKey

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
generalizeTx
  :: NodeConfig ByronConsensusProtocol
  -> CC.UTxO.Tx
  -> Crypto.SigningKey -- signingKey for spending the input
  -> GenTx ByronBlock
generalizeTx config tx signingKey =
  Byron.fromMempoolPayload $
    CC.Mempool.MempoolTx $ CC.UTxO.annotateTxAux $ CC.UTxO.mkTxAux tx witness
 where
  witness = pure $
      CC.UTxO.VKWitness
        (Crypto.toVerification signingKey)
        (Crypto.sign
          (Crypto.getProtocolMagicId . pbftProtocolMagic . pbftExtConfig $ config)
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
  -> ExceptT TxGenError IO FundValueStatus
findAvailablefunds valueThreshold predStatus = do
  mFVS <- liftIO . STM.atomically $ do
    funds <- TVar.readTVar availableFunds
    case find predFVS (Set.toList funds) of
      Nothing                  -> return Nothing
      j@(Just fundValueStatus) -> do
        -- Remove it from set of available funds (to prevent double spending).
        TVar.modifyTVar' availableFunds (Set.delete fundValueStatus)
        return j
  case mFVS of
    Nothing -> left InsufficientFundsForRecipientTx
    Just fvs -> right fvs
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
  :: Tracer IO (TraceBenchTxSubmit (Byron.GenTxId ByronBlock))
  -> Tracer IO SendRecvConnect
  -> Tracer IO (SendRecvTxSubmission ByronBlock)
  -> Tracer IO String
  -> SocketFile
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
  -> ExceptT TxGenError IO ()
runBenchmark benchTracer
             connectTracer
             submitTracer
             lowLevelSubmitTracer
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
             txAdditionalSize = do
  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 1: make enough available UTxO entries *******"
  createMoreFundCoins
    lowLevelSubmitTracer
    socketFp
    pInfoConfig
    sourceKey
    txFee
    numOfTxs
    numOfInsPerTx

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
        :: ROEnv (Byron.GenTxId ByronBlock) (GenTx ByronBlock)
        -> ROEnv (Byron.GenTxId ByronBlock) (GenTx ByronBlock)
      updROEnv defaultROEnv =
        ROEnv { targetBacklog     = targetBacklog defaultROEnv
              , txNumServiceTime  = Just $ minimalTPSRate tpsRate
              , txSizeServiceTime = Nothing
              }

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

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, launch submission threads... *******"

  liftIO $ do
    txsLists <- STM.atomically $ STM.takeTMVar txsListsForTargetNodes
    let targetNodesAddrsAndTxsLists = zip (NE.toList remoteAddresses) txsLists
    allAsyncs <- forM targetNodesAddrsAndTxsLists $ \(remoteAddr, txsList) ->
      -- Launch connection and submission threads for a peer
      -- (corresponding to one target node).
      launchTxPeer benchTracer
                   benchmarkTracers
                   txSubmissionTerm
                   pInfoConfig
                   localAddr
                   remoteAddr
                   updROEnv
                   txsList
    let allAsyncs' = intercalate [] [[c, s] | (c, s) <- allAsyncs]
    -- Just wait for all threads to complete.
    mapM_ (void . wait) allAsyncs'

-- | At this moment 'sourceAddress' contains a huge amount of money (lets call it A).
--   Now we have to split this amount to N equal parts, as a result we'll have
--   N UTxO entries, and alltogether these entries will contain the same amount A.
--   E.g. (1 entry * 1000 ADA) -> (10 entries * 100 ADA).
--   Technically all splitting transactions will send money back to 'sourceAddress'.
createMoreFundCoins
  :: Tracer IO String
  -> SocketFile
  -> NodeConfig ByronConsensusProtocol
  -> Crypto.SigningKey
  -> FeePerTx
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> ExceptT TxGenError IO ()
createMoreFundCoins llTracer
                    socketFp
                    pInfoConfig
                    sourceKey
                    (FeePerTx txFee)
                    (NumberOfTxs numOfTxs)
                    (NumberOfInputsPerTx numOfInsPerTx) = do
  let feePerTx              = assumeBound . CC.Common.mkLovelace $ txFee
      -- The number of splitting txout entries (corresponds to the number of all inputs we will need).
      numSplittingTxOuts    = numOfTxs * (fromIntegral numOfInsPerTx)
      numOutsPerSplittingTx = 60 :: Word64 -- near the upper bound so as not to exceed the tx size limit

  -- Now we have to find the first output with sufficient amount of money.
  -- But since we made only one single transaction, there is only one
  -- 'FundValueStatus' in 'availableFunds', and it definitely contains a
  -- huge amount of money, so we can pick any amount.
  let someMoney = assumeBound . CC.Common.mkLovelace $ 10000000 -- 10 ADA
  fvs <- findAvailablefunds someMoney (const True)
  let sourceAddress = CC.UTxO.txOutAddress . snd $ txDetails fvs
      sourceValue   = CC.UTxO.txOutValue   . snd $ txDetails fvs
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
                                             (txDetails fvs, sourceKey)
                                             numSplittingTxOuts
                                             numOutsPerSplittingTx
                                             42
                                             txOut
                                             []
  liftIO $ forM_ splittingTxs $ \(tx, txDetailsList) -> do
    submitTx socketFp pInfoConfig (CoreId 0) tx llTracer
    -- Update available fundValueStatus to reuse the numSplittingTxOuts TxOuts.
    forM_ txDetailsList addToAvailableFunds
 where
  -- create txs which split the funds to numTxOuts equal parts
  createSplittingTxs
    :: NodeConfig ByronConsensusProtocol
    -> (TxDetails, Crypto.SigningKey)
    -> Word64
    -> Word64
    -> Int
    -> CC.UTxO.TxOut
    -> [(GenTx ByronBlock, [TxDetails])]
    -> [(GenTx ByronBlock, [TxDetails])]
  createSplittingTxs config details numTxOuts maxOutsPerInitTx identityIndex txOut acc
    | numTxOuts <= 0 = reverse acc
    | otherwise =
        let numOutsPerInitTx = min maxOutsPerInitTx numTxOuts
            sourceAddress = CC.UTxO.txOutAddress . snd . fst $ details
            -- same TxOut for all
            outs = Set.fromList $ zip [identityIndex .. identityIndex + (fromIntegral numOutsPerInitTx) - 1]
                                      (repeat txOut)
            (mFunds, _fees, outIndices, genTx) = mkTransaction config
                                                               (details :| [])
                                                               Nothing
                                                               outs
                                                               Nothing
            !txId = getTxIdFromGenTx genTx
            txDetailsList = (flip map) (Map.toList outIndices) $
                \(_, txInIndex) ->
                  let !txIn  = CC.UTxO.TxInUtxo txId txInIndex
                  in (txIn, txOut)
        in
          case mFunds of
            Nothing                 -> reverse $ (genTx, txDetailsList) : acc
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
                                   ((genTx, txDetailsList) : acc)

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
  :: Tracer IO (TraceBenchTxSubmit (Byron.GenTxId ByronBlock))
  -> NodeConfig ByronConsensusProtocol
  -> CC.Common.Address
  -> Crypto.SigningKey
  -> FeePerTx
  -> Int
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> Maybe TxAdditionalSize
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
            txAdditionalSize = do
  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ " Prepare to generate, total number of transactions " ++ show numOfTransactions

  -- Prepare a number of lists for transactions, for all target nodes.
  -- Later we'll write generated transactions in these lists,
  -- and they will be received and submitted by 'bulkSubmission' function.
  forM_ [1 .. numOfTargetNodes] $ \_ ->
    liftIO $ addTxsListForTargetNode txsForSubmission

  txs <- forM [1 .. numOfTransactions] $ \_ -> do
    -- TODO: currently we take the first available output, but don't check the 'status'.
    -- Later it should be changed: we have to use outputs from 'Seen' transactions only.

    -- We take as many available inputs as we need (because we already have sufficient
    -- numbers of inputs after splitting phase).
    txInputs <- forM [1 .. numOfInsPerTx] $ \_ -> do
      fvs <- findAvailablefunds totalValue (const True)
      return (txDetails fvs, sourceKey)

    let (_, _, _, tx :: GenTx ByronBlock) = mkTransaction cfg
                                                                             (NE.fromList txInputs)
                                                                             (Just addressForChange)
                                                                             recipients
                                                                             txAdditionalSize
    return tx

  -- Now we have to split all transactions to parts for every target node.
  -- These parts will be written to corresponding lists and submitted to the node.
  let numOfTxsForOneTargetNode = (fromIntegral numOfTransactions) `div` numOfTargetNodes
      subLists = divListToSublists txs numOfTxsForOneTargetNode
      subListsAndIndices = zip subLists [0 .. numOfTargetNodes - 1]
  forM_ subListsAndIndices $ \(txsForOneTargetNode, nodeIx) ->
    -- Write newly generated txs in the list for particular node.
    -- These txs will be handled by 'bulkSubmission' function and will be sent to
    -- corresponding target node.
    liftIO $ writeTxsInListForTargetNode txsForOneTargetNode nodeIx

  -- It's possible that we have a remaining transactions, in the latest sublist
  -- (if 'numOfTransactions' cannot be divided by 'numOfTargetNodes'). In this case
  -- just send these transactions to the first node. TODO: should we change this behaviour?
  when (length subLists > numOfTargetNodes) $ do
    let stillUnsentTxs = last subLists
    liftIO $ writeTxsInListForTargetNode stillUnsentTxs 0

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
-- TVar for submitter termination.
---------------------------------------------------------------------------------------------------

txSubmissionTerm :: MSTM.TVar IO Bool
txSubmissionTerm = unsafePerformIO $ STM.newTVarIO False

-- stopTxSubmission :: IO ()
-- stopTxSubmission = STM.atomically $
--     STM.modifyTVar' txSubmissionTerm (const True)

---------------------------------------------------------------------------------------------------
-- Txs for submission.
---------------------------------------------------------------------------------------------------

-- | Creates a list of 'TMVar's with lists of transactions for submitting.
--   The number of these lists corresponds to the number of target nodes.
txsListsForTargetNodes :: forall tx . MSTM.TMVar IO [MSTM.TMVar IO [tx]]
txsListsForTargetNodes = unsafePerformIO $ STM.newTMVarIO []

-- | Adds a list for transactions, for particular target node.
addTxsListForTargetNode :: forall tx . MSTM.TMVar IO [tx] -> IO ()
addTxsListForTargetNode listForOneTargetNode = STM.atomically $
  STM.tryTakeTMVar txsListsForTargetNodes >>=
    \case
      Nothing      -> STM.putTMVar txsListsForTargetNodes [listForOneTargetNode]
      Just curList -> STM.putTMVar txsListsForTargetNodes $ curList ++ [listForOneTargetNode]

-- | Writes list of generated transactions to the list, for particular target node.
--   For example, if we have 3 target nodes and write txs to the list 0,
--   these txs will later be sent to the first target node.
writeTxsInListForTargetNode :: forall tx . [tx] -> Int -> IO ()
writeTxsInListForTargetNode txs listIndex = STM.atomically $ do
  txsLists <- STM.takeTMVar txsListsForTargetNodes
  -- We shouldn't check 'listIndex' - we control both list and index, it cannot be out-of-range.
  let txsListForTargetNode = txsLists !! listIndex
  STM.tryTakeTMVar txsListForTargetNode >>=
    \case
      Nothing      -> STM.putTMVar txsListForTargetNode txs
      Just txsList -> STM.putTMVar txsListForTargetNode $ txsList ++ txs
  -- Put updated content back.
  STM.putTMVar txsListsForTargetNodes txsLists

-- | Generator is producing transactions and writes them in the list.
--   Later sumbitter is reading and submitting these transactions.
txsForSubmission :: forall tx . MSTM.TMVar IO [tx]
txsForSubmission = unsafePerformIO $ STM.newTMVarIO []

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
launchTxPeer tr1 tr2 termTM nc localAddr remoteAddr updROEnv txInChan = do
  tmv <- MSTM.newEmptyTMVarM
  (,) <$> (async $ benchmarkConnectTxSubmit tr2 nc localAddr remoteAddr (txSubmissionClient tmv))
      <*> (async $ bulkSubmission updROEnv tr1 termTM txInChan tmv)
