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
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Monad (forM, forM_, mapM, when)
import qualified Control.Monad.Class.MonadSTM as MSTM
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT,
                                                   hoistEither,
                                                   left, newExceptT,
                                                   right)
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as LB
import           Data.Either (isLeft)
import           Data.Foldable (find, foldl', foldr, toList)
import qualified Data.IP as IP
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
import           Cardano.Config.Types (CardanoConfiguration(..))
import qualified Cardano.Crypto as Crypto
import           Cardano.Config.Topology (NetworkTopology (..),
                                          NodeAddress (..),
                                          NodeHostAddress(..),
                                          TopologyError(..),
                                          TopologyInfo (..),
                                          createNodeAddress,
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
                | TargetNodeAddressError TopologyError
                -- ^ Error occurred while creating the target node address.
                | TopologyFileReadError String
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
  :: RunNode (ByronBlockOrEBB ByronConfig)
  => LoggingLayer
  -> CardanoConfiguration
  -> Consensus.Protocol (ByronBlockOrEBB ByronConfig)
  -> TopologyInfo
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> FeePerTx
  -> TPSRate
  -> Maybe TxAdditionalSize
  -> [FilePath]
  -> ExceptT TxGenError IO ()
genesisBenchmarkRunner loggingLayer
                       cc
                       protocol@(Consensus.ProtocolRealPBFT genesisConfig _ _ _ _)
                       topologyInfo
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

  (topologyFromFile, ProtocolInfo{pInfoConfig}) <- prepareProtocolInfo protocol topologyInfo

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, protocol info and topology are ready *******"

  -- We have to extract host and port of the node we talk
  -- with (based on value of `-n` CLI argument) from the topology file.
  let eitherNodeAddress = createNodeAddress (node topologyInfo) topologyFromFile (topologyFile topologyInfo)
  targetNodeAddress <- firstExceptT TargetNodeAddressError . hoistEither $ eitherNodeAddress

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, target node's address is ready *******"

  -- 'genesisKey' is for genesis address with initial amount of money (1.4 billion ADA for now).
  -- 'sourceKey' is for source address that we'll use as a source of money for next transactions.
  -- 'recepientKey' is for recipient address that we'll use as an output for next transactions.
  (genesisKey:sourceKey:recepientKey:_) <- prepareSigningKeys signingKeyFiles

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, signing keys are ready *******"

  let genesisUtxo = extractGenesisFunds genesisConfig [genesisKey]

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, genesis UTxO is ready *******"

  let genesisAddress   = mkAddressForKey pInfoConfig genesisKey
      sourceAddress    = mkAddressForKey pInfoConfig sourceKey
      recipientAddress = mkAddressForKey pInfoConfig recepientKey

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, addresses are ready *******"

  -- We have to prepare an initial funds (it's the money we'll send from 'genesisAddress' to
  -- 'sourceAddress'), this will be our very first transaction.
  liftIO $ prepareInitialFunds
             lowLevelSubmitTracer
             cc
             genesisConfig
             pInfoConfig
             topologyInfo
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
                 cc
                 pInfoConfig
                 sourceKey
                 recipientAddress
                 topologyInfo
                 targetNodeAddress
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

prepareProtocolInfo
  :: forall blk. Consensus.Protocol blk
  -> TopologyInfo
  -> ExceptT TxGenError IO (NetworkTopology, ProtocolInfo blk)
prepareProtocolInfo protocol topologyInfo = do
  let topologyfile = readTopologyFile (topologyFile topologyInfo)
  t@(NetworkTopology nodeSetups) <- firstExceptT TopologyFileReadError . newExceptT $ topologyfile
  nodeId <-
    case node topologyInfo of
      RelayId{}  -> left . CurrentlyCannotSendTxToRelayNode $ topologyFile topologyInfo
      CoreId nid -> return nid

  return ( t
         , protocolInfo (NumCoreNodes (length nodeSetups))
                        (CoreNodeId nodeId)
                        protocol
         )

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
  :: NodeConfig ByronEBBExtNodeConfig
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
  :: RunNode (ByronBlockOrEBB ByronConfig)
  => Tracer IO String
  -> CardanoConfiguration
  -> CC.Genesis.Config
  -> NodeConfig ByronEBBExtNodeConfig
  -> TopologyInfo
  -> Map Int ((CC.UTxO.TxIn, CC.UTxO.TxOut), Crypto.SigningKey)
  -> CC.Common.Address
  -> CC.Common.Address
  -> FeePerTx
  -> IO ()
prepareInitialFunds llTracer
                    cc
                    genesisConfig
                    pInfoConfig
                    topologyInfo
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

  let genesisTx :: GenTx (ByronBlockOrEBB ByronConfig)
      genesisTx = txSpendGenesisUTxOByronPBFT genesisConfig
                                              signingKey
                                              genesisAddress
                                              (NE.fromList [outForBig])

  submitTx cc pInfoConfig (node topologyInfo) genesisTx llTracer
  -- Done, the first transaction 'initGenTx' is submitted, now 'sourceAddress' has a lot of money.

  let txIn  = CC.UTxO.TxInUtxo (getTxIdFromGenTx genesisTx) 0
      txOut = outForBig
  addToAvailableFunds (txIn, txOut)
  -- Now we can use these money for further transactions.

-- | Get 'TxId' from 'GenTx'. Since we generate transactions by ourselves -
--   we definitely know that it's 'ByronTx' only.
getTxIdFromGenTx
  :: GenTx (ByronBlockOrEBB cfg)
  -> CC.UTxO.TxId
getTxIdFromGenTx (ByronTx txId _) = txId
getTxIdFromGenTx _ = panic "Impossible happened: generated transaction is not a ByronTx!"

-- | One or more inputs -> one or more outputs.
mkTransaction
  :: (FiscalRecipient r)
  => NodeConfig ByronEBBExtNodeConfig
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
     , GenTx (ByronBlockOrEBB cfg)
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
  :: NodeConfig ByronEBBExtNodeConfig
  -> CC.UTxO.Tx
  -> Crypto.SigningKey -- signingKey for spending the input
  -> GenTx (ByronBlockOrEBB cfg)
generalizeTx (WithEBBNodeConfig config) tx signingKey =
  Byron.mkByronGenTx $
    CC.Mempool.MempoolTx $ CC.UTxO.annotateTxAux $ CC.UTxO.mkTxAux tx witness
 where
  witness = pure $
      CC.UTxO.VKWitness
        (Crypto.toVerification signingKey)
        (Crypto.sign
          (Crypto.getProtocolMagicId . pbftProtocolMagic . encNodeConfigExt $ config)
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
  :: RunNode (ByronBlockOrEBB ByronConfig)
  => Tracer IO (TraceBenchTxSubmit (Byron.GenTxId (ByronBlockOrEBB ByronConfig)))
  -> Tracer IO SendRecvConnect
  -> Tracer IO (SendRecvTxSubmission (ByronBlockOrEBB ByronConfig))
  -> Tracer IO String
  -> CardanoConfiguration
  -> NodeConfig ByronEBBExtNodeConfig
  -> Crypto.SigningKey
  -> CC.Common.Address
  -> TopologyInfo
  -> NodeAddress
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
             cc
             pInfoConfig
             sourceKey
             recipientAddress
             topologyInfo
             targetNodeAddress
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
    cc
    pInfoConfig
    sourceKey
    txFee
    topologyInfo
    numOfTxs
    numOfInsPerTx

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, phase 2: pay to recipients *******"
  let benchmarkTracers :: BenchmarkTxSubmitTracers IO (ByronBlockOrEBB ByronConfig)
      benchmarkTracers = BenchmarkTracers
                           { trSendRecvConnect      = connectTracer
                           , trSendRecvTxSubmission = submitTracer
                           }

  let localAddr :: Maybe Network.Socket.AddrInfo
      localAddr = Nothing

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
  -- Corresponds to a node 0 (used by default).
  (remoteAddr:_) <- liftIO $ getAddrInfo (Just hints) (Just targetNodeHost) (Just targetNodePort)

  let updROEnv
        :: ROEnv (Byron.GenTxId (ByronBlockOrEBB ByronConfig)) (GenTx (ByronBlockOrEBB ByronConfig))
        -> ROEnv (Byron.GenTxId (ByronBlockOrEBB ByronConfig)) (GenTx (ByronBlockOrEBB ByronConfig))
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
              numOfTxs
              numOfInsPerTx
              numOfOutsPerTx
              txAdditionalSize

  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ "******* Tx generator, launch submission threads... *******"

  -- Launch tx submission threads.
  liftIO $ launchTxPeer
             benchTracer
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
  :: RunNode (ByronBlockOrEBB ByronConfig)
  => Tracer IO String
  -> CardanoConfiguration
  -> NodeConfig ByronEBBExtNodeConfig
  -> Crypto.SigningKey
  -> FeePerTx
  -> TopologyInfo
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> ExceptT TxGenError IO ()
createMoreFundCoins llTracer
                    cc
                    pInfoConfig
                    sourceKey
                    (FeePerTx txFee)
                    topologyInfo
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
  :: Tracer IO (TraceBenchTxSubmit (Byron.GenTxId (ByronBlockOrEBB ByronConfig)))
  -> NodeConfig ByronEBBExtNodeConfig
  -> CC.Common.Address
  -> Crypto.SigningKey
  -> FeePerTx
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
            (NumberOfTxs numOfTransactions)
            (NumberOfInputsPerTx numOfInsPerTx)
            (NumberOfOutputsPerTx numOfOutsPerTx)
            txAdditionalSize = do
  liftIO . traceWith benchTracer . TraceBenchTxSubDebug
    $ " Prepare to generate, total number of transactions " ++ show numOfTransactions
  forM_ [1 .. numOfTransactions] $ \_ -> do
    -- TODO: currently we take the first available output, but don't check the 'status'.
    -- Later it should be changed: we have to use outputs from 'Seen' transactions only.

    -- We take as many available inputs as we need (because we already have sufficient
    -- numbers of inputs after splitting phase).
    txInputs <- forM [1 .. numOfInsPerTx] $ \_ -> do
      fvs <- findAvailablefunds totalValue (const True)
      return (txDetails fvs, sourceKey)

    let (_, _, _, tx :: GenTx (ByronBlockOrEBB ByronConfig)) = mkTransaction cfg
                                                                             (NE.fromList txInputs)
                                                                             (Just addressForChange)
                                                                             recipients
                                                                             txAdditionalSize
    -- Write newly generated tx in the list.
    -- It will be handled by 'bulkSubmission' function.
    liftIO $ writeTxInList tx

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
