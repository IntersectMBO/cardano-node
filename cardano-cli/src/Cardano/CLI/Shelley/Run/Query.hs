{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , renderShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Prelude (String)
import           Cardano.Prelude hiding (atomically)

import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Numeric (showEFloat)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT,
                   newExceptT)

import           Cardano.Api.Typed
import           Cardano.Api.LocalChainSync (getLocalTip)

import           Cardano.CLI.Shelley.Commands (QueryFilter(..))
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))

import           Cardano.Config.Shelley.Orphans ()
import           Cardano.Config.Types (SocketPath(..))
import           Cardano.Binary (decodeFull)

import           Ouroboros.Consensus.Cardano.Block (Either (..), EraMismatch (..))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Network.Block (Point, getTipPoint)


import qualified Shelley.Spec.Ledger.Address as Ledger
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Credential              as Ledger
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr(..))
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Ledger (PoolDistr(..))
import qualified Shelley.Spec.Ledger.Keys as Ledger
import           Shelley.Spec.Ledger.LedgerState (EpochState)
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.TxData as Ledger (TxId (..), TxIn (..), TxOut (..))
import qualified Shelley.Spec.Ledger.UTxO as Ledger (UTxO(..))

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
                   (AcquireFailure (..))


data ShelleyQueryCmdError
  = ShelleyQueryEnvVarSocketErr !EnvSocketError
  | ShelleyQueryNodeLocalStateQueryError !LocalStateQueryError
  | ShelleyQueryWriteProtocolParamsError !FilePath !IOException
  | ShelleyQueryWriteFilteredUTxOsError !FilePath !IOException
  | ShelleyQueryWriteStakeDistributionError !FilePath !IOException
  | ShelleyQueryWriteLedgerStateError !FilePath !IOException
  | ShelleyQueryWriteStakeAddressInfoError !FilePath !IOException
  | ShelleyHelpersError !HelpersError
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryNodeLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryWriteProtocolParamsError fp ioException ->
      "Error writing protocol parameters at: " <> show fp <> " Error: " <> show ioException
    ShelleyQueryWriteFilteredUTxOsError fp ioException ->
      "Error writing filtered UTxOs at: " <> show fp <> " Error: " <> show ioException
    ShelleyQueryWriteStakeDistributionError fp ioException ->
      "Error writing stake distribution at: " <> show fp <> " Error: " <> show ioException
    ShelleyQueryWriteLedgerStateError fp ioException ->
      "Error writing ledger state at: " <> show fp <> " Error: " <> show ioException
    ShelleyQueryWriteStakeAddressInfoError fp ioException ->
      "Error writing stake address info at: " <> show fp <> " Error: " <> show ioException
    ShelleyHelpersError helpersErr -> renderHelpersError helpersErr

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters network mOutFile ->
      runQueryProtocolParameters network mOutFile
    QueryTip network mOutFile ->
      runQueryTip network mOutFile
    QueryStakeDistribution network mOutFile ->
      runQueryStakeDistribution network mOutFile
    QueryStakeAddressInfo addr network mOutFile ->
      runQueryStakeAddressInfo addr network mOutFile
    QueryLedgerState network mOutFile ->
      runQueryLedgerState network mOutFile
    QueryUTxO qFilter networkId mOutFile ->
      runQueryUTxO qFilter networkId mOutFile
    _ -> liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd


runQueryProtocolParameters
  :: NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let connectInfo =
        LocalNodeConnectInfo {
          localNodeSocketPath    = sockPath,
          localNodeNetworkId     = network,
          localNodeConsensusMode = ShelleyMode
        }
  tip <- liftIO $ getLocalTip connectInfo
  pparams <- firstExceptT (ShelleyQueryNodeLocalStateQueryError . AcquireFailureError) $
    queryPParamsFromLocalState connectInfo (getTipPoint tip)
  writeProtocolParameters mOutFile pparams

writeProtocolParameters :: Maybe OutputFile -> PParams -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteProtocolParamsError fpath) $
        LBS.writeFile fpath (encodePretty pparams)

runQueryTip
  :: NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let connectInfo =
        LocalNodeConnectInfo {
          localNodeSocketPath    = sockPath,
          localNodeNetworkId     = network,
          localNodeConsensusMode = ShelleyMode
        }
  tip <- liftIO $ getLocalTip connectInfo
  case mOutFile of
    Just (OutputFile fpath) -> liftIO . LBS.writeFile fpath $ encodePretty tip
    Nothing -> liftIO $ LBS.putStrLn (encodePretty tip)


runQueryUTxO
  :: QueryFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO qfilter network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let connectInfo =
        LocalNodeConnectInfo {
          localNodeSocketPath    = sockPath,
          localNodeNetworkId     = network,
          localNodeConsensusMode = ShelleyMode
        }
  tip <- liftIO $ getLocalTip connectInfo
  filteredUtxo <- firstExceptT (ShelleyQueryNodeLocalStateQueryError . AcquireFailureError) $
    queryUTxOFromLocalState connectInfo qfilter (getTipPoint tip)
  writeFilteredUTxOs mOutFile filteredUtxo

runQueryLedgerState
  :: NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let connectInfo =
        LocalNodeConnectInfo {
          localNodeSocketPath    = sockPath,
          localNodeNetworkId     = network,
          localNodeConsensusMode = ShelleyMode
        }
  tip <- liftIO $ getLocalTip connectInfo
  els <- firstExceptT (ShelleyQueryNodeLocalStateQueryError . AcquireFailureError) $
                      queryLocalLedgerState connectInfo (getTipPoint tip)
  case els of
    Right lstate -> writeLedgerState mOutFile lstate
    Left lbs -> do
      liftIO $ putTextLn "Version mismatch between node and consensus, so dumping this as generic CBOR."
      firstExceptT ShelleyHelpersError $ pPrintCBOR lbs

runQueryStakeAddressInfo
  :: StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo addr network mOutFile = do
    SocketPath sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
    let connectInfo =
          LocalNodeConnectInfo {
            localNodeSocketPath    = sockPath,
            localNodeNetworkId     = network,
            localNodeConsensusMode = ShelleyMode
          }
    tip <- liftIO $ getLocalTip connectInfo
    delegsAndRwds <- firstExceptT (ShelleyQueryNodeLocalStateQueryError . AcquireFailureError) $
      queryDelegationsAndRewardsFromLocalState
        connectInfo
        (Set.singleton addr)
        (getTipPoint tip)
    writeStakeAddressInfo mOutFile delegsAndRwds

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data LocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  deriving (Eq, Show)

renderLocalStateQueryError :: LocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    ByronProtocolNotSupportedError ->
      "The attempted local state query does not support the Byron protocol."

writeStakeAddressInfo
  :: Maybe OutputFile
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile dr@(DelegationsAndRewards _delegsAndRwds) =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty dr)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteStakeAddressInfoError fpath)
        $ LBS.writeFile fpath (encodePretty dr)

writeLedgerState :: Maybe OutputFile -> EpochState TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile lstate =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty lstate)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteLedgerStateError fpath)
        $ LBS.writeFile fpath (encodePretty lstate)

writeFilteredUTxOs :: Maybe OutputFile -> Ledger.UTxO TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs utxo
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryWriteFilteredUTxOsError fpath) $ LBS.writeFile fpath (encodePretty utxo)

printFilteredUTxOs :: Ledger.UTxO TPraosStandardCrypto -> IO ()
printFilteredUTxOs (Ledger.UTxO utxo) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    mapM_ printUtxo $ Map.toList utxo
  where
    title :: Text
    title =
      "                           TxHash                                 TxIx        Lovelace"

    printUtxo :: (Ledger.TxIn TPraosStandardCrypto, Ledger.TxOut TPraosStandardCrypto) -> IO ()
    printUtxo (Ledger.TxIn (Ledger.TxId txhash) txin , Ledger.TxOut _ (Coin coin)) =
      Text.putStrLn $
        mconcat
          [ Text.pack (show txhash)
          , textShowN 6 txin
          , textShowN 18 coin -- enough to display maxLovelaceVal
          ]

    textShowN :: Show a => Int -> a -> Text
    textShowN len x =
      let str = show x
          slen = length str
      in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

runQueryStakeDistribution
  :: NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let connectInfo =
        LocalNodeConnectInfo {
          localNodeSocketPath    = sockPath,
          localNodeNetworkId     = network,
          localNodeConsensusMode = ShelleyMode
        }
  tip <- liftIO $ getLocalTip connectInfo
  stakeDist <- firstExceptT (ShelleyQueryNodeLocalStateQueryError . AcquireFailureError) $
    queryStakeDistributionFromLocalState connectInfo (getTipPoint tip)
  writeStakeDistribution mOutFile stakeDist

writeStakeDistribution :: Maybe OutputFile
                       -> PoolDistr TPraosStandardCrypto
                       -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) (PoolDistr stakeDist) =
    handleIOExceptT (ShelleyQueryWriteStakeDistributionError outFile) $
      LBS.writeFile outFile (encodePretty stakeDist)

writeStakeDistribution Nothing stakeDist =
   liftIO $ printStakeDistribution stakeDist

printStakeDistribution :: PoolDistr TPraosStandardCrypto -> IO ()
printStakeDistribution (PoolDistr stakeDist) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    sequence_
      [ putStrLn $ showStakeDistr (StakePoolKeyHash poolId) stakeFraction (VrfKeyHash vrfKeyId)
      | (poolId, (stakeFraction, vrfKeyId)) <- Map.toList stakeDist ]
  where
    title :: Text
    title =
      "                           PoolId                                 Stake frac"

    showStakeDistr :: PoolId
                   -> Rational
                   -> Hash VrfKey
                   -> String
    showStakeDistr poolId stakeFraction _vrfKeyId =
      concat
        [ BS.unpack (serialiseToRawBytesHex poolId)
        , "   "
        , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
-- TODO: we could show the VRF id, but it will then not fit in 80 cols
--      , show vrfKeyId
        ]


-- From Cardano.Api

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryUTxOFromLocalState
  :: LocalNodeConnectInfo ShelleyMode (ShelleyBlock TPraosStandardCrypto)
  -> QueryFilter
  -> Point (ShelleyBlock TPraosStandardCrypto)
  -> ExceptT LocalStateQuery.AcquireFailure IO (Ledger.UTxO TPraosStandardCrypto)
queryUTxOFromLocalState connctInfo qFilter point =
    newExceptT $
      queryNodeLocalState
        connctInfo
        (point, applyUTxOFilter qFilter)
  where
    applyUTxOFilter (FilterByAddress as) = GetFilteredUTxO (toShelleyAddrs as)
    applyUTxOFilter NoFilter             = GetUTxO

    -- TODO: ultimately, these should be exported from Cardano.API.Shelley
    -- for the Shelley-specific types and conversion for the API wrapper types.
    -- But alternatively, the API can also be extended to cover the queries
    -- properly using the API types.

    toShelleyAddrs :: Set (Address Shelley) -> Set (Ledger.Addr TPraosStandardCrypto)
    toShelleyAddrs = Set.map toShelleyAddr

    toShelleyAddr :: Address era -> Ledger.Addr TPraosStandardCrypto
    toShelleyAddr (ByronAddress addr)        = Ledger.AddrBootstrap
                                                 (Ledger.BootstrapAddress addr)
    toShelleyAddr (ShelleyAddress nw pc scr) = Ledger.Addr nw pc scr


-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
newtype DelegationsAndRewards
  = DelegationsAndRewards
      (Map (Ledger.RewardAcnt TPraosStandardCrypto) (Maybe (Hash StakePoolKey), Coin))

instance ToJSON DelegationsAndRewards where
  toJSON (DelegationsAndRewards delegsAndRwds) =
      Aeson.Object $
        Map.foldlWithKey' delegAndRwdToJson HMS.empty delegsAndRwds
    where
      delegAndRwdToJson
        :: HashMap Text Aeson.Value
        -> Ledger.RewardAcnt TPraosStandardCrypto
        -> (Maybe (Hash StakePoolKey), Coin)
        -> HashMap Text Aeson.Value
      delegAndRwdToJson acc k (d, r) =
        HMS.insert
          (Text.decodeLatin1 $ Ledger.serialiseRewardAcnt k)
          (Aeson.object ["delegation" .= d, "rewardAccountBalance" .= r])
          acc


-- | Query the current protocol parameters from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryPParamsFromLocalState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => LocalNodeConnectInfo ShelleyMode blk
  -> Point blk
  -> ExceptT LocalStateQuery.AcquireFailure IO PParams
queryPParamsFromLocalState connctInfo point = do
  let pointAndQuery = (point, GetCurrentPParams)
  newExceptT $ liftIO $
    queryNodeLocalState
      connctInfo
      pointAndQuery

-- | Query the current stake distribution from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryStakeDistributionFromLocalState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => LocalNodeConnectInfo ShelleyMode blk
  -> Point blk
  -> ExceptT LocalStateQuery.AcquireFailure IO (Ledger.PoolDistr TPraosStandardCrypto)
queryStakeDistributionFromLocalState connctInfo point = do
  let pointAndQuery = (point, GetStakeDistribution)
  newExceptT $ liftIO $
    queryNodeLocalState
      connctInfo
      pointAndQuery

queryLocalLedgerState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => LocalNodeConnectInfo ShelleyMode blk
  -> Point blk
  -> ExceptT LocalStateQuery.AcquireFailure IO
             (Either LByteString (Ledger.EpochState TPraosStandardCrypto))
queryLocalLedgerState connctInfo point =
    fmap decodeLedgerState $
    newExceptT . liftIO $
      queryNodeLocalState
        connctInfo
        (point, GetCBOR GetCurrentEpochState) -- Get CBOR-in-CBOR version
  where
    -- If decode as a LedgerState fails we return the ByteString so we can do a generic
    -- CBOR decode.
    decodeLedgerState (Serialised lbs) =
      first (const lbs) (decodeFull lbs)


-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryDelegationsAndRewardsFromLocalState
  :: LocalNodeConnectInfo ShelleyMode (ShelleyBlock TPraosStandardCrypto)
  -> Set StakeAddress
  -> Point (ShelleyBlock TPraosStandardCrypto)
  -> ExceptT LocalStateQuery.AcquireFailure IO DelegationsAndRewards
queryDelegationsAndRewardsFromLocalState connctInfo stakeaddrs point =
    fmap (uncurry toDelegsAndRwds) $
    newExceptT $ liftIO $
      queryNodeLocalState
        connctInfo
        (point, GetFilteredDelegationsAndRewardAccounts
                  (toShelleyStakeCredentials stakeaddrs))
  where
    toDelegsAndRwds
      :: Map (Ledger.Credential Ledger.Staking TPraosStandardCrypto)
             (Ledger.KeyHash Ledger.StakePool TPraosStandardCrypto)
      -> Ledger.RewardAccounts TPraosStandardCrypto
      -> DelegationsAndRewards
    toDelegsAndRwds delegs rwdAcnts =
      DelegationsAndRewards $
        Map.mapWithKey
          (\k v -> (StakePoolKeyHash <$> Map.lookup (Ledger.getRwdCred k) delegs, v))
          rwdAcnts

    toShelleyStakeCredentials :: Set StakeAddress
                              -> Set (Ledger.StakeCredential TPraosStandardCrypto)
    toShelleyStakeCredentials = Set.map (\(StakeAddress _ cred) -> cred)

