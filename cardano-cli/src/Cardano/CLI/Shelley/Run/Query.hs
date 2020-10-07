{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , renderShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import           Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import           Numeric (showEFloat)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, newExceptT)

import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Protocol
import           Cardano.Api.Typed

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))
import           Cardano.CLI.Types

import           Cardano.Binary (decodeFull)
import           Cardano.Crypto.Hash (hashToBytesAsHex)

import           Ouroboros.Consensus.Cardano.Block (Either (..), EraMismatch (..), Query (..))
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate (Either (DegenQueryResult),
                     Query (DegenQuery))
import           Ouroboros.Network.Block (Serialised (..), getTipPoint)


import qualified Shelley.Spec.Ledger.Address as Ledger
import qualified Shelley.Spec.Ledger.Credential as Ledger
import           Shelley.Spec.Ledger.Delegation.Certificates (IndividualPoolStake (..),
                     PoolDistr (..))
import qualified Shelley.Spec.Ledger.Keys as Ledger
import           Shelley.Spec.Ledger.LedgerState (EpochState)
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.TxBody as Ledger (TxId (..), TxIn (..), TxOut (..))
import qualified Shelley.Spec.Ledger.UTxO as Ledger (UTxO (..))

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
                     (AcquireFailure (..))


data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters protocol network mOutFile ->
      runQueryProtocolParameters protocol network mOutFile
    QueryTip protocol network mOutFile ->
      runQueryTip protocol network mOutFile
    QueryStakeDistribution protocol network mOutFile ->
      runQueryStakeDistribution protocol network mOutFile
    QueryStakeAddressInfo protocol addr network mOutFile ->
      runQueryStakeAddressInfo protocol addr network mOutFile
    QueryLedgerState protocol network mOutFile ->
      runQueryLedgerState protocol network mOutFile
    QueryUTxO protocol qFilter networkId mOutFile ->
      runQueryUTxO protocol qFilter networkId mOutFile


runQueryProtocolParameters
  :: Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters protocol network mOutFile = do
    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr
                           readEnvSocketPath
    pparams <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
               withlocalNodeConnectInfo protocol network sockPath
                 queryPParamsFromLocalState
    writeProtocolParameters mOutFile pparams

writeProtocolParameters
  :: Maybe OutputFile
  -> PParams StandardShelley
  -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $
        LBS.writeFile fpath (encodePretty pparams)

runQueryTip
  :: Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip protocol network mOutFile = do
    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    output <-
      firstExceptT ShelleyQueryCmdLocalStateQueryError $
      withlocalNodeConnectInfo protocol network sockPath $ \connectInfo -> do
        tip <- liftIO $ getLocalTip connectInfo
        let output = case localNodeConsensusMode connectInfo of
                       ByronMode{}   -> encodePretty tip
                       ShelleyMode{} -> encodePretty tip
                       CardanoMode{} -> encodePretty tip
        return output
    case mOutFile of
      Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath output
      Nothing                 -> liftIO $ LBS.putStrLn        output


runQueryUTxO
  :: Protocol
  -> QueryFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO protocol qfilter network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  filteredUtxo <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
    withlocalNodeConnectInfo protocol network sockPath (queryUTxOFromLocalState qfilter)
  writeFilteredUTxOs mOutFile filteredUtxo

runQueryLedgerState
  :: Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState protocol network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  els <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
    withlocalNodeConnectInfo protocol network sockPath queryLocalLedgerState
  case els of
    Right lstate -> writeLedgerState mOutFile lstate
    Left lbs -> do
      liftIO $ putTextLn "Version mismatch between node and consensus, so dumping this as generic CBOR."
      firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR lbs

runQueryStakeAddressInfo
  :: Protocol
  -> StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo protocol addr network mOutFile = do
    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    delegsAndRwds <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
      withlocalNodeConnectInfo
        protocol
        network
        sockPath
        (queryDelegationsAndRewardsFromLocalState (Set.singleton addr))
    writeStakeAddressInfo mOutFile delegsAndRwds

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  deriving (Eq, Show)

renderLocalStateQueryError :: ShelleyQueryCmdLocalStateQueryError -> Text
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
writeStakeAddressInfo mOutFile dr@(DelegationsAndRewards _ _delegsAndRwds) =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty dr)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty dr)

writeLedgerState :: Maybe OutputFile -> EpochState StandardShelley -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile lstate =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty lstate)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty lstate)

writeFilteredUTxOs :: Maybe OutputFile -> Ledger.UTxO StandardShelley -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs utxo
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $ LBS.writeFile fpath (encodePretty utxo)

printFilteredUTxOs :: Ledger.UTxO StandardShelley -> IO ()
printFilteredUTxOs (Ledger.UTxO utxo) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    mapM_ printUtxo $ Map.toList utxo
  where
    title :: Text
    title =
      "                           TxHash                                 TxIx        Lovelace"

    printUtxo :: (Ledger.TxIn StandardShelley, Ledger.TxOut StandardShelley) -> IO ()
    printUtxo (Ledger.TxIn (Ledger.TxId txhash) txin , Ledger.TxOut _ (Coin coin)) =
      Text.putStrLn $
        mconcat
          [ Text.decodeLatin1 (hashToBytesAsHex txhash)
          , textShowN 6 txin
          , textShowN 18 coin -- enough to display maxLovelaceVal
          ]

    textShowN :: Show a => Int -> a -> Text
    textShowN len x =
      let str = show x
          slen = length str
      in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

runQueryStakeDistribution
  :: Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution protocol network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  stakeDist <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
      withlocalNodeConnectInfo
        protocol
        network
        sockPath
        queryStakeDistributionFromLocalState
  writeStakeDistribution mOutFile stakeDist

writeStakeDistribution :: Maybe OutputFile
                       -> PoolDistr StandardShelley
                       -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) (PoolDistr stakeDist) =
    handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
      LBS.writeFile outFile (encodePretty stakeDist)

writeStakeDistribution Nothing stakeDist =
   liftIO $ printStakeDistribution stakeDist

printStakeDistribution :: PoolDistr StandardShelley -> IO ()
printStakeDistribution (PoolDistr stakeDist) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    sequence_
      [ putStrLn $ showStakeDistr (StakePoolKeyHash poolId) stakeFraction (VrfKeyHash vrfKeyId)
      | (poolId, IndividualPoolStake stakeFraction vrfKeyId) <- Map.toList stakeDist ]
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
        [ Text.unpack (serialiseToBech32 poolId)
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
  :: QueryFilter
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (Ledger.UTxO StandardShelley)
queryUTxOFromLocalState qFilter connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      DegenQueryResult result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip, DegenQuery (applyUTxOFilter qFilter))
      return result

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip, QueryIfCurrentShelley (applyUTxOFilter qFilter))
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess utxo -> return utxo
  where
    applyUTxOFilter (FilterByAddress as) = GetFilteredUTxO (toShelleyAddrs as)
    applyUTxOFilter NoFilter             = GetUTxO

    -- TODO: ultimately, these should be exported from Cardano.API.Shelley
    -- for the Shelley-specific types and conversion for the API wrapper types.
    -- But alternatively, the API can also be extended to cover the queries
    -- properly using the API types.

    toShelleyAddrs :: Set (Address Shelley) -> Set (Ledger.Addr StandardShelley)
    toShelleyAddrs = Set.map toShelleyAddr

    toShelleyAddr :: Address era -> Ledger.Addr StandardShelley
    toShelleyAddr (ByronAddress addr)        = Ledger.AddrBootstrap
                                                 (Ledger.BootstrapAddress addr)
    toShelleyAddr (ShelleyAddress nw pc scr) = Ledger.Addr nw pc scr


-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
data DelegationsAndRewards
  = DelegationsAndRewards
      !NetworkId
      !(Map (Ledger.Credential Ledger.Staking StandardShelley)
            (Maybe (Hash StakePoolKey), Coin))

instance ToJSON DelegationsAndRewards where
  toJSON (DelegationsAndRewards nw delegsAndRwds) =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ Map.toList delegsAndRwds
    where
      delegAndRwdToJson
        :: (Ledger.Credential 'Ledger.Staking StandardShelley, (Maybe (Hash StakePoolKey), Coin))
        -> Aeson.Value
      delegAndRwdToJson (k, (d, r)) =
        Aeson.object
          [ "address" .= renderAddress k
          , "delegation" .= d
          , "rewardAccountBalance" .= r
          ]

      renderAddress :: Ledger.Credential Ledger.Staking StandardShelley -> Text
      renderAddress = serialiseAddress . StakeAddress (toShelleyNetwork nw)


-- | Query the current protocol parameters from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryPParamsFromLocalState
  :: LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (PParams StandardShelley)
queryPParamsFromLocalState LocalNodeConnectInfo{
                             localNodeConsensusMode = ByronMode{}
                           } =
    throwError ByronProtocolNotSupportedError

queryPParamsFromLocalState connectInfo@LocalNodeConnectInfo{
                             localNodeConsensusMode = ShelleyMode
                           } = do
    tip <- liftIO $ getLocalTip connectInfo
    DegenQueryResult result <- firstExceptT AcquireFailureError . newExceptT $
      queryNodeLocalState
        connectInfo
        (getTipPoint tip, DegenQuery GetCurrentPParams)
    return result

queryPParamsFromLocalState connectInfo@LocalNodeConnectInfo{
                             localNodeConsensusMode = CardanoMode{}
                           } = do
    tip <- liftIO $ getLocalTip connectInfo
    result <- firstExceptT AcquireFailureError . newExceptT $
      queryNodeLocalState
        connectInfo
        (getTipPoint tip, QueryIfCurrentShelley GetCurrentPParams)
    case result of
      QueryResultEraMismatch eraerr  -> throwError (EraMismatchError eraerr)
      QueryResultSuccess     pparams -> return pparams


-- | Query the current stake distribution from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryStakeDistributionFromLocalState
  :: LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (PoolDistr StandardShelley)
queryStakeDistributionFromLocalState LocalNodeConnectInfo{
                                       localNodeConsensusMode = ByronMode{}
                                     } =
  throwError ByronProtocolNotSupportedError

queryStakeDistributionFromLocalState connectInfo@LocalNodeConnectInfo{
                                       localNodeConsensusMode = ShelleyMode{}
                                     } = do
  tip <- liftIO $ getLocalTip connectInfo
  DegenQueryResult result <- firstExceptT AcquireFailureError . newExceptT $
    queryNodeLocalState
      connectInfo
      (getTipPoint tip, DegenQuery GetStakeDistribution)
  return result

queryStakeDistributionFromLocalState connectInfo@LocalNodeConnectInfo{
                                       localNodeConsensusMode = CardanoMode{}
                                     } = do
  tip <- liftIO $ getLocalTip connectInfo
  result <- firstExceptT AcquireFailureError . newExceptT $
    queryNodeLocalState
      connectInfo
      (getTipPoint tip, QueryIfCurrentShelley GetStakeDistribution)
  case result of
    QueryResultEraMismatch err -> throwError (EraMismatchError err)
    QueryResultSuccess stakeDist -> return stakeDist

queryLocalLedgerState
  :: LocalNodeConnectInfo mode blk
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO
             (Either LByteString (Ledger.EpochState StandardShelley))
queryLocalLedgerState connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      DegenQueryResult result <- firstExceptT AcquireFailureError . newExceptT $
          queryNodeLocalState
            connectInfo
            ( getTipPoint tip
            , DegenQuery $
                GetCBOR GetCurrentEpochState  -- Get CBOR-in-CBOR version
            )
      return (decodeLedgerState result)

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip, QueryIfCurrentShelley (GetCBOR GetCurrentEpochState)) -- Get CBOR-in-CBOR version
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess ls -> return (decodeLedgerState ls)
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
  :: Set StakeAddress
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO DelegationsAndRewards
queryDelegationsAndRewardsFromLocalState stakeaddrs
                                         connectInfo@LocalNodeConnectInfo{
                                           localNodeNetworkId,
                                           localNodeConsensusMode
                                         } =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      DegenQueryResult result <-
        firstExceptT AcquireFailureError . newExceptT $
          queryNodeLocalState
            connectInfo
            ( getTipPoint tip
            , DegenQuery $
                GetFilteredDelegationsAndRewardAccounts
                  (toShelleyStakeCredentials stakeaddrs)
            )
      return (uncurry toDelegsAndRwds result)

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          ( getTipPoint tip
          , QueryIfCurrentShelley $
              GetFilteredDelegationsAndRewardAccounts
                (toShelleyStakeCredentials stakeaddrs)
          )
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess drs -> return $ uncurry toDelegsAndRwds drs
  where
    toDelegsAndRwds
      :: Map (Ledger.Credential Ledger.Staking StandardShelley)
             (Ledger.KeyHash Ledger.StakePool StandardShelley)
      -> Ledger.RewardAccounts StandardShelley
      -> DelegationsAndRewards
    toDelegsAndRwds delegs rwdAcnts =
      DelegationsAndRewards localNodeNetworkId $
        Map.mapWithKey
          (\k v -> (StakePoolKeyHash <$> Map.lookup k delegs, v))
          rwdAcnts

    toShelleyStakeCredentials :: Set StakeAddress
                              -> Set (Ledger.StakeCredential StandardShelley)
    toShelleyStakeCredentials = Set.map (\(StakeAddress _ cred) -> cred)
