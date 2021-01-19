{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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

import           Cardano.Api
import           Cardano.Api.Byron
import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.Protocol
import           Cardano.Api.Shelley

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Mary.RenderValue (defaultRenderValueOptions, renderValue)
import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))
import           Cardano.CLI.Types

import           Cardano.Binary (decodeFull)
import           Cardano.Crypto.Hash (hashToBytesAsHex)

import           Ouroboros.Consensus.Cardano.Block as Consensus (Either (..), EraMismatch (..),
                     Query (..))
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import           Ouroboros.Network.Block (Serialised (..), getTipPoint)

import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Shelley.Constraints as Ledger

import qualified Shelley.Spec.Ledger.Address as Ledger
import qualified Shelley.Spec.Ledger.API.Protocol as Ledger
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Credential as Ledger
import           Shelley.Spec.Ledger.Delegation.Certificates (IndividualPoolStake (..),
                     PoolDistr (..))
import qualified Shelley.Spec.Ledger.Keys as Ledger
import           Shelley.Spec.Ledger.LedgerState (NewEpochState)
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import           Shelley.Spec.Ledger.PParams (PParams)
import           Shelley.Spec.Ledger.Scripts ()
import qualified Shelley.Spec.Ledger.TxBody as Ledger (TxId (..), TxIn (..), TxOut (..))
import qualified Shelley.Spec.Ledger.UTxO as Ledger (UTxO (..))

import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

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
    QueryProtocolParameters era protocol network mOutFile ->
      runQueryProtocolParameters era protocol network mOutFile
    QueryTip protocol network mOutFile ->
      runQueryTip protocol network mOutFile
    QueryStakeDistribution era protocol network mOutFile ->
      runQueryStakeDistribution era protocol network mOutFile
    QueryStakeAddressInfo era protocol addr network mOutFile ->
      runQueryStakeAddressInfo era protocol addr network mOutFile
    QueryLedgerState era protocol network mOutFile ->
      runQueryLedgerState era protocol network mOutFile
    QueryProtocolState era protocol network mOutFile ->
      runQueryProtocolState era protocol network mOutFile
    QueryUTxO era protocol qFilter networkId mOutFile ->
      runQueryUTxO era protocol qFilter networkId mOutFile

runQueryProtocolParameters
  :: AnyCardanoEra
  -> Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters (AnyCardanoEra era) protocol network mOutFile
  | ShelleyBasedEra era' <- cardanoEraStyle era = do

    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr
                           readEnvSocketPath
    pparams <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
               withlocalNodeConnectInfo protocol network sockPath $
                 queryPParamsFromLocalState era'
    writeProtocolParameters mOutFile pparams

  | otherwise = throwError (ShelleyQueryCmdLocalStateQueryError
                              ByronProtocolNotSupportedError)


writeProtocolParameters
  :: Maybe OutputFile
  -> PParams ledgerera
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
  :: AnyCardanoEra
  -> Protocol
  -> QueryFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO (AnyCardanoEra era) protocol qfilter network mOutFile
  | ShelleyBasedEra era' <- cardanoEraStyle era =

    -- Obtain the required type equality constaints and class constaints
    obtainToJSONValue era' $
    obtainLedgerEraClassConstraints era' $ do

    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    filteredUtxo <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
      withlocalNodeConnectInfo protocol network sockPath $
        queryUTxOFromLocalState era' qfilter
    writeFilteredUTxOs era' mOutFile filteredUtxo

  | otherwise = throwError (ShelleyQueryCmdLocalStateQueryError
                              ByronProtocolNotSupportedError)


runQueryLedgerState
  :: AnyCardanoEra
  -> Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState (AnyCardanoEra era) protocol network mOutFile
  | ShelleyBasedEra era' <- cardanoEraStyle era =

    -- Obtain the required class constaints
    obtainLedgerEraClassConstraints era' $
    obtainToJSONNewEpochState era' $ do

    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    els <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
      withlocalNodeConnectInfo protocol network sockPath $
        queryLocalLedgerState era'
    case els of
      Right lstate -> writeLedgerState mOutFile lstate
      Left lbs -> do
        liftIO $ putTextLn "Version mismatch between node and consensus, so dumping this as generic CBOR."
        firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR lbs

  | otherwise = throwError (ShelleyQueryCmdLocalStateQueryError
                              ByronProtocolNotSupportedError)


runQueryProtocolState
  :: AnyCardanoEra
  -> Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolState (AnyCardanoEra era) protocol network mOutFile
  | ShelleyBasedEra era' <- cardanoEraStyle era = do

    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    els <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
      withlocalNodeConnectInfo protocol network sockPath $
        queryLocalProtocolState era'
    case els of
      Right protocolState -> writeProtocolState mOutFile protocolState
      Left pbs -> do
        liftIO $ putTextLn "Version mismatch between node and consensus, so dumping this as generic CBOR."
        firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR pbs

    | otherwise = throwError (ShelleyQueryCmdLocalStateQueryError
                              ByronProtocolNotSupportedError)


runQueryStakeAddressInfo
  :: AnyCardanoEra
  -> Protocol
  -> StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo (AnyCardanoEra era) protocol addr network mOutFile
  | ShelleyBasedEra era' <- cardanoEraStyle era =

    -- Obtain the required type equality constaints
    obtainStandardCrypto era' $ do

    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    delegsAndRwds <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
      withlocalNodeConnectInfo protocol network sockPath $
        queryDelegationsAndRewardsFromLocalState era' (Set.singleton addr)
    writeStakeAddressInfo mOutFile delegsAndRwds

  | otherwise = throwError (ShelleyQueryCmdLocalStateQueryError
                              ByronProtocolNotSupportedError)


-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  | ShelleyProtocolEraMismatch
  -- ^ The Shelley protocol only supports the Shelley era.
  deriving (Eq, Show)

renderLocalStateQueryError :: ShelleyQueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    ByronProtocolNotSupportedError ->
      "The attempted local state query does not support the Byron protocol."
    ShelleyProtocolEraMismatch ->
        "The Shelley protocol mode can only be used with the Shelley era, "
     <> "i.e. with --shelley-mode use --shelly-era flag"

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

writeLedgerState :: ToJSON (NewEpochState ledgerera)
                 => Maybe OutputFile
                 -> NewEpochState ledgerera
                 -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile lstate =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty lstate)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty lstate)

writeProtocolState :: Maybe OutputFile
                   -> Ledger.ChainDepState StandardCrypto
                   -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile pstate =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pstate)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty pstate)

writeFilteredUTxOs :: forall era ledgerera.
                      ( Consensus.ShelleyBasedEra ledgerera
                      , ShelleyLedgerEra era ~ ledgerera
                      , Ledger.Crypto ledgerera ~ StandardCrypto
                      , ToJSON (Ledger.Value ledgerera)
                      )
                   => ShelleyBasedEra era
                   -> Maybe OutputFile
                   -> Ledger.UTxO ledgerera
                   -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs era mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs era utxo
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $ LBS.writeFile fpath (encodePretty utxo)

printFilteredUTxOs :: forall era ledgerera.
                      ( Ledger.ShelleyBased ledgerera
                      , ShelleyLedgerEra era ~ ledgerera
                      , Ledger.Crypto ledgerera ~ StandardCrypto
                      )
                   => ShelleyBasedEra era -> Ledger.UTxO ledgerera -> IO ()
printFilteredUTxOs era (Ledger.UTxO utxo) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    mapM_ printUtxo $ Map.toList utxo
  where
    title :: Text
    title =
      "                           TxHash                                 TxIx        Amount"

    printUtxo :: (Ledger.TxIn StandardCrypto, Ledger.TxOut ledgerera) -> IO ()
    printUtxo (Ledger.TxIn (Ledger.TxId txhash) txin , Ledger.TxOut _ value) =
      Text.putStrLn $
        mconcat
          [ Text.decodeLatin1 (hashToBytesAsHex txhash)
          , textShowN 6 txin
          , "        " <> printableValue (convertToApiValue era value)
          ]

    textShowN :: Show a => Int -> a -> Text
    textShowN len x =
      let str = show x
          slen = length str
      in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

    printableValue :: Value -> Text
    printableValue = renderValue defaultRenderValueOptions

runQueryStakeDistribution
  :: AnyCardanoEra
  -> Protocol
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution (AnyCardanoEra era) protocol network mOutFile
  | ShelleyBasedEra era' <- cardanoEraStyle era =

    -- Obtain the required type equality constaints
    obtainStandardCrypto era' $ do

    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    stakeDist <- firstExceptT ShelleyQueryCmdLocalStateQueryError $
        withlocalNodeConnectInfo protocol network sockPath $
          queryStakeDistributionFromLocalState era'
    writeStakeDistribution mOutFile stakeDist

  | otherwise = throwError (ShelleyQueryCmdLocalStateQueryError
                              ByronProtocolNotSupportedError)


writeStakeDistribution :: Maybe OutputFile
                       -> PoolDistr StandardCrypto
                       -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) (PoolDistr stakeDist) =
    handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
      LBS.writeFile outFile (encodePretty stakeDist)

writeStakeDistribution Nothing stakeDist =
   liftIO $ printStakeDistribution stakeDist

printStakeDistribution :: PoolDistr StandardCrypto -> IO ()
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
  :: forall era ledgerera mode block.
     ShelleyLedgerEra era ~ ledgerera
  => Ledger.Crypto ledgerera ~ StandardCrypto
  => IsShelleyBasedEra era
  => ShelleyBasedEra era
  -> QueryFilter
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (Ledger.UTxO ledgerera)
queryUTxOFromLocalState era qFilter
                        connectInfo@LocalNodeConnectInfo{
                          localNodeConsensusMode
                        } =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} | ShelleyBasedEraShelley <- era -> do
      tip <- liftIO $ getLocalTip connectInfo
      Consensus.DegenQueryResult result <-
        firstExceptT AcquireFailureError . newExceptT $
          queryNodeLocalState
            connectInfo
            ( getTipPoint tip
            , Consensus.DegenQuery (applyUTxOFilter qFilter)
            )
      return result

    ShelleyMode{} | otherwise -> throwError ShelleyProtocolEraMismatch

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip, queryIfCurrentEra era (applyUTxOFilter qFilter))
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess utxo -> return utxo
  where
    applyUTxOFilter :: QueryFilter
                    -> Query (Consensus.ShelleyBlock ledgerera)
                             (Ledger.UTxO ledgerera)
    applyUTxOFilter (FilterByAddress as) = Consensus.GetFilteredUTxO (toShelleyAddrs as)
    applyUTxOFilter NoFilter             = Consensus.GetUTxO

    toShelleyAddrs :: Set AddressAny -> Set (Ledger.Addr StandardCrypto)
    toShelleyAddrs = Set.map (toShelleyAddr
                           . (anyAddressInShelleyBasedEra
                                :: AddressAny -> AddressInEra era))

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
data DelegationsAndRewards
  = DelegationsAndRewards
      !NetworkId
      !(Map (Ledger.Credential Ledger.Staking StandardCrypto)
            (Maybe (Hash StakePoolKey), Coin))

instance ToJSON DelegationsAndRewards where
  toJSON (DelegationsAndRewards nw delegsAndRwds) =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ Map.toList delegsAndRwds
    where
      delegAndRwdToJson
        :: (Ledger.Credential Ledger.Staking StandardCrypto, (Maybe (Hash StakePoolKey), Coin))
        -> Aeson.Value
      delegAndRwdToJson (k, (d, r)) =
        Aeson.object
          [ "address" .= renderAddress k
          , "delegation" .= d
          , "rewardAccountBalance" .= r
          ]

      renderAddress :: Ledger.Credential Ledger.Staking StandardCrypto -> Text
      renderAddress = serialiseAddress
                    . StakeAddress (toShelleyNetwork nw)
                    . toShelleyStakeCredential
                    . fromShelleyStakeCredential


-- | Query the current protocol parameters from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryPParamsFromLocalState
  :: forall era ledgerera mode block.
     ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (PParams ledgerera)
queryPParamsFromLocalState _ LocalNodeConnectInfo{
                               localNodeConsensusMode = ByronMode{}
                             } =
    throwError ByronProtocolNotSupportedError

queryPParamsFromLocalState era connectInfo@LocalNodeConnectInfo{
                                 localNodeConsensusMode = ShelleyMode
                               }
  | ShelleyBasedEraShelley <- era = do
    tip <- liftIO $ getLocalTip connectInfo
    Consensus.DegenQueryResult result <-
      firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          ( getTipPoint tip
          , Consensus.DegenQuery Consensus.GetCurrentPParams
          )
    return result

  | otherwise = throwError ShelleyProtocolEraMismatch

queryPParamsFromLocalState era connectInfo@LocalNodeConnectInfo{
                                 localNodeConsensusMode = CardanoMode{}
                               } = do
    tip <- liftIO $ getLocalTip connectInfo
    result <- firstExceptT AcquireFailureError . newExceptT $
      queryNodeLocalState
        connectInfo
        (getTipPoint tip, queryIfCurrentEra era Consensus.GetCurrentPParams)
    case result of
      QueryResultEraMismatch eraerr  -> throwError (EraMismatchError eraerr)
      QueryResultSuccess     pparams -> return pparams


-- | Query the current stake distribution from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryStakeDistributionFromLocalState
  :: forall era ledgerera mode block.
     ShelleyLedgerEra era ~ ledgerera
  => Ledger.Crypto ledgerera ~ StandardCrypto
  => ShelleyBasedEra era
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO (PoolDistr StandardCrypto)
queryStakeDistributionFromLocalState _ LocalNodeConnectInfo{
                                         localNodeConsensusMode = ByronMode{}
                                       } =
  throwError ByronProtocolNotSupportedError

queryStakeDistributionFromLocalState era connectInfo@LocalNodeConnectInfo{
                                           localNodeConsensusMode = ShelleyMode{}
                                         }
  | ShelleyBasedEraShelley <- era = do
    tip <- liftIO $ getLocalTip connectInfo
    Consensus.DegenQueryResult result <-
      firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          ( getTipPoint tip
          , Consensus.DegenQuery Consensus.GetStakeDistribution
          )
    return result

  | otherwise = throwError ShelleyProtocolEraMismatch

queryStakeDistributionFromLocalState era connectInfo@LocalNodeConnectInfo{
                                           localNodeConsensusMode = CardanoMode{}
                                         } = do
  tip <- liftIO $ getLocalTip connectInfo
  result <- firstExceptT AcquireFailureError . newExceptT $
    queryNodeLocalState
      connectInfo
      (getTipPoint tip, queryIfCurrentEra era Consensus.GetStakeDistribution)
  case result of
    QueryResultEraMismatch err -> throwError (EraMismatchError err)
    QueryResultSuccess stakeDist -> return stakeDist

queryLocalLedgerState
  :: forall era ledgerera mode block.
     ShelleyLedgerEra era ~ ledgerera
  => Consensus.ShelleyBasedEra ledgerera
  => ShelleyBasedEra era
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO
             (Either LByteString (NewEpochState ledgerera))
queryLocalLedgerState era connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} | ShelleyBasedEraShelley <- era -> do
      tip <- liftIO $ getLocalTip connectInfo
      Consensus.DegenQueryResult result <-
        firstExceptT AcquireFailureError . newExceptT $
          queryNodeLocalState
            connectInfo
            ( getTipPoint tip
            , Consensus.DegenQuery $
                Consensus.GetCBOR Consensus.DebugNewEpochState
                -- Get CBOR-in-CBOR version
            )
      return (decodeLedgerState result)

    ShelleyMode{} | otherwise -> throwError ShelleyProtocolEraMismatch

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip,
           queryIfCurrentEra era (Consensus.GetCBOR Consensus.DebugNewEpochState))
           -- Get CBOR-in-CBOR version
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess ls -> return (decodeLedgerState ls)
  where
    -- If decode as a LedgerState fails we return the ByteString so we can do a generic
    -- CBOR decode.
    decodeLedgerState (Serialised lbs) =
      first (const lbs) (decodeFull lbs)

queryLocalProtocolState
  :: forall era ledgerera mode block.
     ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO
             (Either LByteString (Ledger.ChainDepState StandardCrypto))
queryLocalProtocolState era connectInfo@LocalNodeConnectInfo{localNodeConsensusMode} =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} | ShelleyBasedEraShelley <- era -> do
      tip <- liftIO $ getLocalTip connectInfo
      Consensus.DegenQueryResult result <-
        firstExceptT AcquireFailureError . newExceptT $
          queryNodeLocalState
            connectInfo
            ( getTipPoint tip
            , Consensus.DegenQuery $
                Consensus.GetCBOR Consensus.DebugChainDepState
                -- Get CBOR-in-CBOR version
            )
      return (decodeProtocolState result)

    ShelleyMode{} | otherwise -> throwError ShelleyProtocolEraMismatch

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          (getTipPoint tip,
           queryIfCurrentEra era (Consensus.GetCBOR Consensus.DebugChainDepState))
                                  -- Get CBOR-in-CBOR version
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess ls -> return (decodeProtocolState ls)
  where
    -- If decode as a ChainDepState fails we return the ByteString so we can do a generic
    -- CBOR decode.
    decodeProtocolState (Serialised pbs) =
      first (const pbs) (decodeFull pbs)


-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryDelegationsAndRewardsFromLocalState
  :: forall era ledgerera mode block.
     ShelleyLedgerEra era ~ ledgerera
  => Ledger.Crypto ledgerera ~ StandardCrypto
  => ShelleyBasedEra era
  -> Set StakeAddress
  -> LocalNodeConnectInfo mode block
  -> ExceptT ShelleyQueryCmdLocalStateQueryError IO
             DelegationsAndRewards
queryDelegationsAndRewardsFromLocalState era stakeaddrs
                                         connectInfo@LocalNodeConnectInfo{
                                           localNodeNetworkId,
                                           localNodeConsensusMode
                                         } =
  case localNodeConsensusMode of
    ByronMode{} -> throwError ByronProtocolNotSupportedError

    ShelleyMode{} | ShelleyBasedEraShelley <- era -> do
      tip <- liftIO $ getLocalTip connectInfo
      Consensus.DegenQueryResult result <-
        firstExceptT AcquireFailureError . newExceptT $
          queryNodeLocalState
            connectInfo
            ( getTipPoint tip
            , Consensus.DegenQuery $
                Consensus.GetFilteredDelegationsAndRewardAccounts
                  (toShelleyStakeCredentials stakeaddrs)
            )
      return (uncurry toDelegsAndRwds result)

    ShelleyMode{} | otherwise -> throwError ShelleyProtocolEraMismatch

    CardanoMode{} -> do
      tip <- liftIO $ getLocalTip connectInfo
      result <- firstExceptT AcquireFailureError . newExceptT $
        queryNodeLocalState
          connectInfo
          ( getTipPoint tip
          , queryIfCurrentEra era $
              Consensus.GetFilteredDelegationsAndRewardAccounts
                (toShelleyStakeCredentials stakeaddrs)
          )
      case result of
        QueryResultEraMismatch err -> throwError (EraMismatchError err)
        QueryResultSuccess drs -> return $ uncurry toDelegsAndRwds drs
  where
    toDelegsAndRwds
      :: Map (Ledger.Credential Ledger.Staking StandardCrypto)
             (Ledger.KeyHash Ledger.StakePool StandardCrypto)
      -> Ledger.RewardAccounts StandardCrypto
      -> DelegationsAndRewards
    toDelegsAndRwds delegs rwdAcnts =
      DelegationsAndRewards localNodeNetworkId $
        Map.mapWithKey
          (\k v -> (StakePoolKeyHash <$> Map.lookup k delegs, v))
          rwdAcnts

    toShelleyStakeCredentials :: Set StakeAddress
                              -> Set (Ledger.StakeCredential StandardCrypto)
    toShelleyStakeCredentials =
      Set.map (toShelleyStakeCredential
             . fromShelleyStakeCredential
             . (\(StakeAddress _ cred) -> cred))


-- -----------------------------------------------------------------------------
-- Era-generic helper functions
--

-- | Select the appropriate query constructor based on the era
-- 'QueryIfCurrentShelley', 'QueryIfCurrentAllegra' or 'QueryIfCurrentMary'.
--
--
queryIfCurrentEra :: ShelleyBasedEra era
                  -> Query (Consensus.ShelleyBlock (ShelleyLedgerEra era)) result
                  -> Consensus.CardanoQuery StandardCrypto
                       (Consensus.CardanoQueryResult StandardCrypto result)
queryIfCurrentEra ShelleyBasedEraShelley = Consensus.QueryIfCurrentShelley
queryIfCurrentEra ShelleyBasedEraAllegra = Consensus.QueryIfCurrentAllegra
queryIfCurrentEra ShelleyBasedEraMary    = Consensus.QueryIfCurrentMary

obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (Consensus.ShelleyBasedEra ledgerera => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f

obtainToJSONNewEpochState
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (ToJSON (NewEpochState ledgerera) => a) -> a
obtainToJSONNewEpochState ShelleyBasedEraShelley f = f
obtainToJSONNewEpochState ShelleyBasedEraAllegra f = f
obtainToJSONNewEpochState ShelleyBasedEraMary    f = f

obtainStandardCrypto
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (Ledger.Crypto ledgerera ~ StandardCrypto => a) -> a
obtainStandardCrypto ShelleyBasedEraShelley f = f
obtainStandardCrypto ShelleyBasedEraAllegra f = f
obtainStandardCrypto ShelleyBasedEraMary    f = f

obtainToJSONValue
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> (ToJSON (Ledger.Value ledgerera) => a) -> a
obtainToJSONValue ShelleyBasedEraShelley f = f
obtainToJSONValue ShelleyBasedEraAllegra f = f
obtainToJSONValue ShelleyBasedEraMary    f = f

-- | Convert a ledger 'Ledger.Value' to a @cardano-api@ 'Value'.
convertToApiValue
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> Ledger.Value ledgerera
  -> Value
convertToApiValue ShelleyBasedEraShelley = lovelaceToValue . fromShelleyLovelace
convertToApiValue ShelleyBasedEraAllegra = lovelaceToValue . fromShelleyLovelace
convertToApiValue ShelleyBasedEraMary = fromMaryValue
