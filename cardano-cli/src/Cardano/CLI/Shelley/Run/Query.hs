{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import qualified Data.HashMap.Strict as HMS
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import           Numeric (showEFloat)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistMaybe, left)

import           Cardano.Api
import           Cardano.Api.Byron
import           Cardano.Api.Shelley

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError (..), pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Mary.RenderValue (defaultRenderValueOptions, renderValue)
import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))
import           Cardano.CLI.Types

import           Cardano.Binary (decodeFull)
import           Cardano.Crypto.Hash (hashToBytesAsHex)

import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Shelley.Constraints as Ledger
import           Ouroboros.Consensus.Cardano.Block as Consensus (EraMismatch (..))
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)
import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
                   (AcquireFailure (..))
import qualified Shelley.Spec.Ledger.API.Protocol as Ledger
import           Shelley.Spec.Ledger.Scripts ()

{- HLINT ignore "Reduce duplication" -}


data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdAcquireFailure !AcquireFailure
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyCardanoEra !AnyConsensusMode
  | ShelleyQueryCmdByronEra
  | ShelleyQueryCmdEraMismatch !EraMismatch
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    ShelleyQueryCmdAcquireFailure aqFail -> Text.pack $ show aqFail
    ShelleyQueryCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyQueryCmdEraConsensusModeMismatch (AnyCardanoEra era) (AnyConsensusMode cMode) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cMode <>
      " Era: " <> show era
    ShelleyQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occured." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters' era consensusModeParams network mOutFile ->
      runQueryProtocolParameters era consensusModeParams network mOutFile
    QueryTip era consensusModeParams network mOutFile ->
      runQueryTip era consensusModeParams network mOutFile
    QueryStakeDistribution' era consensusModeParams network mOutFile ->
      runQueryStakeDistribution era consensusModeParams network mOutFile
    QueryStakeAddressInfo era consensusModeParams addr network mOutFile ->
      runQueryStakeAddressInfo era consensusModeParams addr network mOutFile
    QueryLedgerState' era consensusModeParams network mOutFile ->
      runQueryLedgerState era consensusModeParams network mOutFile
    QueryProtocolState' era consensusModeParams network mOutFile ->
      runQueryProtocolState era consensusModeParams network mOutFile
    QueryUTxO' era consensusModeParams qFilter networkId mOutFile ->
      runQueryUTxO era consensusModeParams qFilter networkId mOutFile

runQueryProtocolParameters
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters anyEra@(AnyCardanoEra era) (AnyConsensusModeParams cModeParams)
                           network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr
                           readEnvSocketPath

  let consensusMode = consensusModeOnly cModeParams
  eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra (AnyConsensusMode consensusMode))
                 $ toEraInMode era consensusMode

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  qInMode <- case cardanoEraStyle era of
               LegacyByronEra -> left ShelleyQueryCmdByronEra
               ShelleyBasedEra sbe -> return . QueryInEra eraInMode
                                        $ QueryInShelleyBasedEra sbe QueryProtocolParameters

  res <- liftIO $ queryNodeLocalState localNodeConnInfo Nothing qInMode
  case res of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right ePparams ->
      case ePparams of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right pparams -> writeProtocolParameters mOutFile pparams

writeProtocolParameters
  :: Maybe OutputFile
  -> ProtocolParameters
  -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $
        LBS.writeFile fpath (encodePretty pparams)

runQueryTip
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip (AnyCardanoEra era) (AnyConsensusModeParams cModeParams) network mOutFile = do
    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
    tip <- liftIO $ getLocalChainTip localNodeConnInfo

    let consensusMode = consensusModeOnly cModeParams

    mEpoch <- mEpochQuery consensusMode tip localNodeConnInfo

    let output = encodePretty . toObject mEpoch $ toJSON tip

    case mOutFile of
      Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath output
      Nothing                 -> liftIO $ LBS.putStrLn        output
 where
   mEpochQuery
     :: ConsensusMode mode
     -> ChainTip
     -> LocalNodeConnectInfo mode
     -> ExceptT ShelleyQueryCmdError IO (Maybe EpochNo)
   mEpochQuery cMode tip' lNodeConnInfo =
     case toEraInMode era cMode of
       Nothing -> return Nothing
       Just eraInMode ->
         case cardanoEraStyle era of
           LegacyByronEra -> return Nothing
           ShelleyBasedEra sbe -> do
             let epochQuery = QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryEpoch
                 cPoint = chainTipToChainPoint tip'
             eResult <- liftIO $ queryNodeLocalState lNodeConnInfo (Just cPoint) epochQuery
             case eResult of
               Left _acqFail -> return Nothing
               Right eNum -> case eNum of
                               Left _eraMismatch -> return Nothing
                               Right epochNum -> return $ Just epochNum

   toObject :: Maybe EpochNo -> Aeson.Value -> Aeson.Value
   toObject (Just e) (Aeson.Object obj) =
     Aeson.Object $ obj <> HMS.fromList ["epoch" .= toJSON e]
   toObject Nothing (Aeson.Object obj) =
     Aeson.Object $ obj <> HMS.fromList ["epoch" .= Aeson.Null]
   toObject _ _ = Aeson.Null




-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--

runQueryUTxO
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> QueryFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO anyEra@(AnyCardanoEra era) (AnyConsensusModeParams cModeParams)
             qfilter network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let consensusMode = consensusModeOnly cModeParams
  eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra (AnyConsensusMode consensusMode))
                 $ toEraInMode era consensusMode
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  sbe <- getSbe $ cardanoEraStyle era

  qInMode <- createQuery sbe eraInMode

  eUtxo <- liftIO $ queryNodeLocalState localNodeConnInfo Nothing qInMode
  case eUtxo of
    Left aF -> left $ ShelleyQueryCmdAcquireFailure aF
    Right eU -> case eU of
                  Left mismatch -> left $ ShelleyQueryCmdEraMismatch mismatch
                  Right utxo -> writeFilteredUTxOs sbe mOutFile utxo
 where
  createQuery
    :: ShelleyBasedEra era
    -> EraInMode era mode
    -> ExceptT ShelleyQueryCmdError IO (QueryInMode mode (Either EraMismatch (UTxO era)))
  createQuery sbe e = do
    let mFilter = maybeFiltered qfilter
        query = QueryInShelleyBasedEra sbe $ QueryUTxO mFilter
    return $ QueryInEra e query

  maybeFiltered :: QueryFilter -> Maybe (Set AddressAny)
  maybeFiltered (FilterByAddress as) = Just as
  maybeFiltered NoFilter = Nothing


runQueryLedgerState
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState anyEra@(AnyCardanoEra era) (AnyConsensusModeParams cModeParams)
                    network mOutFile = do
    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath

    let consensusMode = consensusModeOnly cModeParams

    eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra (AnyConsensusMode consensusMode))
                  $ toEraInMode era consensusMode

    let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
    sbe <- getSbe $ cardanoEraStyle era


    let qInMode = QueryInEra eraInMode
                    . QueryInShelleyBasedEra sbe
                    $ QueryLedgerState

    res <- liftIO $ queryNodeLocalState localNodeConnInfo Nothing qInMode
    case res of
      Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
      Right eStakeDist ->
        case eStakeDist of
          Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
          Right stakeDist -> obtainLedgerEraClassConstraints sbe
                               $ writeLedgerState mOutFile stakeDist

runQueryProtocolState
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolState anyEra@(AnyCardanoEra era) (AnyConsensusModeParams cModeParams)
                      network mOutFile = do

    SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
    let consensusMode = consensusModeOnly cModeParams

    eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra (AnyConsensusMode consensusMode))
                  $ toEraInMode era consensusMode

    let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

    sbe <- getSbe $ cardanoEraStyle era


    let qInMode = QueryInEra eraInMode
                    . QueryInShelleyBasedEra sbe
                    $ QueryProtocolState


    res <- liftIO $ queryNodeLocalState localNodeConnInfo Nothing qInMode
    case res of
      Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
      Right eStakeDist ->
        case eStakeDist of
          Left mismatch -> left $ ShelleyQueryCmdEraMismatch mismatch
          Right stakeDist -> writeProtocolState mOutFile stakeDist


-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runQueryStakeAddressInfo
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo anyEra@(AnyCardanoEra era) (AnyConsensusModeParams cModeParams)
                         (StakeAddress _ addr) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath

  let consensusMode = consensusModeOnly cModeParams

  eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra (AnyConsensusMode consensusMode))
                 $ toEraInMode era consensusMode

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
  qInMode <- case cardanoEraStyle era of
               LegacyByronEra -> left ShelleyQueryCmdByronEra
               ShelleyBasedEra sbe ->
                 let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr
                     query = QueryInShelleyBasedEra sbe
                               $ QueryStakeAddresses stakeAddr network

                 in return $ QueryInEra eraInMode query

  res <- liftIO $ queryNodeLocalState localNodeConnInfo Nothing qInMode
  case res of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right eDelegsAndRwds ->
      case eDelegsAndRwds of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right delegsAndRewards -> writeStakeAddressInfo mOutFile $ DelegationsAndRewards delegsAndRewards


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
writeStakeAddressInfo mOutFile delegsAndRewards =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty delegsAndRewards)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty delegsAndRewards)

writeLedgerState :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => ToJSON (LedgerState era)
                 => FromCBOR (LedgerState era)
                 => Maybe OutputFile
                 -> SerialisedLedgerState era
                 -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile qState@(SerialisedLedgerState serLedgerState) =
  case mOutFile of
    Nothing -> case decodeLedgerState qState of
                 Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
                 Right ledgerState -> liftIO . LBS.putStrLn $ encodePretty ledgerState
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState
 where
   decodeLedgerState
     :: SerialisedLedgerState era
     -> Either LBS.ByteString (LedgerState era)
   decodeLedgerState (SerialisedLedgerState (Serialised ls)) =
     first (const ls) (decodeFull ls)


writeProtocolState :: Crypto.Crypto StandardCrypto
                   => Maybe OutputFile
                   -> ProtocolState era
                   -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile ps@(ProtocolState pstate) =
  case mOutFile of
    Nothing -> case decodeProtocolState ps of
                 Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
                 Right chainDepstate -> liftIO . LBS.putStrLn $ encodePretty chainDepstate
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        . LBS.writeFile fpath $ unSerialised pstate
 where
  decodeProtocolState
    :: ProtocolState era
    -> Either LBS.ByteString (Ledger.ChainDepState StandardCrypto)
  decodeProtocolState (ProtocolState (Serialised pbs)) =
    first (const pbs) (decodeFull pbs)

writeFilteredUTxOs :: ShelleyBasedEra era
                   -> Maybe OutputFile
                   -> UTxO era
                   -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs shelleyBasedEra' mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs shelleyBasedEra' utxo
      Just (OutputFile fpath) ->
        case shelleyBasedEra' of
          ShelleyBasedEraShelley -> writeUTxo fpath utxo
          ShelleyBasedEraAllegra -> writeUTxo fpath utxo
          ShelleyBasedEraMary -> writeUTxo fpath utxo
 where
   writeUTxo fpath utxo' =
     handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
       $ LBS.writeFile fpath (encodePretty utxo')

printFilteredUTxOs :: ShelleyBasedEra era -> UTxO era -> IO ()
printFilteredUTxOs shelleyBasedEra' (UTxO utxo) = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraAllegra ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraMary    ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
 where
   title :: Text
   title =
     "                           TxHash                                 TxIx        Amount"

printUtxo
  :: ShelleyBasedEra era
  -> (TxIn, TxOut era)
  -> IO ()
printUtxo shelleyBasedEra' txInOutTuple =
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]

    ShelleyBasedEraAllegra ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraMary ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]

 where
  textShowN :: Show a => Int -> a -> Text
  textShowN len x =
    let str = show x
        slen = length str
    in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

  printableValue :: TxOutValue era -> Text
  printableValue (TxOutValue _ val) = renderValue defaultRenderValueOptions val
  printableValue (TxOutAdaOnly _ (Lovelace i)) = Text.pack $ show i


runQueryStakeDistribution
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution anyEra@(AnyCardanoEra era) (AnyConsensusModeParams cModeParams)
                          network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let consensusMode = consensusModeOnly cModeParams
  eraInMode <- hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch anyEra (AnyConsensusMode consensusMode))
                 $ toEraInMode era (consensusModeOnly cModeParams)

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  qInMode <- case cardanoEraStyle era of
               LegacyByronEra -> left ShelleyQueryCmdByronEra
               ShelleyBasedEra sbe ->
                 let query = QueryInShelleyBasedEra sbe
                               QueryStakeDistribution

                 in return $ QueryInEra eraInMode query

  res <- liftIO $ queryNodeLocalState localNodeConnInfo Nothing qInMode
  case res of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right eStakeDist ->
      case eStakeDist of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right stakeDist ->  writeStakeDistribution mOutFile stakeDist

writeStakeDistribution
  :: Maybe OutputFile
  -> Map PoolId Rational
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) stakeDistrib =
  handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakeDistrib)

writeStakeDistribution Nothing stakeDistrib =
  liftIO $ printStakeDistribution stakeDistrib


printStakeDistribution :: Map PoolId Rational -> IO ()
printStakeDistribution stakeDistrib = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  sequence_
    [ putStrLn $ showStakeDistr poolId stakeFraction
    | (poolId, stakeFraction) <- Map.toList stakeDistrib ]
 where
   title :: Text
   title =
     "                           PoolId                                 Stake frac"

   showStakeDistr :: PoolId
                  -> Rational
                  -- ^ Stake fraction
                  -> String
   showStakeDistr poolId stakeFraction =
     concat
       [ Text.unpack (serialiseToBech32 poolId)
       , "   "
       , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
       ]

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress Lovelace, Map StakeAddress PoolId)


mergeDelegsAndRewards :: DelegationsAndRewards -> [(StakeAddress, Maybe Lovelace, Maybe PoolId)]
mergeDelegsAndRewards (DelegationsAndRewards (rewardsMap, delegMap)) =
 [ (stakeAddr, Map.lookup stakeAddr rewardsMap, Map.lookup stakeAddr delegMap)
 | stakeAddr <- nub $ Map.keys rewardsMap ++ Map.keys delegMap
 ]


instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ mergeDelegsAndRewards delegsAndRwds
    where
      delegAndRwdToJson :: (StakeAddress, Maybe Lovelace, Maybe PoolId) -> Aeson.Value
      delegAndRwdToJson (addr, mRewards, mPoolId) =
        Aeson.object
          [ "address" .= serialiseAddress addr
          , "delegation" .= mPoolId
          , "rewardAccountBalance" .= mRewards
          ]

-- Helpers

getSbe :: CardanoEraStyle era -> ExceptT ShelleyQueryCmdError IO (ShelleyBasedEra era)
getSbe LegacyByronEra = left ShelleyQueryCmdByronEra
getSbe (ShelleyBasedEra sbe) = return sbe

obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> ((Ledger.ShelleyBased ledgerera
      , ToJSON (LedgerState era)
      , FromCBOR (LedgerState era)
      ) => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f
