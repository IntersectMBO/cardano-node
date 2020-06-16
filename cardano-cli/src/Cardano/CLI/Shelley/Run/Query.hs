{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , renderShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Prelude (String)
import           Cardano.Prelude

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Numeric (showEFloat)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT)

import           Cardano.Api
                   (Address (..), DelegationsAndRewards (..), LocalStateQueryError,
                    Network(..), QueryFilter, getLocalTip, queryLocalLedgerState,
                    queryPParamsFromLocalState, queryStakeDistributionFromLocalState,
                    queryUTxOFromLocalState, renderLocalStateQueryError,
                    queryDelegationsAndRewardsFromLocalState, textShow)

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))

import           Cardano.Config.Shelley.Orphans ()
import           Cardano.Api.Protocol.Shelley (mkNodeClientProtocolShelley)

import           Cardano.Crypto.Hash.Class (getHashBytesAsHex)

import           Ouroboros.Consensus.Cardano (protocolClientInfo)
import           Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Ouroboros.Network.Block (getTipPoint)

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr(..))
import           Shelley.Spec.Ledger.Keys (Hash, KeyHash(..), KeyRole (..), VerKeyVRF)
import           Shelley.Spec.Ledger.LedgerState (LedgerState)
import           Shelley.Spec.Ledger.PParams (PParams)
import           Shelley.Spec.Ledger.TxData (TxId (..), TxIn (..), TxOut (..))
import           Shelley.Spec.Ledger.UTxO (UTxO (..))


data ShelleyQueryCmdError
  = ShelleyQueryEnvVarSocketErr !EnvSocketError
  | NodeLocalStateQueryError !LocalStateQueryError
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
    NodeLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryWriteProtocolParamsError fp ioException ->
      "Error writing protocol parameters at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteFilteredUTxOsError fp ioException ->
      "Error writing filtered UTxOs at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteStakeDistributionError fp ioException ->
      "Error writing stake distribution at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteLedgerStateError fp ioException ->
      "Error writing ledger state at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteStakeAddressInfoError fp ioException ->
      "Error writing stake address info at: " <> textShow fp <> " Error: " <> textShow ioException
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
    QueryUTxO qFilter network mOutFile ->
      runQueryUTxO qFilter network mOutFile
    _ -> liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd

runQueryProtocolParameters
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  pparams <- firstExceptT NodeLocalStateQueryError $
    queryPParamsFromLocalState network sockPath (getTipPoint tip)
  writeProtocolParameters mOutFile pparams

writeProtocolParameters :: Maybe OutputFile -> PParams -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteProtocolParamsError fpath) $
        LBS.writeFile fpath (encodePretty pparams)

runQueryTip
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  case mOutFile of
    Just (OutputFile fpath) -> liftIO . writeFile fpath $ show tip
    Nothing -> liftIO $ putTextLn (show tip)


runQueryUTxO
  :: QueryFilter
  -> Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO qfilter network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
            getLocalTip iomgr ptclClientInfo network sockPath
  filteredUtxo <- firstExceptT NodeLocalStateQueryError $
    queryUTxOFromLocalState network sockPath qfilter (getTipPoint tip)
  writeFilteredUTxOs mOutFile filteredUtxo

runQueryLedgerState
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
            getLocalTip iomgr ptclClientInfo network sockPath
  els <- firstExceptT NodeLocalStateQueryError $
                      queryLocalLedgerState network sockPath (getTipPoint tip)
  case els of
    Right lstate -> writeLedgerState mOutFile lstate
    Left lbs -> do
      liftIO $ putTextLn "Verion mismatch beteen node and consensus, so dumping this as generic CBOR."
      firstExceptT ShelleyHelpersError $ pPrintCBOR lbs

runQueryStakeAddressInfo
  :: Address
  -> Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo addr network mOutFile = do
    sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
    let ptclClientInfo = pClientInfoCodecConfig
                       . protocolClientInfo
                       $ mkNodeClientProtocolShelley
    tip <- liftIO $ withIOManager $ \iomgr ->
      getLocalTip iomgr ptclClientInfo network sockPath
    delegsAndRwds <- firstExceptT NodeLocalStateQueryError $
      queryDelegationsAndRewardsFromLocalState
        network
        sockPath
        (Set.singleton addr)
        (getTipPoint tip)
    writeStakeAddressInfo mOutFile delegsAndRwds

-- -------------------------------------------------------------------------------------------------

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

writeLedgerState :: Maybe OutputFile -> LedgerState TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile lstate =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty lstate)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteLedgerStateError fpath)
        $ LBS.writeFile fpath (encodePretty lstate)

writeFilteredUTxOs :: Maybe OutputFile -> UTxO TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs utxo
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryWriteFilteredUTxOsError fpath) $ LBS.writeFile fpath (encodePretty utxo)

printFilteredUTxOs :: UTxO TPraosStandardCrypto -> IO ()
printFilteredUTxOs (UTxO utxo) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    mapM_ printUtxo $ Map.toList utxo
  where
    title :: Text
    title =
      "                           TxHash                                 TxIx        Lovelace"

    printUtxo :: (TxIn TPraosStandardCrypto, TxOut TPraosStandardCrypto) -> IO ()
    printUtxo (TxIn (TxId txhash) txin , TxOut _ (Coin coin)) =
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
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  stakeDist <- firstExceptT NodeLocalStateQueryError $
    queryStakeDistributionFromLocalState network sockPath (getTipPoint tip)
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
      [ putStrLn $ showStakeDistr poolId stakeFraction vrfKeyId
      | (poolId, (stakeFraction, vrfKeyId)) <- Map.toList stakeDist ]
  where
    title :: Text
    title =
      "                           PoolId                                 Stake frac"

    showStakeDistr :: KeyHash 'StakePool crypto
                   -> Rational
                   -> Hash crypto (VerKeyVRF crypto)
                   -> String
    showStakeDistr (KeyHash poolId) stakeFraction _vrfKeyId =
      concat
        [ BS.unpack (getHashBytesAsHex poolId)
        , "   "
        , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
-- TODO: we could show the VRF id, but it will then not fit in 80 cols
--      , show vrfKeyId
        ]
