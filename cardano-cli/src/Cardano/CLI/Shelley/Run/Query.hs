module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api
                   (Address, LocalStateQueryError, Network(..), getLocalTip, queryFilteredUTxOFromLocalState,
                    queryPParamsFromLocalState)

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath)
import           Cardano.CLI.Helpers
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))

import           Cardano.Config.Shelley.Protocol (mkNodeClientProtocolTPraos)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT)

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Ouroboros.Consensus.Cardano (protocolClientInfo)
import           Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Ouroboros.Network.Block (getTipPoint)

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.TxData (TxId (..), TxIn (..), TxOut (..))
import           Shelley.Spec.Ledger.UTxO (UTxO (..))

data ShelleyQueryCmdError
  = ShelleyQueryEnvVarSocketErr !EnvSocketError
  | NodeLocalStateQueryError !LocalStateQueryError
  | ShelleyHelperError !HelpersError
  deriving Show


runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters network mOutFile ->
      runQueryProtocolParameters network mOutFile
    QueryTip network ->
      runQueryTip network
    QueryFilteredUTxO addr network mOutFile ->
      runQueryFilteredUTxO addr network mOutFile
    _ -> liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd

runQueryProtocolParameters
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  pparams <- firstExceptT NodeLocalStateQueryError $
    queryPParamsFromLocalState network sockPath (getTipPoint tip)
  firstExceptT ShelleyHelperError $ writeProtocolParameters mOutFile pparams

runQueryTip
  :: Network
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip network = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  liftIO $ putTextLn (show tip)

runQueryFilteredUTxO
  :: Address
  -> Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryFilteredUTxO addr network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  filteredUtxo <- firstExceptT NodeLocalStateQueryError $
    queryFilteredUTxOFromLocalState network sockPath (Set.singleton addr) (getTipPoint tip)
  writeFilteredUTxOs mOutFile filteredUtxo

writeFilteredUTxOs :: Maybe OutputFile -> UTxO TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs utxo
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyHelperError . IOError' fpath) $ LBS.writeFile fpath (encodePretty utxo)

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
