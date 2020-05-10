{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.Shelley.Run.Query
  ( runQueryCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api
                   (Address, Network(..), queryFilteredUTxOFromLocalState,
                    queryPParamsFromLocalState)

import           Cardano.CLI.Environment (readEnvSocketPath)
import           Cardano.CLI.Ops (CliError (..), getLocalTip)
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))

import           Cardano.Config.Protocol (mkConsensusProtocol)
import           Cardano.Config.Types (ConfigYamlFilePath, NodeConfiguration (..),
                     SomeConsensusProtocol (..), parseNodeConfigurationFP)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, left)

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Ouroboros.Consensus.Cardano (Protocol (..), protocolInfo)
import           Ouroboros.Consensus.Config (configCodec)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo(..))
import           Ouroboros.Consensus.Node.Run (nodeNetworkMagic)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Ouroboros.Network.Block (getTipPoint)

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.PParams (PParams)
import           Shelley.Spec.Ledger.TxData (TxId (..), TxIn (..), TxOut (..))
import           Shelley.Spec.Ledger.UTxO (UTxO (..))


runQueryCmd :: QueryCmd -> ExceptT CliError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters configFp mOutFile ->
      runQueryProtocolParameters configFp mOutFile
    QueryFilteredUTxO addr configFp ->
      runQueryFilteredUTxO addr configFp
    _ -> liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd

runQueryProtocolParameters
  :: ConfigYamlFilePath
  -> Maybe OutputFile
  -> ExceptT CliError IO ()
runQueryProtocolParameters configFp mOutFile = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    SomeConsensusProtocol p <- firstExceptT ProtocolError $ mkConsensusProtocol nc Nothing
    case p of
      ptcl@ProtocolRealTPraos{} -> do
        sockPath <- readEnvSocketPath
        tip <- liftIO $ withIOManager $ \iomgr -> getLocalTip iomgr cfg nm sockPath
        pparams <- firstExceptT NodeLocalStateQueryError $
          queryPParamsFromLocalState cfg nm sockPath (getTipPoint tip)
        writeProtocolParameters mOutFile pparams
        where
          cfg = configCodec ptclcfg
          --FIXME: this works, but we should get the magic properly:
          nm  = Testnet (nodeNetworkMagic (Proxy :: Proxy blk) ptclcfg)
          ProtocolInfo{pInfoConfig = ptclcfg} = protocolInfo ptcl

      _ -> left $ IncorrectProtocolSpecifiedError (ncProtocol nc)

runQueryFilteredUTxO
  :: Address
  -> ConfigYamlFilePath
  -> ExceptT CliError IO ()
runQueryFilteredUTxO addr configFp = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    SomeConsensusProtocol p <- firstExceptT ProtocolError $ mkConsensusProtocol nc Nothing

    case p of
      ptcl@ProtocolRealTPraos{} -> do
        sockPath <- readEnvSocketPath
        tip <- liftIO $ withIOManager $ \iomgr -> getLocalTip iomgr cfg nm sockPath
        filteredUtxo <- firstExceptT NodeLocalStateQueryError $
          queryFilteredUTxOFromLocalState cfg nm sockPath
                                          (Set.singleton addr) (getTipPoint tip)
        liftIO $ printFilteredUTxOs filteredUtxo
        where
          cfg = configCodec ptclcfg
          --FIXME: this works, but we should get the magic properly:
          nm  = Testnet (nodeNetworkMagic (Proxy :: Proxy blk) ptclcfg)
          ProtocolInfo{pInfoConfig = ptclcfg} = protocolInfo ptcl

      _ -> left $ IncorrectProtocolSpecifiedError (ncProtocol nc)

writeProtocolParameters :: Maybe OutputFile  -> PParams -> ExceptT CliError IO ()
writeProtocolParameters mOutFile pparams =
    case mOutFile of
      Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
      Just (OutputFile fpath) ->
        handleIOExceptT (IOError fpath) $ LBS.writeFile fpath (encodePretty pparams)


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
