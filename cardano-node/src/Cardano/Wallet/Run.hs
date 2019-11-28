{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Run
  ( runClient
  ) where

import           Cardano.Prelude

import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text, pack)
import           System.Exit (exitFailure)

import           Cardano.BM.Data.Tracer (ToLogObject (..))
import           Cardano.BM.Trace (Trace, appendName)
import           Cardano.Config.Protocol(SomeProtocol(..), fromProtocol)

import           Ouroboros.Consensus.NodeId (NodeId (..))

import           Cardano.Config.Protocol (ProtocolInstantiationError)
import           Cardano.Config.Types ( ConfigYamlFilePath(..), NodeConfiguration(..)
                                      , parseNodeConfiguration)
import           Cardano.Wallet.Client
import           Cardano.Wallet.Logging (WalletCLI (..))

runClient :: WalletCLI -> Trace IO Text -> IO ()
runClient (WalletCLI config delegCertFile gHash gFile sKeyFile socketFp) tracer = do
    nc <- parseNodeConfiguration $ unConfigPath config
    let coreNodeId = case ncNodeId nc of
                       Nothing -> panic "Cardano.Wallet.Run.srunClient: NodeId not specified"
                       Just (CoreId nid) -> nid
                       Just (RelayId _) -> panic "Cardano.Wallet.Run.runClient: Relay nodes not supported"

    let tracer' = contramap pack . toLogObject $
          appendName ("Wallet " <> pack (show coreNodeId)) tracer

    eSomeProtocol <- runExceptT $ fromProtocol
                                    gHash
                                    (ncNodeId nc)
                                    (ncNumCoreNodes nc)
                                    (Just gFile)
                                    (ncReqNetworkMagic nc)
                                    (ncPbftSignatureThresh nc)
                                    delegCertFile
                                    sKeyFile
                                    (ncUpdate nc)
                                    (ncProtocol nc)

    SomeProtocol p <- case eSomeProtocol of
                        Left err -> (putTextLn $ renderError err) >> exitFailure
                        Right (SomeProtocol p) -> pure $ SomeProtocol p

    runWalletClient p socketFp coreNodeId tracer'

renderError :: ProtocolInstantiationError -> Text
renderError = pack . show
