module Cardano.Benchmarking.GeneratorTx.CLI.Run
  ( runCommand 
  ) where

import           Prelude (error)
import           Cardano.Prelude hiding (option)

import           Cardano.Benchmarking.GeneratorTx.CLI.Parsers (GenerateTxs (..))

data CliError = CliError Int deriving Show

runCommand :: GenerateTxs -> ExceptT CliError IO ()
runCommand _ = error ""

{-
runCommand (GenerateTxs
            logConfigFp
            signingKey
            delegCert
            genFile
            socketFp
            targetNodeAddresses
            numOfTxs
            numOfInsPerTx
            numOfOutsPerTx
            feePerTx
            tps
            txAdditionalSize
            explorerAPIEndpoint
            sigKeysFiles) = withIOManagerE $ \iocp -> do
  -- Default update value
  let update = Update (ApplicationName "cardano-sl") 1 $ LastKnownBlockVersion 0 2 0
  nc <- liftIO $ parseNodeConfigurationFP logConfigFp

  -- Logging layer
  (loggingLayer, _) <- liftIO $ createLoggingFeatureCLI
                                  (pack $ showVersion version)
                                  NoEnvironment
                                  (Just logConfigFp)
                                  (ncLogMetrics nc)

  genHash <- getGenesisHash genFile

  firstExceptT
    GenerateTxsError
    $  withRealPBFT
         genHash
         genFile
         (ncReqNetworkMagic nc)
         Nothing
         (Just delegCert)
         (Just signingKey)
         update
         (ncProtocol nc) $ \protocol@(Consensus.ProtocolRealPBFT _ _ _ _ _) ->
                             firstExceptT GenesisBenchmarkRunnerError
                               $ genesisBenchmarkRunner
                                    loggingLayer
                                    iocp
                                    socketFp
                                    protocol
                                    targetNodeAddresses
                                    numOfTxs
                                    numOfInsPerTx
                                    numOfOutsPerTx
                                    feePerTx
                                    tps
                                    txAdditionalSize
                                    explorerAPIEndpoint
                                    [fp | SigningKeyFile fp <- sigKeysFiles]
-}
