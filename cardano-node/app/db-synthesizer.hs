-- | This tool synthesizes a valid ChainDB, replicating cardano-node's UX.
--
-- Usage: db-synthesizer --config FILE --db PATH
--                       [--shelley-operational-certificate FILE]
--                       [--shelley-vrf-key FILE] [--shelley-kes-key FILE]
--                       [--bulk-credentials-file FILE]
--                       ((-s|--slots NUMBER) | (-b|--blocks NUMBER) |
--                         (-e|--epochs NUMBER)) [-f | -a]
--
-- The node configuration and forging credentials are turned into a Cardano
-- 'ProtocolInfo' and block forgers using cardano-node's own protocol-instantiation
-- machinery (see "Cardano.Node.Tools.DBSynthesizer"); the actual forging is done
-- by @ouroboros-consensus@'s @synthesize@.
module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Node.Tools.DBSynthesizer (synthesizeFromConfig)
import           DBSynthesizer.Parsers (parseCommandLine)

main :: IO ()
main = do
    cryptoInit
    (configFp, dbDir, protocolFiles, opts) <- parseCommandLine
    result <- synthesizeFromConfig configFp protocolFiles opts dbDir
    putStrLn $ "--> done; result: " ++ show result
