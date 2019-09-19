{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumericUnderscores #-}

import qualified Prelude
import           Cardano.Prelude hiding (option)

import           Control.Applicative (some)
import           Control.Exception (Exception)
import           Control.Concurrent (threadDelay)
import           Options.Applicative

import           System.Exit (exitFailure)

import           Cardano.Config.Presets (mainnetConfiguration)

import           Control.Tracer (stdoutTracer)

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import           Ouroboros.Network.Block (SlotNo)
import           Ouroboros.Consensus.BlockchainTime (SlotLength(..))
import qualified Ouroboros.Consensus.Demo.Run as Demo
import qualified Ouroboros.Consensus.Ledger.Byron as Ledger
import qualified Ouroboros.Consensus.Ledger.Byron.Config as Ledger
import qualified Ouroboros.Consensus.Protocol as Protocol
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Node
import           Ouroboros.Consensus.NodeId (CoreNodeId(..))

import           Cardano.Config.CommonCLI
import           Cardano.Config.Protocol (Protocol(..), SomeProtocol(..), fromProtocol)
import           Cardano.Config.Types (CardanoConfiguration(..))
import           Cardano.Common.Parsers (parseCoreNodeId, parseProtocol)
import           Cardano.Common.Protocol

import           Cardano.Chairman (ConsensusValidationMode(..), runChairman)

main :: IO ()
main = do
  ca <- execParser opts
  cc <- case mkConfiguration mainnetConfiguration (caCommonCLI ca) (caCommonCLIAdv ca) of
          Left e -> throwIO e
          Right x -> pure x
  withRealPBFT cc $ main' ca cc

-- 'runChairman' is still generic, but here we have to tap into
-- genesis configuration, and so polymorphism must be limited.
main'
  :: Demo.RunDemo (Ledger.ByronBlockOrEBB Ledger.ByronConfig)
  => ChairmanArgs
  -> CardanoConfiguration
  -> Protocol.Protocol (Ledger.ByronBlockOrEBB Ledger.ByronConfig)
  -> IO ()
main' ca cc p'@(Protocol.ProtocolRealPBFT gc _ _ _ _) =
  case caTimeout ca of
    Nothing      -> run
    Just timeout ->
      run
      `race_`
      do
        threadDelay (timeout * 1_000_000)
        putStrLn $ "Failing with timeout, after "<> Prelude.show timeout <>"seconds."
        exitFailure
  where
    run = runChairman
          p'
          slotLen
          startTime
          (caCoreNodeIds ca)
          (Node.NumCoreNodes . length $ caCoreNodeIds ca)
          (caSecurityParam ca)
          (caConsensusValidation ca)
          (ccSocketDir cc)
          stdoutTracer

    slotLen = slotDurationLength . Update.ppSlotDuration
              . Genesis.gdProtocolParameters . Genesis.configGenesisData $ gc
    startTime = Genesis.configStartTime gc

    slotDurationLength :: Natural -> SlotLength
    slotDurationLength = SlotLength . (/1000) . fromIntegral

data ChairmanArgs = ChairmanArgs {
     _caProtocol        :: !Protocol
    , caCoreNodeIds     :: ![CoreNodeId]
    , caSecurityParam   :: !Protocol.SecurityParam
      -- | Timeout with failure after given number of seconds.
      -- This is useful in combination with 'caConsensusValidation'.
      --
      -- TODO: when we'll have timeouts for 'typed-protocols' we will be able to
      -- detect progress errors separately, while running the 'chain-sync' protocol.
    , caTimeout         :: !(Maybe Int)
      -- | Exit with success when conditions specified by 'caConsensusValidation' hold.
    , caConsensusValidation :: !(Maybe ConsensusValidationMode)
    , caCommonCLI       :: !CommonCLI
    , caCommonCLIAdv    :: !CommonCLIAdvanced
    }

parseSecurityParam :: Parser Protocol.SecurityParam
parseSecurityParam =
    option (Protocol.SecurityParam <$> auto) (
         long "security-param"
      <> short 'k'
      <> metavar "K"
      <> help "The security parameter"
    )

parseSlots :: Prelude.String -> Prelude.String -> Parser SlotNo
parseSlots opt desc =
    option ((fromIntegral :: Int -> SlotNo) <$> auto) (
         long opt
      <> metavar "SLOTS"
      <> help desc
    )

parseIntOption :: Prelude.String -> Prelude.String -> Prelude.String -> Parser Int
parseIntOption opt meta desc =
  option auto (long opt <> metavar meta <> help desc)

parseTimeout :: Parser Int
parseTimeout = parseIntOption "timeout" "SECONDS"
  "Time out after that many seconds."

parseConsensusValidationMode :: Parser (Maybe ConsensusValidationMode)
parseConsensusValidationMode =
  (liftA2 MaxForkLengthForSlots)
    <$> (optional
          $ parseIntOption "maximum-fork-length" "SLOTS"
          "Forks longer than this will break consensus acceptance.")
    <*> (optional
          $ parseSlots "slots-within-tolerance"
          "This many slots with sufficiently short forks does constitute success.")

parseChairmanArgs :: Parser ChairmanArgs
parseChairmanArgs =
    ChairmanArgs
      <$> parseProtocol
      <*> some parseCoreNodeId
      <*> parseSecurityParam
      <*> optional parseTimeout
      <*> parseConsensusValidationMode
      <*> parseCommonCLI
      <*> parseCommonCLIAdvanced

opts :: ParserInfo ChairmanArgs
opts = info (parseChairmanArgs <**> helper)
  ( fullDesc
  <> progDesc "Chairman Shelly application checks if Shelly nodes find consensus."
  <> header "Chairman sits in a room full of Shelley nodes, and checks if they are all behaving ...")

data Timeout = Timeout
  deriving Show

instance Exception Timeout
