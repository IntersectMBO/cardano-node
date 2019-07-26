{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Cardano.Node.CLI (
  -- * Untyped/typed protocol boundary
    Protocol(..)
  , SomeProtocol(..)
  , TraceConstraints
  , ViewMode(..)
  , fromProtocol
  -- * Common CLI
  , CommonCLI(..)
  , parseCommonCLI
  , mergeConfigurationCommonCLI
  -- * Parsers
  , parseSystemStart
  , parseSlotDuration
  , parseProtocol
  , parseNodeId
  , parseCoreNodeId
  , parseNumCoreNodes
  , parseViewMode
  , parseTestnetBalanceOptions
  , parseLovelace
  , parseLovelacePortion
  , parseFakeAvvmOptions
  , parseK
  , parseProtocolMagic
  , parseNetworkMagic
  , parseFilePath
  , parseIntegral
  , parseFlag
  , parseUTCTime
  -- * Generic
  , command'
  ) where

import           Prelude

import           Codec.CBOR.Read (deserialiseFromBytes)

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as LB
import           Data.Foldable (asum)
import           Data.Monoid (Last (..))
import           Data.Reflection (give)
import           Data.Semigroup ((<>))
import           Data.String (IsString)
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           GHC.Generics
import           Options.Applicative

import           Ouroboros.Network.Block (ChainHash, HeaderHash)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol hiding (Protocol)
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Consensus.Protocol as Consensus

import           Cardano.Binary (Annotated (..))
import           Cardano.Chain.Common
import           Cardano.Crypto.ProtocolMagic

import           Cardano.Shell.Constants.CLI
import           Cardano.Shell.Constants.PartialTypes (PartialCardanoConfiguration (..),
                                                       PartialCore (..),
                                                       PartialGenesis (..),
                                                       PartialStaticKeyMaterial (..) )
import           Cardano.Shell.Lib (GeneralException (..))
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Crypto (RequiresNetworkMagic (..),
                                 decodeAbstractHash)
import qualified Cardano.Crypto.Signing as Signing
import           Control.Exception
import           Control.Monad.Except
import           Cardano.Shell.Constants.Types (CardanoConfiguration (..),
                                                Core (..),
                                                StaticKeyMaterial (..),
                                                Genesis (..),
                                                RequireNetworkMagic (..))
import qualified Cardano.Chain.Update as Update

import qualified Cardano.Node.CanonicalJSON as CanonicalJSON


{-------------------------------------------------------------------------------
  Untyped/typed protocol boundary
-------------------------------------------------------------------------------}

data Protocol =
    BFT
  | Praos
  | MockPBFT
  | RealPBFT

-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing.
type TraceConstraints blk =
    ( Condense blk
    , Condense [blk]
    , Condense (ChainHash blk)
    , Condense (Header blk)
    , Condense (HeaderHash blk)
    , Condense (GenTx blk)
    , Show (ApplyTxErr blk)
    , Show (GenTx blk)
    , Show blk
    , Show (Header blk)
    )

data SomeProtocol where
  SomeProtocol :: (RunDemo blk, TraceConstraints blk)
               => Consensus.Protocol blk -> SomeProtocol

fromProtocol :: CardanoConfiguration -> Protocol -> IO SomeProtocol
fromProtocol _ BFT =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = ProtocolMockBFT defaultSecurityParam
fromProtocol _ Praos =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = ProtocolMockPraos defaultDemoPraosParams
fromProtocol _ MockPBFT =
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p
  where
    p = ProtocolMockPBFT defaultDemoPBftParams
fromProtocol CardanoConfiguration{ccCore}
  RealPBFT = do
    let Genesis{geSrc, geGenesisHash} = coGenesis ccCore
        genHash = either (throw . ConfigurationError) id $
                  decodeAbstractHash geGenesisHash
        cvtRNM :: RequireNetworkMagic -> RequiresNetworkMagic
        cvtRNM NoRequireNetworkMagic = RequiresNoMagic
        cvtRNM RequireNetworkMagic   = RequiresMagic

    gcE <- runExceptT (Genesis.mkConfigFromFile (cvtRNM $ coRequiresNetworkMagic ccCore) geSrc genHash)
    let gc = case gcE of
          Left err -> throw err
          Right x  -> x

    mplc <- sequence $ readLeaderCredentials gc <$> coStaticKeyMaterial ccCore

    usParamsE <- eitherDecode <$> LB.readFile (coUpdateSystemParams ccCore)
    let UpdateSystemParams protoV softV = case usParamsE of
          Left err -> error err
          Right x  -> x

    let p = ProtocolRealPBFT
            gc
            (PBftSignatureThreshold <$> coPBftSigThd ccCore)
            protoV
            softV
            mplc
        epochSlots = Genesis.configEpochSlots gc
        protocolMagicId = Genesis.configProtocolMagicId gc
    -- EpochSlots, ProtocolMagicId, and GenesisHash must be given by way of
    -- reflection constraints, even though they are derivable from `p`. That's
    -- because of the typeclass-based design: these values need to be "static"
    -- so they are available to instances, so we fake it.
    return $
      give epochSlots $
      give protocolMagicId $
      give genHash $ SomeProtocol p

readLeaderCredentials :: Genesis.Config -> StaticKeyMaterial -> IO PBftLeaderCredentials
readLeaderCredentials gc StaticKeyMaterial{skmSigningKeyFile, skmDlgCertFile} = do
  let deserialiseSigningKey = Signing.SigningKey . snd . either (error . show) id .
                              deserialiseFromBytes Signing.fromCBORXPrv
  sk <- deserialiseSigningKey <$> LB.readFile skmSigningKeyFile

  dlgE <- CanonicalJSON.canonicalDecPre <$> LB.readFile skmDlgCertFile

  let plcE = case dlgE of
               Left  err -> error . show $ err
               Right dlg -> mkPBftLeaderCredentials gc sk dlg

  case plcE of
    Left  err -> error . show $ err
    Right plc -> pure plc

-- TODO:  move this to 'cardano-shell'
instance Exception Genesis.ConfigurationError

-- Node can be run in two modes.
data ViewMode =
    LiveView    -- Live mode with TUI
  | SimpleView  -- Simple mode, just output text.

{-------------------------------------------------------------------------------
  Update system params
-------------------------------------------------------------------------------}

data UpdateSystemParams =
  UpdateSystemParams
  { uspProtocolVersion :: Update.ProtocolVersion
  , uspSoftwareVersion :: Update.SoftwareVersion
  } deriving (Generic)

deriveFromJSON defaultOptions ''UpdateSystemParams

{-------------------------------------------------------------------------------
  Common CLI
-------------------------------------------------------------------------------}

-- TODO:  this could benefit from using precise types.
-- Sadly, we'll have to merge this into non-specific CardanoConfiguration.
-- ..only to recover type precision later.
-- | CLI Arguments common to all Cardano node flavors
data CommonCLI = CommonCLI
  { cliGenesis            :: !(Last PartialGenesis)
  , cliKeyMaterialSpec    :: !(Last PartialStaticKeyMaterial)
  , cliPBftSigThd         :: !(Last (Maybe Double))
  , cliUpdateSystemParams :: !(Last FilePath)
  }

parseCommonCLI :: Parser CommonCLI
parseCommonCLI = CommonCLI
  <$> (Last . Just <$> configGenesisCLIParser)
  <*> (Last . Just <$> configStaticKeyMaterialCLIParser)
  <*> lastOption    (option auto $ long "pbft-signature-threshold" <> metavar "DOUBLE"   <> help "The filepath to the genesis file.")
  <*> lastStrOption               (long "update-system-params"     <> metavar "FILEPATH" <> help "The filepath to the update system parameters JSON file.")

-- | Perform merging of layers of configuration, for now using just CommonCLI.
--   We expect this process to become more generic at some point.
mergeConfigurationCommonCLI
  :: PartialCardanoConfiguration
  -> CommonCLI
  -> PartialCardanoConfiguration
mergeConfigurationCommonCLI
  pcc@PartialCardanoConfiguration{pccCore}
  CommonCLI{..}
  =
  pcc { pccCore = updateCore <$> pccCore }

  where updateCore
          pc@PartialCore
          {pcoGenesis, pcoStaticKeyMaterial, pcoPBftSigThd, pcoUpdateSystemParams} =
          -- We need to lift (<>) because we need monoidal operation over Partial*
          -- constituents, and not the (Last Partial*) ones.
          pc { pcoGenesis            = liftA2 (<>) pcoGenesis            cliGenesis
             , pcoStaticKeyMaterial  = liftA2 (<>) pcoStaticKeyMaterial  cliKeyMaterialSpec
             , pcoPBftSigThd         =        (<>) pcoPBftSigThd         cliPBftSigThd
             , pcoUpdateSystemParams =        (<>) pcoUpdateSystemParams cliUpdateSystemParams
             }

{-------------------------------------------------------------------------------
  Command parsers
-------------------------------------------------------------------------------}

parseSystemStart :: Parser SystemStart
parseSystemStart = option (SystemStart <$> auto) $ mconcat [
      long "system-start"
    , help "The start time of the system (e.g. \"2018-12-10 15:58:06\""
    ]

parseSlotDuration :: Parser SlotLength
parseSlotDuration = option (mkSlotLength <$> auto) $ mconcat [
      long "slot-duration"
    , value (mkSlotLength 5)
    , help "The slot duration (seconds)"
    ]
  where
    mkSlotLength :: Integer -> SlotLength
    mkSlotLength = slotLengthFromMillisec . (* 1000)

parseProtocol :: Parser Protocol
parseProtocol = asum [
      flag' BFT $ mconcat [
          long "bft"
        , help "Use the BFT consensus algorithm"
        ]
    , flag' Praos $ mconcat [
          long "praos"
        , help "Use the Praos consensus algorithm"
        ]
    , flag' MockPBFT $ mconcat [
          long "mock-pbft"
        , help "Use the Permissive BFT consensus algorithm using a mock ledger"
        ]
    , flag' RealPBFT $ mconcat [
          long "real-pbft"
        , help "Use the Permissive BFT consensus algorithm using the real ledger"
        ]
    ]

parseNodeId :: Parser NodeId
parseNodeId =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node"
    )

parseCoreNodeId :: Parser CoreNodeId
parseCoreNodeId =
    option (fmap CoreNodeId auto) (
            long "core-node-id"
         <> short 'n'
         <> metavar "CORE-NODE-ID"
         <> help "The ID of the core node to which this client is connected."
    )

parseNumCoreNodes :: Parser NumCoreNodes
parseNumCoreNodes =
    option (fmap NumCoreNodes auto) (
            long "num-core-nodes"
         <> short 'm'
         <> metavar "NUM-CORE-NODES"
         <> help "The number of core nodes"
    )

-- Optional flag for live view (with TUI graphics).
parseViewMode :: Parser ViewMode
parseViewMode =
    flag SimpleView LiveView $ mconcat
        [ long "live-view"
        , help "Live view with TUI."
        ]

parseTestnetBalanceOptions :: Parser Genesis.TestnetBalanceOptions
parseTestnetBalanceOptions =
  Genesis.TestnetBalanceOptions
  <$> parseIntegral        "n-poor-addresses"         "Number of poor nodes (with small balance)."
  <*> parseIntegral        "n-delegate-addresses"     "Number of delegate nodes (with huge balance)."
  <*> parseLovelace        "total-balance"            "Total balance owned by these nodes."
  <*> parseLovelacePortion "delegate-share"           "Portion of stake owned by all delegates together."
  <*> parseFlag            "use-hd-addresses"         "Whether generate plain addresses or with hd payload."

parseLovelace :: String -> String -> Parser Lovelace
parseLovelace optname desc =
  either (error . show) id . mkLovelace
  <$> parseIntegral optname desc

parseLovelacePortion :: String -> String -> Parser LovelacePortion
parseLovelacePortion optname desc =
  either (error . show) id . mkLovelacePortion
  <$> parseIntegral optname desc

parseFakeAvvmOptions :: Parser Genesis.FakeAvvmOptions
parseFakeAvvmOptions =
  Genesis.FakeAvvmOptions
  <$> parseIntegral        "avvm-entry-count"         "Number of AVVM addresses."
  <*> parseLovelace        "avvm-entry-balance"       "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
  <$> parseIntegral        "k"                        "The security parameter of the Ouroboros protocol."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated () . ProtocolMagicId
  <$> parseIntegral        "protocol-magic"           "The magic number unique to any instance of Cardano."

parseNetworkMagic :: Parser NetworkMagic
parseNetworkMagic = asum
    [ flag' NetworkMainOrStage $ mconcat [
          long "main-or-staging"
        , help ""
        ]
    , option (fmap NetworkTestnet auto) (
          long "testnet-magic"
       <> metavar "MAGIC"
       <> help "The testnet network magic, decibal"
        )
    ]

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
    strOption (
            long optname
         <> metavar "FILEPATH"
         <> help desc
    )

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc =
    option (fromInteger <$> auto) (
            long optname
         <> metavar "INT"
         <> help desc
    )

parseFlag :: String -> String -> Parser Bool
parseFlag optname desc =
    flag False True (
            long optname
         <> help desc
    )

parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
    option (posixSecondsToUTCTime . fromInteger <$> auto) (
            long optname
         <> metavar "POSIXSECONDS"
         <> help desc
    )

{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]

-- | Lift the parser to an optional @Last@ type.
lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

lastStrOption :: IsString a => Mod OptionFields a -> Parser (Last a)
lastStrOption args = Last <$> optional (strOption args)
