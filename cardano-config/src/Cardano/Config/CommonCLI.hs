{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.CommonCLI
  ( CommonCLI(..)
  , CommonCLIAdvanced(..)
  , parseCommonCLI
  , parseCommonCLI'
  , parseCommonCLIAdvanced
  , parseCommonCLIAdvanced'
  , mergeConfiguration
  , mkConfiguration
   -- * Generic
  , command'
  , lastOption
  , lastAutoOption
  , lastIntOption
  , lastDoubleOption
  , lastBoolOption
  , lastWordOption
  , lastTextListOption
  , lastStrOption
  , lastStrOptionM
  , lastFlag
  ) where

import           Cardano.Prelude hiding (option)
import qualified Prelude

import           Options.Applicative hiding (command)
import qualified Options.Applicative as OA

import qualified Ouroboros.Consensus.BlockchainTime as Consensus

import           Cardano.Config.Partial
import           Cardano.Config.Types


data CommonCLI = CommonCLI
  { cliDBPath                     :: !(Last FilePath)
  , cliGenesisFile                :: !(Last FilePath)
  , cliGenesisHash                :: !(Last Text)
  , cliStaticKeyDlgCertFile       :: !(Last FilePath)
  , cliStaticKeySigningKeyFile    :: !(Last FilePath)
  , cliSocketDir                  :: !(Last FilePath)
  --TODO cliUpdate                :: !PartialUpdate
  }

data CommonCLIAdvanced = CommonCLIAdvanced
  { ccaPBftSigThd                 :: !(Last Double)
  , ccaRequiresNetworkMagic       :: !(Last RequireNetworkMagic)
  , ccaSlotLength                 :: !(Last Consensus.SlotLength)
  --TODO cliUpdate                :: !PartialUpdate
  }

{-------------------------------------------------------------------------------
  Common CLI
-------------------------------------------------------------------------------}


parseCommonCLI' :: Parser PartialCardanoConfiguration
parseCommonCLI' = do
   dbPath  <- lastStrOption
                ( long "database-path"
                <> metavar "FILEPATH"
                <> help "Directory where the state is stored."
                )
   genPath <- lastStrOption
                ( long "genesis-file"
                <> metavar "FILEPATH"
                <> help "The filepath to the genesis file."
                )
   genHash <- lastStrOption
                ( long "genesis-hash"
                <> metavar "GENESIS-HASH"
                <> help "The genesis hash value."
                )
   delCert <- lastStrOption
                ( long "delegation-certificate"
                <> metavar "FILEPATH"
                <> help "Path to the delegation certificate."
                )
   sKey <- lastStrOption
             ( long "signing-key"
             <> metavar "FILEPATH"
             <> help "Path to the signing key."
             )
   socketDir <- lastStrOption
                  ( long "socket-dir"
                  <> metavar "FILEPATH"
                  <> help "Directory with local sockets:\
                          \  ${dir}/node-{core,relay}-${node-id}.socket"
                  )
   pure $ mempty { pccDBPath = dbPath
                 , pccSocketDir = socketDir
                 , pccCore = mempty { pcoGenesisFile = genPath
                                    , pcoGenesisHash = genHash
                                    , pcoStaticKeyDlgCertFile = delCert
                                    , pcoStaticKeySigningKeyFile = sKey
                                    }
                 }

-- | CLI Arguments common to all Cardano node flavors
parseCommonCLI :: Parser CommonCLI
parseCommonCLI =
    CommonCLI
    <$> lastStrOption (
            long "database-path"
         <> metavar "FILEPATH"
         <> help "Directory where the state is stored."
        )
    <*> lastStrOption
           ( long "genesis-file"
          <> metavar "FILEPATH"
          <> help "The filepath to the genesis file."
           )
    <*> lastStrOption
           ( long "genesis-hash"
          <> metavar "GENESIS-HASH"
          <> help "The genesis hash value."
           )
    <*> lastStrOption
           ( long "delegation-certificate"
          <> metavar "FILEPATH"
          <> help "Path to the delegation certificate."
           )
    <*> lastStrOption
           ( long "signing-key"
          <> metavar "FILEPATH"
          <> help "Path to the signing key."
           )
    <*> lastStrOption (
            long "socket-dir"
         <> metavar "FILEPATH"
         <> help "Directory with local sockets:  ${dir}/node-{core,relay}-${node-id}.socket"
        )

parseCommonCLIAdvanced' :: Parser PartialCardanoConfiguration
parseCommonCLIAdvanced' = do
    pbftSigThresh <- lastDoubleOption
                       ( long "pbft-signature-threshold"
                       <> metavar "DOUBLE"
                       <> help "The PBFT signature threshold."
                       <> hidden
                       )
    reqNetMagic <- lastFlag NoRequireNetworkMagic RequireNetworkMagic
                     ( long "require-network-magic"
                     <> help "Require network magic in transactions."
                     <> hidden
                     )
    slotDur <- lastAutoOption
                        ( long "slot-duration"
                          <> metavar "SECONDS"
                          <> help "The slot duration (seconds)"
                          <> hidden
                        )
    pure $ mempty { pccCore = mempty { pcoPBftSigThd = pbftSigThresh
                                     , pcoRequiresNetworkMagic = reqNetMagic
                                     }
                  , pccNode = mempty { pnoSlotLength = mkSlotLength <$> slotDur }

                  }
  where
    mkSlotLength :: Integer -> Consensus.SlotLength
    mkSlotLength = Consensus.slotLengthFromMillisec . (* 1000)


-- | These are advanced options, and so are hidden by default.
parseCommonCLIAdvanced :: Parser CommonCLIAdvanced
parseCommonCLIAdvanced =
    CommonCLIAdvanced
    <$> lastDoubleOption
           ( long "pbft-signature-threshold"
          <> metavar "DOUBLE"
          <> help "The PBFT signature threshold."
          <> hidden
           )
    <*> lastFlag NoRequireNetworkMagic RequireNetworkMagic
           ( long "require-network-magic"
          <> help "Require network magic in transactions."
          <> hidden
           )
    <*> ((mkSlotLength <$>)
         <$> lastAutoOption
             ( long "slot-duration"
               <> metavar "SECONDS"
               <> help "The slot duration (seconds)"
               <> hidden
             ))
  where
    mkSlotLength :: Integer -> Consensus.SlotLength
    mkSlotLength = Consensus.slotLengthFromMillisec . (* 1000)

{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

command' :: Prelude.String -> Prelude.String -> Parser a -> Mod CommandFields a
command' c descr p =
    OA.command c $ info (p <**> helper) $ mconcat [
        progDesc descr
      ]

-- TODO:  deal with cardano-shell duplication
-- | Lift the parser to an optional @Last@ type.
lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

-- | General @Last@ auto option from @Read@ instance.
lastAutoOption :: Read a => Mod OptionFields a -> Parser (Last a)
lastAutoOption args = lastOption (option auto args)

lastIntOption :: Mod OptionFields Int -> Parser (Last Int)
lastIntOption = lastAutoOption

lastDoubleOption :: Mod OptionFields Double -> Parser (Last Double)
lastDoubleOption = lastAutoOption

lastBoolOption :: Mod OptionFields Bool -> Parser (Last Bool)
lastBoolOption = lastAutoOption

lastWordOption :: Mod OptionFields Word -> Parser (Last Word)
lastWordOption = lastAutoOption

lastTextListOption :: Mod OptionFields [Text] -> Parser (Last [Text])
lastTextListOption = lastAutoOption

lastStrOption :: IsString a => Mod OptionFields a -> Parser (Last a)
lastStrOption args = Last <$> optional (strOption args)

lastFlag :: a -> a -> Mod FlagFields a -> Parser (Last a)
lastFlag def act opts  = Last <$> optional (flag def act opts)

-- | Mandatory versions of option parsers.
--   Use these for the cases when presets don't define a default value
--   for a particular field -- i.e. when the field is set to 'mempty'.
lastStrOptionM :: IsString a => Mod OptionFields a -> Parser (Last a)
lastStrOptionM args = Last . Just <$> strOption args


{-------------------------------------------------------------------------------
  Configuration merging
-------------------------------------------------------------------------------}

-- | Perform merging of layers of configuration, but for now, only in a trivial way,
--   just for Cardano.Shell.Constants.Types.{Genesis,StaticKeyMaterial}.
--   We expect this process to become generic at some point.
mergeConfiguration
  :: PartialCardanoConfiguration
  -> CommonCLI
  -> CommonCLIAdvanced
  -> PartialCardanoConfiguration
mergeConfiguration pcc cli cca =
    -- The beauty of this kind of configuration management (using trees of
    -- monoids) is that we can override individual config elements by simply
    -- merging an extra partial config on top. That extra partial config is
    -- built starting from mempty and setting the fields of interest.
    --
    -- TODO:  see TODO in 'initializeAllFeatures' in 'cardano-node.hs'.
    pcc <> commonCLIToPCC cli cca
  where
    commonCLIToPCC
      :: CommonCLI -> CommonCLIAdvanced -> PartialCardanoConfiguration
    commonCLIToPCC cc ca =
      mempty { pccCore = mempty
                    { pcoGenesisFile             = cliGenesisFile cc
                    , pcoGenesisHash             = cliGenesisHash cc
                    , pcoStaticKeySigningKeyFile = cliStaticKeySigningKeyFile cc
                    , pcoStaticKeyDlgCertFile    = cliStaticKeyDlgCertFile cc
                    , pcoPBftSigThd              = ccaPBftSigThd ca
                    , pcoRequiresNetworkMagic    = ccaRequiresNetworkMagic ca
                    -- TODO: cliUpdate
                    }
             , pccNode = mempty
                    { pnoSlotLength              = ccaSlotLength ca
                    }
             , pccDBPath = cliDBPath cc
             , pccSocketDir = cliSocketDir cc
             }

mkConfiguration
  :: PartialCardanoConfiguration
  -> CommonCLI
  -> CommonCLIAdvanced
  -> Either ConfigError CardanoConfiguration
mkConfiguration partialConfig cli cca =
    finaliseCardanoConfiguration $
    mergeConfiguration partialConfig cli cca
