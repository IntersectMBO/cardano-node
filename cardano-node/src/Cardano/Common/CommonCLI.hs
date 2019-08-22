{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Common.CommonCLI
  ( CommonCLI(..)
  , parseCommonCLI
  , mergeConfiguration
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
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude

import           Options.Applicative hiding (command)
import qualified Options.Applicative as OA

import           Cardano.Node.Configuration.Partial ( PartialCardanoConfiguration (..), PartialCore (..) )


data CommonCLI = CommonCLI
  { cliGenesisFile                :: !(Last FilePath)
  , cliGenesisHash                :: !(Last Text)
  , cliStaticKeySigningKeyFile    :: !(Last FilePath)
  , cliStaticKeyDlgCertFile       :: !(Last FilePath)
  , cliPBftSigThd                 :: !(Last Double)
  , cliDBPath                     :: !(Last FilePath)
  , cliSocketPath                 :: !(Last FilePath)
  --TODO cliUpdate                :: !PartialUpdate
  }

{-------------------------------------------------------------------------------
  Common CLI
-------------------------------------------------------------------------------}

-- | CLI Arguments common to all Cardano node flavors


parseCommonCLI :: Parser CommonCLI
parseCommonCLI =
    CommonCLI
    <$> lastStrOption
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
           ( long "signing-key"
          <> metavar "FILEPATH"
          <> help "Path to the signing key."
           )
    <*> lastStrOption
           ( long "delegation-certificate"
          <> metavar "FILEPATH"
          <> help "Path to the delegation certificate."
           )
    <*> lastDoubleOption
           ( long "pbft-signature-threshold"
          <> metavar "DOUBLE"
          <> help "The PBFT signature threshold."
           )
    <*> lastStrOption (
            long "database-path"
         <> metavar "FILEPATH"
         <> help "Directory where the state is stored."
        )
    <*> lastStrOption (
            long "socket-path"
         <> metavar "FILEPATH"
         <> help "The local socket filepath."
        )

{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

command' :: String -> String -> Parser a -> Mod CommandFields a
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


{-------------------------------------------------------------------------------
  Configuration merging
-------------------------------------------------------------------------------}

-- | Perform merging of layers of configuration, but for now, only in a trivial way,
--   just for Cardano.Shell.Constants.Types.{Genesis,StaticKeyMaterial}.
--   We expect this process to become generic at some point.
mergeConfiguration
  :: PartialCardanoConfiguration
  -> CommonCLI
  -> PartialCardanoConfiguration
mergeConfiguration pcc cli =
    -- The beauty of this kind of configuration management (using trees of
    -- monoids) is that we can override individual config elements by simply
    -- merging an extra partial config on top. That extra partial config is
    -- built starting from mempty and setting the fields of interest.
    pcc <> commonCLIToPCC cli
  where
    commonCLIToPCC :: CommonCLI -> PartialCardanoConfiguration
    commonCLIToPCC CommonCLI {
                     cliGenesisFile
                   , cliGenesisHash
                   , cliStaticKeySigningKeyFile
                   , cliStaticKeyDlgCertFile
                   , cliPBftSigThd
                   , cliDBPath
                   , cliSocketPath
                   } =
      mempty { pccCore = mempty
                    { pcoGenesisFile             = cliGenesisFile
                    , pcoGenesisHash             = cliGenesisHash
                    , pcoStaticKeySigningKeyFile = cliStaticKeySigningKeyFile
                    , pcoStaticKeyDlgCertFile    = cliStaticKeyDlgCertFile
                    , pcoPBftSigThd              = cliPBftSigThd
                    -- TODO: cliUpdate
                    }
             , pccDBPath = cliDBPath
             , pccSocketPath = cliSocketPath
             }
