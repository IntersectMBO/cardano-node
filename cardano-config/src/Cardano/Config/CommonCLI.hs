{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.CommonCLI
  ( CommonCLI(..)
  , parseCommonCLI
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
  , lastFlag
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude

import           Options.Applicative hiding (command)
import qualified Options.Applicative as OA

import           Cardano.Config.Types (CardanoConfiguration(..)
                                                  ,RequireNetworkMagic(..))
import           Cardano.Config.Partial (PartialCardanoConfiguration (..)
                                                    ,PartialCore (..)
                                                    ,finaliseCardanoConfiguration)


data CommonCLI = CommonCLI
  { cliGenesisFile                :: !(Last FilePath)
  , cliGenesisHash                :: !(Last Text)
  , cliStaticKeySigningKeyFile    :: !(Last FilePath)
  , cliStaticKeyDlgCertFile       :: !(Last FilePath)
  , cliPBftSigThd                 :: !(Last Double)
  , cliRequiresNetworkMagic       :: !(Last RequireNetworkMagic)
  , cliDBPath                     :: !(Last FilePath)
  , cliSocketDir                  :: !(Last FilePath)
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
    <*> lastFlag NoRequireNetworkMagic RequireNetworkMagic
           ( long "require-network-magic"
          <> help "Require network magic in transactions."
           )
    <*> lastStrOption (
            long "database-path"
         <> metavar "FILEPATH"
         <> help "Directory where the state is stored."
        )
    <*> lastStrOption (
            long "socket-dir"
         <> metavar "FILEPATH"
         <> help "Directory with local sockets:  ${dir}/node-{core,relay}-${node-id}.socket"
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

lastFlag :: a -> a -> Mod FlagFields a -> Parser (Last a)
lastFlag def act opts  = Last <$> optional (flag def act opts)


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
                   , cliRequiresNetworkMagic
                   , cliDBPath
                   , cliSocketDir
                   } =
      mempty { pccCore = mempty
                    { pcoGenesisFile             = cliGenesisFile
                    , pcoGenesisHash             = cliGenesisHash
                    , pcoStaticKeySigningKeyFile = cliStaticKeySigningKeyFile
                    , pcoStaticKeyDlgCertFile    = cliStaticKeyDlgCertFile
                    , pcoPBftSigThd              = cliPBftSigThd
                    , pcoRequiresNetworkMagic    = cliRequiresNetworkMagic
                    -- TODO: cliUpdate
                    }
             , pccDBPath = cliDBPath
             , pccSocketDir = cliSocketDir
             }

-- TODO: if we're using exceptions for this, then we should use a local
-- excption type, local to this app, that enumerates all the ones we
-- are reporting, and has proper formatting of the result.
-- It would also require catching at the top level and printing.
--
-- Now, that this is a library function, the proper solution would also
-- require having a common error type.
mkConfiguration :: PartialCardanoConfiguration -> CommonCLI -> IO CardanoConfiguration
mkConfiguration partialConfig cli =
    case finaliseCardanoConfiguration $
         mergeConfiguration partialConfig cli
    of
      Left err -> fail $ Prelude.show err
      Right x  -> pure x
