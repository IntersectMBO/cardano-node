module DBSynthesizer.Parsers (parseCommandLine) where

import           Cardano.Node.Types (KESSource (..), ProtocolFilepaths (..))
import           Cardano.Tools.DBSynthesizer.Types
import           Data.Word (Word64)
import           Options.Applicative as Opt
import           Ouroboros.Consensus.Block.Abstract (SlotNo (..))

parseCommandLine :: IO (FilePath, FilePath, ProtocolFilepaths, DBSynthesizerOptions)
parseCommandLine =
    Opt.customExecParser p opts
  where
    p = Opt.prefs Opt.showHelpOnEmpty
    opts = Opt.info parserCommandLine mempty

parserCommandLine :: Parser (FilePath, FilePath, ProtocolFilepaths, DBSynthesizerOptions)
parserCommandLine =
    (,,,)
      <$> parseNodeConfigFilePath
      <*> parseChainDBFilePath
      <*> parseProtocolFilepaths
      <*> parseDBSynthesizerOptions

-- | The forging credentials, as file paths. Byron delegation credentials are
-- not wired up (the synthesizer forges Shelley-based blocks); the KES key path,
-- when given, is interpreted as a key file (not a KES agent socket).
parseProtocolFilepaths :: Parser ProtocolFilepaths
parseProtocolFilepaths =
    mkFilepaths
      <$> optional parseKesKeyFilePath
      <*> optional parseVrfKeyFilePath
      <*> optional parseOperationalCertFilePath
      <*> optional parseBulkFilePath
  where
    mkFilepaths mKes mVrf mCert mBulk =
      ProtocolFilepaths
        { byronCertFile = Nothing
        , byronKeyFile = Nothing
        , shelleyKESSource = KESKeyFilePath <$> mKes
        , shelleyVRFFile = mVrf
        , shelleyCertFile = mCert
        , shelleyBulkCredsFile = mBulk
        }

parseDBSynthesizerOptions :: Parser DBSynthesizerOptions
parseDBSynthesizerOptions =
    DBSynthesizerOptions
      <$> parseForgeOptions
      <*> parseOpenMode

parseForgeOptions :: Parser ForgeLimit
parseForgeOptions =
    ForgeLimitSlot <$> parseSlotLimit
      <|> ForgeLimitBlock <$> parseBlockLimit
      <|> ForgeLimitEpoch <$> parseEpochLimit

parseChainDBFilePath :: Parser FilePath
parseChainDBFilePath =
    strOption
      ( long "db"
          <> metavar "PATH"
          <> help "Path to the Chain DB"
          <> completer (bashCompleter "directory")
      )

parseNodeConfigFilePath :: Parser FilePath
parseNodeConfigFilePath =
    strOption
      ( long "config"
          <> metavar "FILE"
          <> help "Path to the node's config.json"
          <> completer (bashCompleter "file")
      )

parseOperationalCertFilePath :: Parser FilePath
parseOperationalCertFilePath =
    strOption
      ( long "shelley-operational-certificate"
          <> metavar "FILE"
          <> help "Path to the delegation certificate (in JSON TextEnvelope format)"
          <> completer (bashCompleter "file")
      )

parseKesKeyFilePath :: Parser FilePath
parseKesKeyFilePath =
    strOption
      ( long "shelley-kes-key"
          <> metavar "FILE"
          <> help "Path to the KES signing key (in JSON TextEnvelope format)"
          <> completer (bashCompleter "file")
      )

parseVrfKeyFilePath :: Parser FilePath
parseVrfKeyFilePath =
    strOption
      ( long "shelley-vrf-key"
          <> metavar "FILE"
          <> help "Path to the VRF signing key (in JSON TextEnvelope format)"
          <> completer (bashCompleter "file")
      )

parseBulkFilePath :: Parser FilePath
parseBulkFilePath =
    strOption
      ( long "bulk-credentials-file"
          <> metavar "FILE"
          <> help
            "Path to the bulk credentials file (a JSON file containing an array of arrays containing 3 TextEnvelope objects for the opcert, VRF Signing key, KES signing key)"
          <> completer (bashCompleter "file")
      )

parseSlotLimit :: Parser SlotNo
parseSlotLimit =
    SlotNo
      <$> option
        auto
        ( short 's'
            <> long "slots"
            <> metavar "NUMBER"
            <> help "Amount of slots to process"
        )

parseBlockLimit :: Parser Word64
parseBlockLimit =
    option
      auto
      ( short 'b'
          <> long "blocks"
          <> metavar "NUMBER"
          <> help "Amount of blocks to forge"
      )

parseEpochLimit :: Parser Word64
parseEpochLimit =
    option
      auto
      ( short 'e'
          <> long "epochs"
          <> metavar "NUMBER"
          <> help "Amount of epochs to process"
      )

parseForce :: Parser Bool
parseForce =
    switch
      ( short 'f'
          <> help "Force overwrite an existing Chain DB"
      )

parseAppend :: Parser Bool
parseAppend =
    switch
      ( short 'a'
          <> help "Append to an existing Chain DB"
      )

parseOpenMode :: Parser DBSynthesizerOpenMode
parseOpenMode =
    (parseForce *> pure OpenCreateForce)
      <|> (parseAppend *> pure OpenAppend)
      <|> pure OpenCreate
