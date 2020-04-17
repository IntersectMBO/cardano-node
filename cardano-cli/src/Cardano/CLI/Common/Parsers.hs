module Cardano.CLI.Common.Parsers
  ( ClientCommand (..)
  , cliParseBase58Address
  , cliParseLovelace
  , cliParseTxId
  , parseAddress
  , parseCardanoEra
  , parseCertificateFile
  , parseFakeAvvmOptions
  , parseK
  , parseNewDirectory
  , parseFractionWithDefault
  , parseNetworkMagic
  , parseNewCertificateFile
  , parseNewSigningKeyFile
  , parseNewTxFile
  , parseNewVerificationKeyFile
  , parseProtocolMagicId
  , parseProtocolMagic
  , parseRequiresNetworkMagic
  , parseTxFile
  , parseUTCTime
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Options.Applicative as OA

import           Cardano.CLI.Byron.Parsers (ByronCommand (..))
import           Cardano.CLI.Delegation
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Ops (CardanoEra(..))
import           Cardano.CLI.Shelley.Parsers (ShelleyCommand (..))
import           Cardano.CLI.Tx

import           Cardano.Common.Parsers

import           Cardano.Binary (Annotated(..))
import           Cardano.Chain.Common
                   (Address(..), BlockCount(..), Lovelace, NetworkMagic(..),
                   decodeAddressBase58, mkLovelace)
import           Cardano.Chain.Genesis (FakeAvvmOptions(..))
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.Chain.UTxO (TxId, TxIn(..), TxOut(..))
import           Cardano.Config.Types
import           Cardano.Crypto (RequiresNetworkMagic(..), decodeHash)
import           Cardano.Crypto.ProtocolMagic
                   (AProtocolMagic(..), ProtocolMagic
                   , ProtocolMagicId(..))

-- | Sub-commands of 'cardano-cli'.
data ClientCommand
  =
  --- Byron Related Commands ---
    ByronClientCommand ByronCommand

  --- Shelley Related Commands ---
  | ShelleyClientCommand ShelleyCommand

  --- Genesis Related Commands ---
  | Genesis
        NewDirectory
        GenesisParameters
        CardanoEra
  | PrintGenesisHash
        GenesisFile

  --- Key Related Commands ---
  | Keygen
        CardanoEra
        NewSigningKeyFile
        PasswordRequirement
  | ToVerification
        CardanoEra
        SigningKeyFile
        NewVerificationKeyFile

  | PrettySigningKeyPublic
        CardanoEra
        SigningKeyFile

  | MigrateDelegateKeyFrom
        CardanoEra
        -- ^ Old CardanoEra
        SigningKeyFile
        -- ^ Old key
        CardanoEra
        -- ^ New CardanoEra
        NewSigningKeyFile
        -- ^ New Key

  | PrintSigningKeyAddress
        CardanoEra
        NetworkMagic  -- TODO:  consider deprecation in favor of ProtocolMagicId,
                      --        once Byron is out of the picture.
        SigningKeyFile

    --- Delegation Related Commands ---

  | IssueDelegationCertificate
        ConfigYamlFilePath
        EpochNumber
        -- ^ The epoch from which the delegation is valid.
        SigningKeyFile
        -- ^ The issuer of the certificate, who delegates their right to sign blocks.
        VerificationKeyFile
        -- ^ The delegate, who gains the right to sign blocks on behalf of the issuer.
        NewCertificateFile
        -- ^ Filepath of the newly created delegation certificate.
  | CheckDelegation
        ConfigYamlFilePath
        CertificateFile
        VerificationKeyFile
        VerificationKeyFile

  | GetLocalNodeTip
        ConfigYamlFilePath
        (Maybe CLISocketPath)

    -----------------------------------

  | SubmitTx
        TxFile
        -- ^ Filepath of transaction to submit.
        ConfigYamlFilePath
        (Maybe CLISocketPath)

  | SpendGenesisUTxO
        ConfigYamlFilePath
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of genesis UTxO owner.
        Address
        -- ^ Genesis UTxO address.
        (NonEmpty TxOut)
        -- ^ Tx output.
  | SpendUTxO
        ConfigYamlFilePath
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of Tx underwriter.
        (NonEmpty TxIn)
        -- ^ Inputs available for spending to the Tx underwriter's key.
        (NonEmpty TxOut)
        -- ^ Genesis UTxO output Address.

    --- Misc Commands ---

  | DisplayVersion

  | ValidateCBOR
        CBORObject
        -- ^ Type of the CBOR object
        FilePath

  | PrettyPrintCBOR
        FilePath
  deriving Show

parseAddress :: String -> String -> Parser Address
parseAddress opt desc =
  option (cliParseBase58Address <$> auto)
    $ long opt <> metavar "ADDR" <> help desc

parseCardanoEra :: Parser CardanoEra
parseCardanoEra = asum
  [ flag' ByronEraLegacy $
        long "byron-legacy-formats"
     <> help "Byron/cardano-sl formats and compatibility"

  , flag' ByronEra $
        long "byron-formats"
     <> help "Byron era formats and compatibility"

  , flag' ShelleyEra $
        long "shelley-formats"
     <> help "Shelley-era formats and compatibility"

    -- And various hidden compatibility flag aliases:
  , flag' ByronEraLegacy $ hidden <> long "byron-legacy"
  , flag' ShelleyEra     $ hidden <> long "bft"
  , flag' ShelleyEra     $ hidden <> long "praos"
  , flag' ShelleyEra     $ hidden <> long "mock-pbft"
  , flag' ByronEra       $ hidden <> long "real-pbft"
  ]

parseCertificateFile :: String -> String -> Parser CertificateFile
parseCertificateFile opt desc = CertificateFile <$> parseFilePath opt desc

parseFakeAvvmOptions :: Parser FakeAvvmOptions
parseFakeAvvmOptions =
  FakeAvvmOptions
    <$> parseIntegral "avvm-entry-count" "Number of AVVM addresses."
    <*> parseLovelace "avvm-entry-balance" "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
    <$> parseIntegral "k" "The security parameter of the Ouroboros protocol."

parseNewDirectory :: String -> String -> Parser NewDirectory
parseNewDirectory opt desc = NewDirectory <$> parseFilePath opt desc

parseFractionWithDefault
  :: String
  -> String
  -> Double
  -> Parser Rational
parseFractionWithDefault optname desc w =
  toRational <$> ( option readDouble
                 $ long optname
                <> metavar "DOUBLE"
                <> help desc
                <> value w
                )

parseNetworkMagic :: Parser NetworkMagic
parseNetworkMagic =
  asum [ flag' NetworkMainOrStage $ mconcat
           [ long "main-or-staging"
           , help ""
           ]
       , option (fmap NetworkTestnet auto)
           $ long "testnet-magic"
             <> metavar "MAGIC"
             <> help "The testnet network magic, decibal"
       ]

parseNewCertificateFile :: String -> Parser NewCertificateFile
parseNewCertificateFile opt =
  NewCertificateFile
    <$> parseFilePath opt "Non-existent file to write the certificate to."

parseNewSigningKeyFile :: String -> Parser NewSigningKeyFile
parseNewSigningKeyFile opt =
  NewSigningKeyFile
    <$> parseFilePath opt "Non-existent file to write the signing key to."

parseNewTxFile :: String -> Parser NewTxFile
parseNewTxFile opt =
  NewTxFile
    <$> parseFilePath opt "Non-existent file to write the signed transaction to."

parseNewVerificationKeyFile :: String -> Parser NewVerificationKeyFile
parseNewVerificationKeyFile opt =
  NewVerificationKeyFile
    <$> parseFilePath opt "Non-existent file to write the verification key to."

parseProtocolMagicId :: String -> Parser ProtocolMagicId
parseProtocolMagicId arg =
  ProtocolMagicId
    <$> parseIntegral arg "The magic number unique to any instance of Cardano."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated ()
    <$> parseProtocolMagicId "protocol-magic"

parseRequiresNetworkMagic :: Parser RequiresNetworkMagic
parseRequiresNetworkMagic =
  flag RequiresNoMagic RequiresMagic
    ( long "require-network-magic"
        <> help "Require network magic in transactions."
        <> hidden
    )

parseTxFile :: String -> Parser TxFile
parseTxFile opt =
  TxFile
    <$> parseFilePath opt "File containing the signed transaction."

parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
  option (posixSecondsToUTCTime . fromInteger <$> auto)
    $ long optname <> metavar "POSIXSECONDS" <> help desc


-- | See the rationale for cliParseBase58Address.
cliParseLovelace :: Word64 -> Lovelace
cliParseLovelace =
  either (panic . ("Bad Lovelace value: " <>) . show) identity
  . mkLovelace

-- | Here, we hope to get away with the usage of 'error' in a pure expression,
--   because the CLI-originated values are either used, in which case the error is
--   unavoidable rather early in the CLI tooling scenario (and especially so, if
--   the relevant command ADT constructor is strict, like with ClientCommand), or
--   they are ignored, in which case they are arguably irrelevant.
--   And we're getting a correct-by-construction value that doesn't need to be
--   scrutinised later, so that's an abstraction benefit as well.
cliParseBase58Address :: Text -> Address
cliParseBase58Address =
  either (panic . ("Bad Base58 address: " <>) . show) identity
  . decodeAddressBase58


-- | See the rationale for cliParseBase58Address.
cliParseTxId :: String -> TxId
cliParseTxId =
  either (panic . ("Bad Lovelace value: " <>) . show) identity
  . decodeHash . Text.pack
