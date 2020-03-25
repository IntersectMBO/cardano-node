module Cardano.CLI.Parsers
  ( ClientCommand(..)
  , parseByron
  , parseDelegationRelatedValues
  , parseGenesisParameters
  , parseGenesisRelatedValues
  , parseKeyRelatedValues
  , parseLocalNodeQueryValues
  , parseMiscellaneous
  , parseRequiresNetworkMagic
  , parseShelley
  , parseTxRelatedValues
  ) where

import           Cardano.CLI.Byron.Parsers (parseByronCommands)
import           Cardano.CLI.Common.Parsers
import           Cardano.CLI.Genesis
import           Cardano.CLI.Key
import           Cardano.CLI.Shelley.Parsers (parseShelleyCommands)

import           Cardano.Common.Parsers

import           Cardano.Chain.Common (rationalToLovelacePortion)
import           Cardano.Chain.Genesis (TestnetBalanceOptions(..))
import           Cardano.Chain.Slotting (EpochNumber(..))
import           Cardano.Chain.UTxO (TxIn(..), TxOut(..))
import           Cardano.Config.Types

import           Cardano.Prelude hiding (option)

import           Data.Bifunctor (first, second)
import qualified Data.List.NonEmpty as NE

import           Options.Applicative as OA

import           Prelude (String)

parseByron :: Parser ClientCommand
parseByron =
  subparser $ mconcat
    [ commandGroup "Byron specific commands"
    , metavar "Byron specific commands"
    , command'
        "byron"
        "Byron specific commands"
        $ ByronClientCommand <$> parseByronCommands
    ]

parseCBORObject :: Parser CBORObject
parseCBORObject = asum
  [ flag' CBORBlockByron $
        long "byron-block"
     <> help "The CBOR file is a byron era block"

  , flag' CBORDelegationCertificateByron $
        long "byron-delegation-certificate"
     <> help "The CBOR file is a byron era delegation certificate"

  , flag' CBORTxByron $
        long "byron-tx"
     <> help "The CBOR file is a byron era tx"

  , flag' CBORUpdateProposalByron $
        long "byron-update-proposal"
     <> help "The CBOR file is a byron era update proposal"
  , flag' CBORVoteByron $
        long "byron-vote"
     <> help "The CBOR file is a byron era vote"
  ]

parseDelegationRelatedValues :: Parser ClientCommand
parseDelegationRelatedValues =
  subparser $ mconcat
    [ commandGroup "Delegation related commands"
    , metavar "Delegation related commands"
    , command'
        "issue-delegation-certificate"
        "Create a delegation certificate allowing the\
        \ delegator to sign blocks on behalf of the issuer"
        $ IssueDelegationCertificate
        <$> (ConfigYamlFilePath <$> parseConfigFile)
        <*> ( EpochNumber
                <$> parseIntegral
                      "since-epoch"
                      "The epoch from which the delegation is valid."
              )
        <*> parseSigningKeyFile
              "secret"
              "The issuer of the certificate, who delegates\
              \ their right to sign blocks."
        <*> parseVerificationKeyFile
              "delegate-key"
              "The delegate, who gains the right to sign block."
        <*> parseNewCertificateFile "certificate"
    , command'
        "check-delegation"
        "Verify that a given certificate constitutes a valid\
        \ delegation relationship between keys."
        $ CheckDelegation
            <$> (ConfigYamlFilePath <$> parseConfigFile)
            <*> parseCertificateFile
                  "certificate"
                  "The certificate embodying delegation to verify."
            <*> parseVerificationKeyFile
                  "issuer-key"
                  "The genesis key that supposedly delegates."
            <*> parseVerificationKeyFile
                  "delegate-key"
                  "The operation verification key supposedly delegated to."
      ]


-- | Values required to create genesis.
parseGenesisParameters :: Parser GenesisParameters
parseGenesisParameters =
  GenesisParameters
    <$> parseUTCTime
          "start-time"
          "Start time of the new cluster to be enshrined in the new genesis."
    <*> parseFilePath
          "protocol-parameters-file"
          "JSON file with protocol parameters."
    <*> parseK
    <*> parseProtocolMagic
    <*> parseTestnetBalanceOptions
    <*> parseFakeAvvmOptions
    <*> (rationalToLovelacePortion <$>
         parseFractionWithDefault
          "avvm-balance-factor"
          "AVVM balances will be multiplied by this factor (defaults to 1)."
          1)
    <*> optional
        ( parseIntegral
            "secret-seed"
            "Optionally specify the seed of generation."
        )

parseGenesisRelatedValues :: Parser ClientCommand
parseGenesisRelatedValues =
  subparser $ mconcat
    [ commandGroup "Genesis related commands"
    , metavar "Genesis related commands"
    , command' "genesis" "Create genesis."
      $ Genesis
          <$> parseNewDirectory
              "genesis-output-dir"
              "Non-existent directory where genesis JSON file and secrets shall be placed."
          <*> parseGenesisParameters
          <*> parseCardanoEra
    , command' "print-genesis-hash" "Compute hash of a genesis file."
        $ PrintGenesisHash
            <$> parseGenesisFile "genesis-json"
    ]

-- | Values required to create keys and perform
-- transformation on keys.
parseKeyRelatedValues :: Parser ClientCommand
parseKeyRelatedValues =
  subparser $ mconcat
        [ commandGroup "Key related commands"
        , metavar "Key related commands"
        , command' "keygen" "Generate a signing key."
            $ Keygen
                <$> parseCardanoEra
                <*> parseNewSigningKeyFile "secret"
                <*> parseFlag' GetPassword EmptyPassword
                      "no-password"
                      "Disable password protection."
        , command'
            "to-verification"
            "Extract a verification key in its base64 form."
            $ ToVerification
                <$> parseCardanoEra
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key file to extract the verification part from."
                <*> parseNewVerificationKeyFile "to"
        , command'
            "signing-key-public"
            "Pretty-print a signing key's verification key (not a secret)."
            $ PrettySigningKeyPublic
                <$> parseCardanoEra
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key to pretty-print."
        , command'
            "signing-key-address"
            "Print address of a signing key."
            $ PrintSigningKeyAddress
                <$> parseCardanoEra
                <*> parseNetworkMagic
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key, whose address is to be printed."
        , command'
            "migrate-delegate-key-from"
            "Migrate a delegate key from an older version."
            $ MigrateDelegateKeyFrom
                <$> parseCardanoEra -- Old CardanoEra
                <*> parseSigningKeyFile "from" "Signing key file to migrate."
                <*> parseCardanoEra -- New CardanoEra
                <*> parseNewSigningKeyFile "to"
        ]
parseLocalNodeQueryValues :: Parser ClientCommand
parseLocalNodeQueryValues =
  subparser $ mconcat
        [ commandGroup "Local node related commands"
        , metavar "Local node related commands"
        , command' "get-tip" "Get the tip of your local node's blockchain"
            $ GetLocalNodeTip
                <$> (ConfigYamlFilePath <$> parseConfigFile)
                <*> parseCLISocketPath "Socket of target node"
        ]

parseMiscellaneous :: Parser ClientCommand
parseMiscellaneous = subparser $ mconcat
  [ commandGroup "Miscellaneous commands"
  , metavar "Miscellaneous commands"
  , command'
      "validate-cbor"
      "Validate a CBOR blockchain object."
      $ ValidateCBOR
          <$> parseCBORObject
          <*> parseFilePath "filepath" "Filepath of CBOR file."
  , command'
      "version"
      "Show cardano-cli version"
      $ pure DisplayVersion
  , command'
      "pretty-print-cbor"
      "Pretty print a CBOR file."
      $ PrettyPrintCBOR
          <$> parseFilePath "filepath" "Filepath of CBOR file."
  ]

parseShelley :: Parser ClientCommand
parseShelley =
  subparser $ mconcat
    [ commandGroup "Shelley specific commands"
    , metavar "Shelley specific commands"
    , command'
        "shelley"
        "Shelley specific commands"
        $ ShelleyClientCommand <$> parseShelleyCommands
    ]


parseTestnetBalanceOptions :: Parser TestnetBalanceOptions
parseTestnetBalanceOptions =
  TestnetBalanceOptions
    <$> parseIntegral
          "n-poor-addresses"
          "Number of poor nodes (with small balance)."
    <*> parseIntegral
          "n-delegate-addresses"
          "Number of delegate nodes (with huge balance)."
    <*> parseLovelace
          "total-balance"
          "Total balance owned by these nodes."
    <*> parseFraction
          "delegate-share"
          "Portion of stake owned by all delegates together."

parseTxIn :: Parser TxIn
parseTxIn =
  option
  ( uncurry TxInUtxo
    . first cliParseTxId
    <$> auto
  )
  $ long "txin"
    <> metavar "(TXID,INDEX)"
    <> help "Transaction input is a pair of an UTxO TxId and a zero-based output index."

parseTxOut :: Parser TxOut
parseTxOut =
  option
    ( uncurry TxOut
      . first cliParseBase58Address
      . second cliParseLovelace
      <$> auto
    )
    $ long "txout"
      <> metavar "ADDR:LOVELACE"
      <> help "Specify a transaction output, as a pair of an address and lovelace."

parseTxRelatedValues :: Parser ClientCommand
parseTxRelatedValues =
  subparser $ mconcat
    [ commandGroup "Transaction related commands"
    , metavar "Transaction related commands"
    , command'
        "submit-tx"
        "Submit a raw, signed transaction, in its on-wire representation."
        $ SubmitTx
            <$> parseTxFile "tx"
            <*> (ConfigYamlFilePath <$> parseConfigFile)
            <*> parseCLISocketPath "Socket of target node"
    , command'
        "issue-genesis-utxo-expenditure"
        "Write a file with a signed transaction, spending genesis UTxO."
        $ SpendGenesisUTxO
            <$> (ConfigYamlFilePath <$> parseConfigFile)
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> parseAddress
                  "rich-addr-from"
                  "Tx source: genesis UTxO richman address (non-HD)."
            <*> (NE.fromList <$> some parseTxOut)

    , command'
        "issue-utxo-expenditure"
        "Write a file with a signed transaction, spending normal UTxO."
        $ SpendUTxO
            <$> (ConfigYamlFilePath <$> parseConfigFile)
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> (NE.fromList <$> some parseTxIn)
            <*> (NE.fromList <$> some parseTxOut)
      ]

parseVerificationKeyFile :: String -> String -> Parser VerificationKeyFile
parseVerificationKeyFile opt desc = VerificationKeyFile <$> parseFilePath opt desc
