module Cardano.CLI.Byron.Parsers
  ( ByronCommand(..)
  , NodeCmd(..)
  , parseByronCommands
  , parseHeavyDelThd
  , parseInstallerHash
  , parseMaxBlockSize
  , parseMaxHeaderSize
  , parseMaxTxSize
  , parseMaxProposalSize
  , parseMpcThd
  , parseScriptVersion
  , parseSlotDuration
  , parseSoftforkRuleParam
  , parseSystemTag
  , parseTxFeePolicy
  , parseUpdateProposalThd
  , parseUpdateProposalTTL
  , parseUnlockStakeEpoch
  , parseUpdateVoteThd
  ) where

import           Cardano.Prelude hiding (option)
import           Prelude (String)

import           Data.Bifunctor (first, second)
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Formatting (build, sformat)

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Cardano.Binary (Annotated(..))

import           Cardano.Crypto.Hashing (hashRaw)
import           Cardano.Crypto (RequiresNetworkMagic(..), decodeHash)
import           Cardano.Crypto.ProtocolMagic
                   (AProtocolMagic(..), ProtocolMagic
                   , ProtocolMagicId(..))

import           Cardano.Chain.Slotting
                   (EpochNumber(..), EpochSlots(..), SlotNumber(..))
import           Cardano.Chain.Common
                   (Address(..), decodeAddressBase58,
                    Lovelace, mkLovelace, rationalToLovelacePortion,
                    BlockCount(..), TxFeePolicy(..), TxSizeLinear(..))
import           Cardano.Chain.Update
                   (ApplicationName(..), checkApplicationName,
                    InstallerHash(..), NumSoftwareVersion,
                    ProtocolVersion(..), SoftforkRule(..), SoftwareVersion(..),
                    SystemTag(..), checkSystemTag)
import           Cardano.Chain.Genesis
                   (TestnetBalanceOptions(..), FakeAvvmOptions(..))
import           Cardano.Chain.UTxO (TxId, TxIn(..), TxOut(..))

import qualified Cardano.Api.Typed as Typed
import           Cardano.Config.Types
import           Cardano.Config.Parsers
                   (parseIntegral, parseFraction, parseLovelace, readDouble,
                    parseFilePath,  parseSigningKeyFile,
                    parseGenesisFile, parseFlag')

import           Cardano.CLI.Byron.Commands
import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Byron.UpdateProposal

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper)
              $ mconcat [ progDesc descr ]

parseByronCommands :: Mod CommandFields ByronCommand
parseByronCommands =
  mconcat
    [ parseNode
    , parseGenesisRelatedValues
    , parseKeyRelatedValues
    , parseDelegationRelatedValues
    , parseTxRelatedValues
    , parseLocalNodeQueryValues
    , parseMiscellaneous
    ]


parseNode :: Mod CommandFields ByronCommand
parseNode = mconcat
    [ Opt.command "byron"
        (Opt.info (NodeCmd <$> pNodeCmd) $ Opt.progDesc "Byron node operation commands")
    ]

parseCBORObject :: Parser CBORObject
parseCBORObject = asum
  [ CBORBlockByron <$> option auto
      (  long "byron-block"
      <> (help $  "The CBOR file is a byron era block."
               <> " Enter the number of slots in an epoch. The default value is 21600")
      <> metavar "INT"
      <> value (EpochSlots 21600)
      )

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

parseDelegationRelatedValues :: Mod CommandFields ByronCommand
parseDelegationRelatedValues =
  mconcat
    [ command'
        "issue-delegation-certificate"
        "Create a delegation certificate allowing the\
        \ delegator to sign blocks on behalf of the issuer"
        $ IssueDelegationCertificate
        <$> pNetworkId
        <*> parseCardanoEra
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
            <$> pNetworkId
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

parseGenesisRelatedValues :: Mod CommandFields ByronCommand
parseGenesisRelatedValues =
  mconcat
    [ command' "genesis" "Create genesis."
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
parseKeyRelatedValues :: Mod CommandFields ByronCommand
parseKeyRelatedValues =
    mconcat
        [ command' "keygen" "Generate a signing key."
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
                <*> pNetworkId
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
parseLocalNodeQueryValues :: Mod CommandFields ByronCommand
parseLocalNodeQueryValues =
    mconcat
        [ command' "get-tip" "Get the tip of your local node's blockchain"
            $ GetLocalNodeTip
                <$> pNetworkId
        ]

parseMiscellaneous :: Mod CommandFields ByronCommand
parseMiscellaneous = mconcat
  [ command'
      "validate-cbor"
      "Validate a CBOR blockchain object."
      $ ValidateCBOR
          <$> parseCBORObject
          <*> parseFilePath "filepath" "Filepath of CBOR file."
  , command'
      "pretty-print-cbor"
      "Pretty print a CBOR file."
      $ PrettyPrintCBOR
          <$> parseFilePath "filepath" "Filepath of CBOR file."
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

parseTxRelatedValues :: Mod CommandFields ByronCommand
parseTxRelatedValues =
  mconcat
    [ command'
        "submit-tx"
        "Submit a raw, signed transaction, in its on-wire representation."
        $ SubmitTx
            <$> pNetworkId
            <*> parseTxFile "tx"
    , command'
        "issue-genesis-utxo-expenditure"
        "Write a file with a signed transaction, spending genesis UTxO."
        $ SpendGenesisUTxO
            <$> parseGenesisFile "genesis-json"
            <*> pNetworkId
            <*> parseCardanoEra
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
            <$> pNetworkId
            <*> parseCardanoEra
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> (NE.fromList <$> some parseTxIn)
            <*> (NE.fromList <$> some parseTxOut)
      ]

parseVerificationKeyFile :: String -> String -> Parser VerificationKeyFile
parseVerificationKeyFile opt desc = VerificationKeyFile <$> parseFilePath opt desc

pNodeCmd :: Parser NodeCmd
pNodeCmd =
  Opt.subparser $
    mconcat
      [ Opt.command "create-update-proposal"
          (Opt.info parseByronUpdateProposal $ Opt.progDesc  "Create an update proposal.")

      , Opt.command "create-proposal-vote"
          (Opt.info parseByronVote $ Opt.progDesc "Create an update proposal vote.")

      , Opt.command "submit-update-proposal"
          (Opt.info parseByronUpdateProposalSubmission $ Opt.progDesc "Submit an update proposal.")

      , Opt.command "submit-proposal-vote"
          (Opt.info parseByronVoteSubmission $ Opt.progDesc "Submit a proposal vote.")
      ]

parseByronUpdateProposal :: Parser NodeCmd
parseByronUpdateProposal = do
  UpdateProposal
    <$> pNetworkId
    <*> parseSigningKeyFile "signing-key" "Path to signing key."
    <*> parseProtocolVersion
    <*> parseSoftwareVersion
    <*> parseSystemTag
    <*> parseInstallerHash
    <*> parseFilePath "filepath" "Byron proposal output filepath."
    <*> parseParametersToUpdate

parseByronVoteSubmission :: Parser NodeCmd
parseByronVoteSubmission = do
  SubmitVote
    <$> pNetworkId
    <*> parseFilePath "filepath" "Filepath of Byron update proposal vote."

parseParametersToUpdate :: Parser [ParametersToUpdate]
parseParametersToUpdate =
  catMaybes
    <$> sequenceA
          [ parseScriptVersion
          , parseSlotDuration
          , parseMaxBlockSize
          , parseMaxHeaderSize
          , parseMaxTxSize
          , parseMaxProposalSize
          , parseMpcThd
          , parseHeavyDelThd
          , parseUpdateVoteThd
          , parseUpdateProposalThd
          , parseUpdateProposalTTL
          , parseSoftforkRuleParam
          , parseTxFeePolicy
          , parseUnlockStakeEpoch
          ]

parseByronUpdateProposalSubmission :: Parser NodeCmd
parseByronUpdateProposalSubmission =
  SubmitUpdateProposal
    <$> pNetworkId
    <*> parseFilePath "filepath" "Filepath of Byron update proposal."


parseByronVote :: Parser NodeCmd
parseByronVote =
  CreateVote
    <$> pNetworkId
    <*> (SigningKeyFile <$> parseFilePath "signing-key" "Filepath of signing key.")
    <*> parseFilePath "proposal-filepath" "Filepath of Byron update proposal."
    <*> parseVoteBool
    <*> parseFilePath "output-filepath" "Byron vote output filepath."

--------------------------------------------------------------------------------
-- CLI Parsers
--------------------------------------------------------------------------------

parseScriptVersion :: Parser (Maybe ParametersToUpdate)
parseScriptVersion = optional $
  ScriptVersion <$> option auto
                      ( long "script-version"
                      <> metavar "WORD16"
                      <> help "Proposed script version."
                      )

parseSlotDuration :: Parser (Maybe ParametersToUpdate)
parseSlotDuration = optional $
  SlotDuration <$> option auto
                     ( long "slot-duration"
                     <> metavar "NATURAL"
                     <> help "Proposed slot duration."
                     )

parseSystemTag :: Parser SystemTag
parseSystemTag = option (eitherReader checkSysTag)
                   ( long "system-tag"
                   <> metavar "STRING"
                   <> help "Identify which system (linux, win64, etc) the update proposal is for."
                   )
 where
  checkSysTag :: String -> Either String SystemTag
  checkSysTag name =
    let tag = SystemTag $ toS name
    in case checkSystemTag tag of
         Left err -> Left . toS $ sformat build err
         Right () -> Right tag

parseInstallerHash :: Parser InstallerHash
parseInstallerHash =
  InstallerHash .  hashRaw . C8.pack
    <$> strOption ( long "installer-hash"
                  <> metavar "HASH"
                  <> help "Software hash."
                  )

parseMaxBlockSize :: Parser (Maybe ParametersToUpdate)
parseMaxBlockSize = optional $
  MaxBlockSize <$> option auto
                     ( long "max-block-size"
                     <> metavar "NATURAL"
                     <> help "Proposed max block size."
                     )

parseMaxHeaderSize :: Parser (Maybe ParametersToUpdate)
parseMaxHeaderSize = optional $
  MaxHeaderSize <$> option auto
                      ( long "max-header-size"
                      <> metavar "NATURAL"
                      <> help "Proposed max block header size."
                      )

parseMaxTxSize :: Parser (Maybe ParametersToUpdate)
parseMaxTxSize = optional $
  MaxTxSize <$> option auto
                  ( long "max-tx-size"
                  <> metavar "NATURAL"
                  <> help "Proposed max transaction size."
                  )

parseMaxProposalSize :: Parser (Maybe ParametersToUpdate)
parseMaxProposalSize = optional $
  MaxProposalSize <$> option auto
                        ( long "max-proposal-size"
                        <> metavar "NATURAL"
                        <> help "Proposed max update proposal size."
                        )

parseMpcThd :: Parser (Maybe ParametersToUpdate)
parseMpcThd = optional $
  MpcThd . rationalToLovelacePortion
    <$> parseFraction "max-mpc-thd" "Proposed max mpc threshold."

parseProtocolVersion :: Parser ProtocolVersion
parseProtocolVersion =
  ProtocolVersion <$> (parseWord "protocol-version-major" "Protocol verson major." "WORD16" :: Parser Word16)
                  <*> (parseWord "protocol-version-minor" "Protocol verson minor." "WORD16" :: Parser Word16)
                  <*> (parseWord "protocol-version-alt" "Protocol verson alt." "WORD8" :: Parser Word8)

parseHeavyDelThd :: Parser (Maybe ParametersToUpdate)
parseHeavyDelThd = optional $
  HeavyDelThd . rationalToLovelacePortion
    <$> parseFraction "heavy-del-thd" "Proposed heavy delegation threshold."

parseUpdateVoteThd :: Parser (Maybe ParametersToUpdate)
parseUpdateVoteThd = optional $
  UpdateVoteThd . rationalToLovelacePortion
    <$> parseFraction "update-vote-thd" "Propose update vote threshold."

parseUpdateProposalThd :: Parser (Maybe ParametersToUpdate)
parseUpdateProposalThd = optional $
  UpdateProposalThd . rationalToLovelacePortion
    <$> parseFraction "update-proposal-thd" "Propose update proposal threshold."

parseUpdateProposalTTL :: Parser (Maybe ParametersToUpdate)
parseUpdateProposalTTL = optional $
  UpdateProposalTTL . SlotNumber
    <$> option auto
          ( long "time-to-live"
          <> metavar "WORD64"
          <> help "Proposed time for an update proposal to live."
          )

parseSoftforkRuleParam :: Parser (Maybe ParametersToUpdate)
parseSoftforkRuleParam = optional $
  SoftforkRuleParam
    <$> (SoftforkRule
           <$> (rationalToLovelacePortion <$> parseFraction "softfork-init-thd" "Propose initial threshold (right after proposal is confirmed).")
           <*> (rationalToLovelacePortion <$> parseFraction "softfork-min-thd" "Propose minimum threshold (threshold can't be less than this).")
           <*> (rationalToLovelacePortion <$> parseFraction "softfork-thd-dec" "Propose threshold decrement (threshold will decrease by this amount after each epoch).")
        )

parseSoftwareVersion :: Parser SoftwareVersion
parseSoftwareVersion =
  SoftwareVersion <$> parseApplicationName <*> parseNumSoftwareVersion

parseApplicationName :: Parser ApplicationName
parseApplicationName = option (eitherReader checkAppNameLength)
       (  long "application-name"
       <> metavar "STRING"
       <> help "The name of the application."
       )
 where
  checkAppNameLength :: String -> Either String ApplicationName
  checkAppNameLength name =
    let appName = ApplicationName $ toS name
    in case checkApplicationName appName of
         Left err -> Left . toS $ sformat build err
         Right () -> Right appName

parseNumSoftwareVersion :: Parser NumSoftwareVersion
parseNumSoftwareVersion =
  parseWord
    "software-version-num"
    "Numeric software version associated with application name."
    "WORD32"

parseTxFeePolicy :: Parser (Maybe ParametersToUpdate)
parseTxFeePolicy = optional $
  TxFeePolicy . TxFeePolicyTxSizeLinear
    <$> ( TxSizeLinear <$> parseLovelace "tx-fee-a-constant" "Propose the constant a for txfee = a + b*s where s is the size."
                       <*> parseFraction "tx-fee-b-constant" "Propose the constant b for txfee = a + b*s where s is the size."
        )

parseVoteBool :: Parser Bool
parseVoteBool = flag' True (long "vote-yes" <> help "Vote yes with respect to an update proposal.")
            <|> flag' False (long "vote-no" <> help "Vote no with respect to an update proposal.")

parseUnlockStakeEpoch :: Parser (Maybe ParametersToUpdate)
parseUnlockStakeEpoch = optional $
  UnlockStakeEpoch . EpochNumber
    <$> option auto
      ( long "unlock-stake-epoch"
      <> metavar "WORD64"
      <> help "Proposed epoch to unlock all stake."
      )


parseWord :: Integral a => String -> String -> String -> Parser a
parseWord optname desc metvar = option (fromInteger <$> auto)
  $ long optname <> metavar metvar <> help desc



parseAddress :: String -> String -> Parser Address
parseAddress opt desc =
  option (cliParseBase58Address <$> str)
    $ long opt <> metavar "ADDR" <> help desc

parseCardanoEra :: Parser CardanoEra
parseCardanoEra = asum
  [ flag' ByronEraLegacy $
        long "byron-legacy-formats"
     <> help "Byron/cardano-sl formats and compatibility"

  , flag' ByronEra $
        long "byron-formats"
     <> help "Byron era formats and compatibility"

    -- And hidden compatibility flag aliases:
  , flag' ByronEraLegacy $ hidden <> long "byron-legacy"
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

pNetworkId :: Parser Typed.NetworkId
pNetworkId =
  pMainnet' <|> fmap Typed.Testnet pTestnetMagic
 where
   pMainnet' :: Parser Typed.NetworkId
   pMainnet' =
    Opt.flag' Typed.Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

pTestnetMagic :: Parser Typed.NetworkMagic
pTestnetMagic =
  Typed.NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

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
