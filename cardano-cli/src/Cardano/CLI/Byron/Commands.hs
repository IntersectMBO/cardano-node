{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Byron.Commands
  ( ByronCommand (..)
  , NodeCmd (..)
  , VerificationKeyFile (..)
  , NewVerificationKeyFile (..)
  , CertificateFile (..)
  , NewCertificateFile (..)
  ) where

import           Cardano.Prelude

import           Cardano.Chain.Update (InstallerHash (..), ProtocolVersion (..),
                     SoftwareVersion (..), SystemTag (..))

import           Cardano.Api (NetworkId)

import           Cardano.CLI.Byron.UpdateProposal

import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Types

import           Cardano.Chain.Common (Address (..))
import           Cardano.Chain.UTxO (TxIn (..), TxOut (..))
import           Cardano.CLI.Shelley.Commands (ByronKeyFormat)

data ByronCommand =

  --- Node Related Commands ---
    NodeCmd NodeCmd

  --- Genesis Related Commands ---
  | Genesis
        NewDirectory
        GenesisParameters
        ByronKeyFormat
  | PrintGenesisHash
        GenesisFile

  --- Key Related Commands ---
  | Keygen
        ByronKeyFormat
        NewSigningKeyFile
        PasswordRequirement
  | ToVerification
        ByronKeyFormat
        SigningKeyFile
        NewVerificationKeyFile

  | PrettySigningKeyPublic
        ByronKeyFormat
        SigningKeyFile

  | MigrateDelegateKeyFrom
        ByronKeyFormat
        -- ^ Old ByronKeyFormat
        SigningKeyFile
        -- ^ Old key
        ByronKeyFormat
        -- ^ New ByronKeyFormat
        NewSigningKeyFile
        -- ^ New Key

  | PrintSigningKeyAddress
        ByronKeyFormat
        NetworkId
        SigningKeyFile

  | GetLocalNodeTip
        NetworkId

    -----------------------------------

  | SubmitTx
        NetworkId
        TxFile
        -- ^ Filepath of transaction to submit.

  | SpendGenesisUTxO
        GenesisFile
        NetworkId
        ByronKeyFormat
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of genesis UTxO owner.
        Address
        -- ^ Genesis UTxO address.
        (NonEmpty TxOut)
        -- ^ Tx output.
  | SpendUTxO
        NetworkId
        ByronKeyFormat
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of Tx underwriter.
        (NonEmpty TxIn)
        -- ^ Inputs available for spending to the Tx underwriter's key.
        (NonEmpty TxOut)
        -- ^ Genesis UTxO output Address.

  | GetTxId TxFile

    --- Misc Commands ---

  | ValidateCBOR
        CBORObject
        -- ^ Type of the CBOR object
        FilePath

  | PrettyPrintCBOR
        FilePath
  deriving Show


data NodeCmd = CreateVote
               NetworkId
               SigningKeyFile
               FilePath -- filepath to update proposal
               Bool
               FilePath
             | UpdateProposal
               NetworkId
               SigningKeyFile
               ProtocolVersion
               SoftwareVersion
               SystemTag
               InstallerHash
               FilePath
               [ParametersToUpdate]
             | SubmitUpdateProposal
               NetworkId
               FilePath
               -- ^ Update proposal filepath.
             | SubmitVote
               NetworkId
               FilePath
               -- ^ Vote filepath.
              deriving Show


newtype NewCertificateFile
  = NewCertificateFile { nFp :: FilePath }
  deriving (Eq, Show, IsString)
