{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Byron.Commands
  ( ByronCommand (..)
  , NodeCmd (..)
  , VerificationKeyFile
  , NewVerificationKeyFile (..)
  , CertificateFile (..)
  , NewCertificateFile (..)
  ) where

import           Data.String (IsString)

import           Cardano.Chain.Update (InstallerHash (..), ProtocolVersion (..),
                   SoftwareVersion (..), SystemTag (..))

import           Cardano.Api hiding (GenesisParameters)
import           Cardano.Api.Byron hiding (GenesisParameters)

import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Types

import           Cardano.CLI.Shelley.Commands (ByronKeyFormat)

data ByronCommand =

  --- Node Related Commands ---
    NodeCmd
        NodeCmd

  --- Genesis Related Commands ---
  | Genesis
        NewDirectory
        GenesisParameters

  | PrintGenesisHash
        GenesisFile

  --- Key Related Commands ---
  | Keygen
        NewSigningKeyFile

  | ToVerification
        ByronKeyFormat
        (SigningKeyFile In)
        NewVerificationKeyFile

  | PrettySigningKeyPublic
        ByronKeyFormat
        (SigningKeyFile In)

  | MigrateDelegateKeyFrom
        (SigningKeyFile In)
        -- ^ Old key
        NewSigningKeyFile
        -- ^ New Key

  | PrintSigningKeyAddress
        ByronKeyFormat
        NetworkId
        (SigningKeyFile In)

  | GetLocalNodeTip
        SocketPath
        NetworkId

    -----------------------------------

  | SubmitTx
        SocketPath
        NetworkId
        (TxFile In)
        -- ^ Filepath of transaction to submit.

  | SpendGenesisUTxO
        GenesisFile
        NetworkId
        ByronKeyFormat
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        (SigningKeyFile In)
        -- ^ Signing key of genesis UTxO owner.
        (Address ByronAddr)
        -- ^ Genesis UTxO address.
        [TxOut CtxTx ByronEra]
        -- ^ Tx output.
  | SpendUTxO
        NetworkId
        ByronKeyFormat
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        (SigningKeyFile In)
        -- ^ Signing key of Tx underwriter.
        [TxIn]
        -- ^ Inputs available for spending to the Tx underwriter's key.
        [TxOut CtxTx ByronEra]
        -- ^ Genesis UTxO output Address.

  | GetTxId (TxFile In)

    --- Misc Commands ---

  | ValidateCBOR
        CBORObject
        -- ^ Type of the CBOR object
        FilePath

  | PrettyPrintCBOR
        FilePath
  deriving Show


data NodeCmd =
    CreateVote
      NetworkId
      (SigningKeyFile In)
      FilePath -- ^ filepath to update proposal
      Bool
      FilePath
  | UpdateProposal
      NetworkId
      (SigningKeyFile In)
      ProtocolVersion
      SoftwareVersion
      SystemTag
      InstallerHash
      FilePath
      ByronProtocolParametersUpdate
  | SubmitUpdateProposal
      SocketPath
      NetworkId
      FilePath -- ^ Update proposal filepath.
  | SubmitVote
      SocketPath
      NetworkId
      FilePath -- ^ Vote filepath.
  deriving Show

newtype NewCertificateFile
  = NewCertificateFile { nFp :: FilePath }
  deriving (Eq, Show, IsString)
