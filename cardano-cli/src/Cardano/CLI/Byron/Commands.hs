{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Byron.Commands
  ( ByronCommand (..)
  , NodeCmd (..)
  , VerificationKeyFile (..)
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
    NodeCmd NodeCmd

  --- Genesis Related Commands ---
  | Genesis
        NewDirectory
        GenesisParameters

  | PrintGenesisHash
        (GenesisFile 'In)

  --- Key Related Commands ---
  | Keygen
        (NewSigningKeyFile 'Out)

  | ToVerification
        ByronKeyFormat
        (SigningKeyFile 'In)
        (NewVerificationKeyFile 'Out)

  | PrettySigningKeyPublic
        ByronKeyFormat
        (SigningKeyFile 'In)

  | MigrateDelegateKeyFrom
        (SigningKeyFile 'In)
        -- ^ Old key
        (NewSigningKeyFile 'Out)
        -- ^ New Key

  | PrintSigningKeyAddress
        ByronKeyFormat
        NetworkId
        (SigningKeyFile 'In)

  | GetLocalNodeTip
        NetworkId

    -----------------------------------

  | SubmitTx
        NetworkId
        (TxFile 'In)
        -- ^ Filepath of transaction to submit.

  | SpendGenesisUTxO
        (GenesisFile 'In)
        NetworkId
        ByronKeyFormat
        (NewTxFile 'Out)
        -- ^ Filepath of the newly created transaction.
        (SigningKeyFile 'In)
        -- ^ Signing key of genesis UTxO owner.
        (Address ByronAddr)
        -- ^ Genesis UTxO address.
        [TxOut CtxTx ByronEra]
        -- ^ Tx output.
  | SpendUTxO
        NetworkId
        ByronKeyFormat
        (NewTxFile 'Out)
        -- ^ Filepath of the newly created transaction.
        (SigningKeyFile 'In)
        -- ^ Signing key of Tx underwriter.
        [TxIn]
        -- ^ Inputs available for spending to the Tx underwriter's key.
        [TxOut CtxTx ByronEra]
        -- ^ Genesis UTxO output Address.

  | GetTxId (TxFile 'In)

    --- Misc Commands ---

  | ValidateCBOR
        CBORObject
        -- ^ Type of the CBOR object
        (File 'In)

  | PrettyPrintCBOR
        (File 'In)
  deriving Show


data NodeCmd = CreateVote
               NetworkId
               (SigningKeyFile 'In)
               (File 'In) -- filepath to update proposal
               Bool
               (File 'Out)
             | UpdateProposal
               NetworkId
               (SigningKeyFile 'In)
               ProtocolVersion
               SoftwareVersion
               SystemTag
               InstallerHash
               (File 'Out)
               ByronProtocolParametersUpdate
             | SubmitUpdateProposal
               NetworkId
               (File 'In)
               -- ^ Update proposal filepath.
             | SubmitVote
               NetworkId
               (File 'In)
               -- ^ Vote filepath.
              deriving Show


newtype NewCertificateFile
  = NewCertificateFile { nFp :: FilePath }
  deriving (Eq, Show, IsString)
