{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Delegation
  ( CertificateFile(..)
  , NewCertificateFile(..)
  , issueByronGenesisDelegation
  , checkByronGenesisDelegation
  )
where

import           Cardano.Prelude hiding (option, show, trace)
import           Test.Cardano.Prelude (canonicalDecodePretty)

import qualified Data.ByteString.Lazy as LB
import qualified Formatting as F

import           Cardano.Binary (Annotated(..), serialize')
import qualified Cardano.Chain.Delegation as Dlg
import           Cardano.Chain.Slotting (EpochNumber)
import qualified Cardano.Crypto as Crypto
import           Cardano.Crypto (ProtocolMagicId)

import           Cardano.CLI.Ops


newtype CertificateFile =
  CertificateFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewCertificateFile =
  NewCertificateFile { nFp :: FilePath }
  deriving (Eq, Ord, Show, IsString)


-- TODO:  we need to support password-protected secrets.
-- | Issue a certificate for genesis delegation to a delegate key, signed by the
--   issuer key, for a given protocol magic and coming into effect at given epoch.
issueByronGenesisDelegation
  :: ProtocolMagicId
  -> EpochNumber
  -> Crypto.SigningKey
  -> Crypto.VerificationKey
  -> Dlg.Certificate
issueByronGenesisDelegation magic epoch issuerSK delegateVK =
  Dlg.signCertificate magic delegateVK epoch $
  Crypto.noPassSafeSigner issuerSK

-- | Verify that a certificate signifies genesis delegation by assumed genesis key
--   to a delegate key, for a given protocol magic.
--   If certificate fails validation, throw an error.
checkByronGenesisDelegation
  :: CertificateFile
  -> ProtocolMagicId
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey
  -> IO ()
checkByronGenesisDelegation (CertificateFile certF) magic issuerVK' delegateVK'  = do
  certBS <- LB.readFile certF
  cert :: CC.Dlg.Certificate <- case canonicalDecodePretty certBS of
    Left e -> throwIO $ DlgCertificateDeserialisationFailed certF e
    Right x -> pure x

  unless (null issues) $
    throwIO $ CertificateValidationErrors certF issues

  where
    issues =
      [ f("Certificate does not have a valid signature.")
      | not (CC.Dlg.isValid magic' cert') ] <>

      [ f("Certificate issuer ".vk." doesn't match expected: ".vk)
        (CC.Dlg.issuerVK   cert)   issuerVK'
      |  CC.Dlg.issuerVK   cert /= issuerVK' ] <>

      [ f("Certificate delegate ".vk." doesn't match expected: ".vk)
        (CC.Dlg.delegateVK cert)   delegateVK'
      |  CC.Dlg.delegateVK cert /= delegateVK' ]
    magic' = Annotated magic (serialize' magic)
    cert' = cert { CC.Dlg.aEpoch = Annotated epoch (serialize' epoch) }
    vk :: F.Format r (Crypto.VerificationKey -> r)
    vk = Crypto.fullVerificationKeyF
    epoch = unAnnotated $ CC.Dlg.aEpoch cert
    f :: F.Format Text a -> a
    f = F.sformat
