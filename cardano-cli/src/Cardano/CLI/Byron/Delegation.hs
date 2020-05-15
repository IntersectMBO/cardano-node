{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Delegation
  ( ByronDelegationError(..)
  , checkByronGenesisDelegation
  , issueByronGenesisDelegation
  , serialiseDelegationCert
  , serialiseDelegateKey
  )
where

import           Cardano.Prelude hiding (option, show, trace)

import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Trans.Except.Extra (left)
import qualified Data.ByteString.Lazy as LB
import           Formatting (Format, sformat)
import qualified Text.JSON.Canonical as CanonicalJSON

import           Cardano.Binary (Annotated(..), serialize')
import qualified Cardano.Chain.Delegation as Dlg
import           Cardano.Chain.Slotting (EpochNumber)
import           Cardano.CLI.Helpers (HelpersError(..), serialiseSigningKey)
import qualified Cardano.CLI.Legacy.Byron as Legacy
import           Cardano.Config.Protocol (CardanoEra(..))
import           Cardano.Config.Types (CertificateFile (..))
import qualified Cardano.Crypto as Crypto
import           Cardano.Crypto (ProtocolMagicId, SigningKey)


data ByronDelegationError
  = CertificateValidationErrors !FilePath ![Text]
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | HelpersError !HelpersError
  deriving Show


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
  -> ExceptT ByronDelegationError IO ()
checkByronGenesisDelegation (CertificateFile certF) magic issuer delegate = do
  ecert <- liftIO $ canonicalDecodePretty <$> LB.readFile certF
  case ecert of
    Left e -> left $ DlgCertificateDeserialisationFailed certF e
    Right (cert :: Dlg.Certificate) -> do
      let issues = checkDlgCert cert magic issuer delegate
      unless (null issues) $
        left $ CertificateValidationErrors certF issues

checkDlgCert
  :: Dlg.ACertificate a
  -> ProtocolMagicId
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey -> [Text]
checkDlgCert cert magic issuerVK' delegateVK' =
  mconcat $
  [ [ sformat ("Certificate does not have a valid signature.")
      | not (Dlg.isValid magic' cert')
    ]
  , [ sformat ("Certificate issuer ".vkF." doesn't match expected: ".vkF)
      ( Dlg.issuerVK cert) issuerVK'
      | Dlg.issuerVK cert /= issuerVK'
    ]
  , [ sformat ("Certificate delegate ".vkF." doesn't match expected: ".vkF)
      ( Dlg.delegateVK cert) delegateVK'
      | Dlg.delegateVK cert /= delegateVK'
    ]
  ]
  where
    magic' :: Annotated ProtocolMagicId ByteString
    magic' = Annotated magic (serialize' magic)

    epoch :: EpochNumber
    epoch = unAnnotated $ Dlg.aEpoch cert

    cert' :: Dlg.ACertificate ByteString
    cert' = cert { Dlg.aEpoch = Annotated epoch (serialize' epoch)
                 , Dlg.annotation = serialize' (void cert')
                 }

    vkF :: forall r. Format r (Crypto.VerificationKey -> r)
    vkF = Crypto.fullVerificationKeyF


serialiseDelegationCert :: CanonicalJSON.ToJSON Identity a
                        => CardanoEra -> a -> Either ByronDelegationError LB.ByteString
serialiseDelegationCert ByronEraLegacy dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert ByronEra       dlgCert = pure $ canonicalEncodePretty dlgCert
serialiseDelegationCert ShelleyEra     _       = Left . HelpersError $ CardanoEraNotSupportedFail ShelleyEra

serialiseDelegateKey :: CardanoEra -> SigningKey -> Either ByronDelegationError LB.ByteString
serialiseDelegateKey ByronEraLegacy sk = pure
                                       . toLazyByteString
                                       . Legacy.encodeLegacyDelegateKey
                                       $ Legacy.LegacyDelegateKey sk
serialiseDelegateKey ByronEra  sk = first HelpersError $ serialiseSigningKey ByronEra sk
serialiseDelegateKey ShelleyEra _ = Left . HelpersError $ CardanoEraNotSupportedFail ShelleyEra
