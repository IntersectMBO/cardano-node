{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Config.Protocol.Byron
  ( mkConsensusProtocolRealPBFT
  , ByronProtocolInstantiationError(..)
  , renderByronProtocolInstantiationError
  ) where

import           Cardano.Prelude

import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (bimapExceptT, firstExceptT,
                                                   hoistEither, left)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Crypto.Signing as Signing

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)

import           Cardano.Config.Types
                   (NodeConfiguration(..), ProtocolFilepaths(..),
                    GenesisFile (..), Update (..), LastKnownBlockVersion (..))
import           Cardano.Config.Protocol.Types (SomeConsensusProtocol(..))
import           Cardano.TracingOrphanInstances.Byron ()


------------------------------------------------------------------------------
-- Real Byron protocol
--

mkConsensusProtocolRealPBFT
  :: NodeConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ByronProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocolRealPBFT NodeConfiguration {
                              ncGenesisFile = GenesisFile genesisFile,
                              ncReqNetworkMagic,
                              ncPbftSignatureThresh,
                              ncUpdate
                            }
                            files = do
    (genesisData, genesisHash) <-
      firstExceptT (GenesisReadError genesisFile) $
        Genesis.readGenesisData genesisFile

    let genesisConfig :: Genesis.Config
        genesisConfig =
          Genesis.Config {
            Genesis.configGenesisData       = genesisData,
            Genesis.configGenesisHash       = genesisHash,
            Genesis.configReqNetMagic       = ncReqNetworkMagic,
            Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
            --TODO: add config support for the UTxOConfiguration if needed
          }

    optionalLeaderCredentials <-
      case files of
        Just ProtocolFilepaths {
               byronCertFile,
               byronKeyFile
             }  -> readLeaderCredentials
                     genesisConfig
                     byronCertFile
                     byronKeyFile
        Nothing -> return Nothing

    let consensusProtocol :: Consensus.Protocol ByronBlock ProtocolRealPBFT
        consensusProtocol =
          protocolConfigRealPbft
            ncUpdate
            ncPbftSignatureThresh
            genesisConfig
            optionalLeaderCredentials

    return (SomeConsensusProtocol consensusProtocol)


-- | The plumbing to select and convert the appropriate configuration subset
-- for the 'RealPBFT' protocol.
--
protocolConfigRealPbft :: Update
                       -> Maybe Double
                       -> Genesis.Config
                       -> Maybe PBftLeaderCredentials
                       -> Consensus.Protocol ByronBlock ProtocolRealPBFT
protocolConfigRealPbft (Update appName appVer lastKnownBlockVersion)
                       pbftSignatureThresh
                       genesis leaderCredentials =
    Consensus.ProtocolRealPBFT
      genesis
      (PBftSignatureThreshold <$> pbftSignatureThresh)
      (convertProtocolVersion lastKnownBlockVersion)
      (Update.SoftwareVersion appName appVer)
      leaderCredentials
  where
    convertProtocolVersion
      LastKnownBlockVersion {lkbvMajor, lkbvMinor, lkbvAlt} =
      Update.ProtocolVersion lkbvMajor lkbvMinor lkbvAlt


readLeaderCredentials :: Genesis.Config
                      -> Maybe FilePath
                      -> Maybe FilePath
                      -> ExceptT ByronProtocolInstantiationError IO
                                 (Maybe PBftLeaderCredentials)
readLeaderCredentials genesisConfig mDelCertFp mSKeyFp =
  case (mDelCertFp, mSKeyFp) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> left SigningKeyFilepathNotSpecified
    (Nothing, Just _) -> left DelegationCertificateFilepathNotSpecified
    (Just delegCertFile, Just signingKeyFile) -> do

         signingKeyFileBytes <- liftIO $ LB.readFile signingKeyFile
         delegCertFileBytes <- liftIO $ LB.readFile delegCertFile
         signingKey <- firstExceptT (SigningKeyDeserialiseFailure signingKeyFile)
                         . hoistEither
                         $ deserialiseSigningKey signingKeyFileBytes
         delegCert  <- firstExceptT (CanonicalDecodeFailure delegCertFile)
                         . hoistEither
                         $ canonicalDecodePretty delegCertFileBytes

         bimapExceptT PbftError Just
           . hoistEither
           $ mkPBftLeaderCredentials genesisConfig signingKey delegCert

  where
    deserialiseSigningKey :: LB.ByteString
                          -> Either DeserialiseFailure Signing.SigningKey
    deserialiseSigningKey =
        fmap (Signing.SigningKey . snd)
      . deserialiseFromBytes Signing.fromCBORXPrv


------------------------------------------------------------------------------
-- Errors
--

data ByronProtocolInstantiationError =
    CanonicalDecodeFailure !FilePath !Text
  | DelegationCertificateFilepathNotSpecified
  | GenesisConfigurationError !FilePath !Genesis.ConfigurationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | PbftError !PBftLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath !DeserialiseFailure
  | SigningKeyFilepathNotSpecified
  deriving Show


renderByronProtocolInstantiationError :: ByronProtocolInstantiationError -> Text
renderByronProtocolInstantiationError pie =
  case pie of
    CanonicalDecodeFailure fp failure -> "Canonical decode failure in " <> toS fp
                                         <> " Canonical failure: " <> failure
    DelegationCertificateFilepathNotSpecified -> "Delegation certificate filepath not specified"
    --TODO: Implement configuration error render function in cardano-ledger
    GenesisConfigurationError fp genesisConfigError -> "Genesis configuration error in: " <> toS fp
                                                       <> " Error: " <> (T.pack $ show genesisConfigError)
    GenesisReadError fp err ->  "There was an error parsing the genesis file: " <> toS fp
                                <> " Error: " <> (T.pack $ show err)
    -- TODO: Implement PBftLeaderCredentialsError render function in ouroboros-network
    PbftError pbftLeaderCredentialsError -> "PBFT leader credentials error: " <> (T.pack $ show pbftLeaderCredentialsError)
    SigningKeyDeserialiseFailure fp deserialiseFailure -> "Signing key deserialisation error in: " <> toS fp
                                                           <> " Error: " <> (T.pack $ show deserialiseFailure)
    SigningKeyFilepathNotSpecified -> "Signing key filepath not specified"

