{-# LANGUAGE DataKinds #-}

module Testnet.Process.Cli.Keys
  ( cliAddressKeyGen
  , cliNodeKeyGen
  , cliNodeKeyGenVrf
  , cliNodeKeyGenKes
  , cliStakeAddressKeyGen
  , execCliStdoutToJson

  , OperatorCounter

  , ByronDelegationKey
  , ByronDelegationCert

  , cliKeyGen

  , cliByronSigningKeyAddress
  ) where

import           Cardano.Api (ByronAddr, ByronKeyLegacy, File (..), FileDirection (..), StakeKey)
import           Cardano.Api.Shelley (KesKey, StakePoolKey)

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           System.FilePath.Posix

import           Testnet.Process.Run
import           Testnet.Types hiding (testnetMagic)

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H (writeFile)

cliAddressKeyGen :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => KeyPair PaymentKey
  -> m ()
cliAddressKeyGen = GHC.withFrozenCallStack $ shelleyKeyGen "address" "key-gen"

cliStakeAddressKeyGen :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => KeyPair StakeKey
  -> m ()
cliStakeAddressKeyGen = GHC.withFrozenCallStack $ shelleyKeyGen "stake-address" "key-gen"

cliNodeKeyGenVrf :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => KeyPair VrfKey
  -> m ()
cliNodeKeyGenVrf = GHC.withFrozenCallStack $ shelleyKeyGen "node" "key-gen-VRF"

cliNodeKeyGenKes :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => KeyPair KesKey
  -> m ()
cliNodeKeyGenKes = GHC.withFrozenCallStack $ shelleyKeyGen "node" "key-gen-KES"

shelleyKeyGen :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => String -- ^ command
  -> String -- ^ sub command
  -> KeyPair k
  -> m ()
shelleyKeyGen command subCommand keyPair =
  GHC.withFrozenCallStack $
    execCli_
        [ command, subCommand
        , "--verification-key-file", verificationKeyFp keyPair
        , "--signing-key-file", signingKeyFp keyPair
        ]

cliNodeKeyGen
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => KeyPair StakePoolKey
  -> File OperatorCounter Out
  -> m ()
cliNodeKeyGen keyPair (File counterPath) =
  execCli_
    [ "node", "key-gen"
    , "--cold-verification-key-file", verificationKeyFp keyPair
    , "--cold-signing-key-file", signingKeyFp keyPair
    , "--operational-certificate-issue-counter-file", counterPath
    ]

-- | The 'OperatorCounter'
data OperatorCounter

-- | Tag a 'File' that holds a 'ByronDelegationKey'.
data ByronDelegationKey

-- | Tag a 'File' that holds a 'ByronDelegationKey'.
data ByronDelegationCert

type TmpDir = FilePath

-- Byron
cliKeyGen :: TmpDir -> FilePath -> H.Integration (File ByronKeyLegacy Out)
cliKeyGen tmp key = do
  let keyPath = tmp </> key
  execCli_
      [ "keygen"
      , "--secret", keyPath
      ]
  return $ File keyPath

cliByronSigningKeyAddress
  :: TmpDir
  -> Int
  -> File ByronKeyLegacy In
  -> FilePath
  -> H.Integration (File ByronAddr Out)
cliByronSigningKeyAddress tmp testnetMagic (File key) destPath = do
  let addrPath = tmp </> destPath
  addr <- execCli
      [ "signing-key-address"
      , "--testnet-magic", show testnetMagic
      , "--secret", tmp </> key
      ]
  H.writeFile addrPath addr
  return $ File addrPath

