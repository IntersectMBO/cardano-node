module Testnet.Process.Cli
  ( cliAddressKeyGen
  , cliNodeKeyGen
  , cliNodeKeyGenVrf
  , cliNodeKeyGenKes
  , cliStakeAddressKeyGen
  , execCliStdoutToJson
  , pNetworkId
  , KeyGen
  , KeyNames (..)

  , File (..)

  , VKey
  , SKey

  , OperatorCounter

  , ByronDelegationKey
  , ByronDelegationCert

  , getVKeyPath
  , getSKeyPath

  , cliKeyGen

  , cliByronSigningKeyAddress
  ) where

import           Cardano.Api (ByronAddr, ByronKeyLegacy, PaymentKey, StakeKey, bounded)
import           Cardano.Api.Shelley (KesKey, StakePoolKey, VrfKey)

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.String
import           GHC.Stack (HasCallStack)
import qualified GHC.Stack as GHC
import           Options.Applicative hiding (command)
import qualified Options.Applicative as Opt
import           System.FilePath.Posix

import           Testnet.Process.Run

import           Hedgehog (MonadTest)
import           Hedgehog.Extras (ExecConfig)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H (writeFile)

data KeyNames = KeyNames
  { verificationKeyFile :: FilePath
  , signingKeyFile :: FilePath
  }

type KeyGen a = (File (VKey a), File (SKey a))

cliAddressKeyGen :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpDir
  -> KeyNames
  -> m (KeyGen PaymentKey)
cliAddressKeyGen = GHC.withFrozenCallStack $ shelleyKeyGen "address" "key-gen"

cliStakeAddressKeyGen :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpDir
  -> KeyNames
  -> m (KeyGen StakeKey)
cliStakeAddressKeyGen = GHC.withFrozenCallStack $ shelleyKeyGen "stake-address" "key-gen"

cliNodeKeyGenVrf :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpDir
  -> KeyNames
  -> m (KeyGen VrfKey)
cliNodeKeyGenVrf = GHC.withFrozenCallStack $ shelleyKeyGen "node" "key-gen-VRF"

cliNodeKeyGenKes :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => TmpDir
  -> KeyNames
  -> m (KeyGen KesKey)
cliNodeKeyGenKes = GHC.withFrozenCallStack $ shelleyKeyGen "node" "key-gen-KES"

shelleyKeyGen :: ()
  => (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => String
  -> String
  -> TmpDir
  -> KeyNames
  -> m (KeyGen x)
shelleyKeyGen command subCommand tmpDir keyNames =
  GHC.withFrozenCallStack $ do
    let
      vKeyPath = tmpDir </> verificationKeyFile keyNames
      sKeyPath = tmpDir </> signingKeyFile keyNames
    execCli_
        [ command, subCommand
        , "--verification-key-file", vKeyPath
        , "--signing-key-file", sKeyPath
        ]
    return (File vKeyPath, File sKeyPath)

cliNodeKeyGen
  :: TmpDir
  -> FilePath
  -> FilePath
  -> FilePath
  -> H.Integration (File (VKey StakePoolKey), File (SKey StakePoolKey), File OperatorCounter)
cliNodeKeyGen tmpDir vkey skey counter = do
  let
    vkPath = tmpDir </> vkey
    skPath = tmpDir </> skey
    counterPath = tmpDir </> counter
  execCli_
    [ "node", "key-gen"
    , "--cold-verification-key-file", vkPath
    , "--cold-signing-key-file", skPath
    , "--operational-certificate-issue-counter-file", counterPath
    ]
  return (File vkPath, File skPath, File counterPath)

-- | Call a command of the CLI that returns JSON to stdout. Then parse it,
-- and deserialize it to a Haskell value. Fail the test if a step fails.
-- If your CLI command doesn't support
-- returning JSON to stdout, and needs going through a file instead, probably
-- you should add a similar function to this one.
execCliStdoutToJson :: ()
  => (Aeson.FromJSON a, MonadTest m, MonadCatch m, MonadIO m)
  => ExecConfig -- ^ The configuration with which to call the CLI
  -> [String] -- ^ The CLI command to execute
  -> m a
execCliStdoutToJson execConfig cmd = do
  result <- execCli' execConfig cmd
  H.leftFail $ Aeson.eitherDecode $ Data.String.fromString result

-- | Verification keys
data VKey a

-- | Signing keys
data SKey a

-- | The 'OperatorCounter'
data OperatorCounter

-- | Tag a 'File' that holds a 'ByronDelegationKey'.
data ByronDelegationKey

-- | Tag a 'File' that holds a 'ByronDelegationKey'.
data ByronDelegationCert

type TmpDir = FilePath

newtype File a = File {unFile :: FilePath}
  deriving (Show, Eq)

getVKeyPath ::  (File (VKey a), File (SKey a)) -> FilePath
getVKeyPath = unFile . fst

getSKeyPath ::  (File (VKey a), File (SKey a)) -> FilePath
getSKeyPath = unFile . snd

-- Byron
cliKeyGen :: TmpDir -> FilePath -> H.Integration (File ByronKeyLegacy)
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
  -> File ByronKeyLegacy
  -> FilePath
  -> H.Integration (File ByronAddr)
cliByronSigningKeyAddress tmp testnetMagic (File key) destPath = do
  let addrPath = tmp </> destPath
  addr <- execCli
      [ "signing-key-address"
      , "--testnet-magic", show testnetMagic
      , "--secret", tmp </> key
      ]
  H.writeFile addrPath addr
  return $ File addrPath

pNetworkId :: Parser Int
pNetworkId =
  Opt.option (bounded "TESTNET_MAGIC") $ mconcat
    [ Opt.long "testnet-magic"
    , Opt.metavar "INT"
    , Opt.help "Specify a testnet magic id."
    ]

