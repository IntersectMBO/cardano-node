module Testnet.Util.Cli
  ( cliAddressKeyGen
  , cliNodeKeyGen
  , cliNodeKeyGenVrf
  , cliNodeKeyGenKes
  , cliStakeAddressKeyGen
  , Comment (..)
  , KeyNames (..)

  , fakeItH
  , fakeIt
  , TmpDir
  , TestnetMagic
  , File (..)
  , Address
  , Operator -- == Cold Keys
  , Kes
  , StakeAddress
  , Vrf

  , VKey
  , SKey

  , OperatorCounter
  , ByronKey
  , ByronAddress

  , ByronDelegationKey
  , ByronDelegationCert

  , getVKeyPath
  , getSKeyPath

  , makeFilePath
  , cliKeyGen

  , cliSigningKeyAddress
  ) where

import           Prelude

import           Data.String
import           Control.Monad
import           System.FilePath.Posix

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H (writeFile)
import           Hedgehog
import           Hedgehog.Gen

import           Testnet.Util.Process

-- TODO: check no whitespace , slash etc allowed
-- TODO: use Comment instead of Filepath
newtype Comment = Comment { unComment :: String}
  deriving (Show, Eq, Ord )

instance IsString Comment where fromString a = Comment a

instance Semigroup Comment where
  Comment a <> Comment b = Comment $ a <> "-" <> b

-- Could also use IO (timestamp).
makeFilePath :: MonadGen m => FilePath -> Comment -> FilePath -> m FilePath
makeFilePath path comment suffix = do
  almostUnique <- Comment <$> replicateM 4 hexit
  return $ path </> unComment ( comment <> almostUnique) <.> suffix

data KeyNames = KeyNames
  { verificationKeyFile :: FilePath
  , signingKeyFile :: FilePath
  }

type KeyGen a = H.Integration (File (a VKey), File (a SKey))

-- cliAddressKeyGen :: FilePath -> Comment -> KeyGen
cliAddressKeyGen :: TmpDir -> KeyNames -> KeyGen Address
cliAddressKeyGen = shelleyKeyGen "address" "key-gen"

cliStakeAddressKeyGen :: TmpDir -> KeyNames -> KeyGen StakeAddress
cliStakeAddressKeyGen = shelleyKeyGen "stake-address" "key-gen"

cliNodeKeyGenVrf :: TmpDir -> KeyNames -> KeyGen Vrf
cliNodeKeyGenVrf = shelleyKeyGen "node" "key-gen-VRF"

cliNodeKeyGenKes :: TmpDir -> KeyNames -> KeyGen Kes
cliNodeKeyGenKes = shelleyKeyGen "node" "key-gen-KES"

shelleyKeyGen :: String -> String -> TmpDir -> KeyNames -> KeyGen x
shelleyKeyGen command subCommand tmpDir keyNames = do
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
  -> H.Integration (File (Operator VKey), File (Operator SKey), File OperatorCounter)
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

data Address key
data Kes key
data Operator key
data StakeAddress key
data Vrf key

data VKey
data SKey

data OperatorCounter
data ByronKey
data ByronAddress
data ByronDelegationKey
data ByronDelegationCert

type TmpDir = FilePath
type TestnetMagic = Int

newtype File a = File {unFile :: FilePath}
  deriving (Show, Eq)

-- This is the back-door for creating File resources in parts of
-- code that have not been refactored.
-- When all uses of fakeItH are removed the cleanup is done.
fakeItH :: FilePath -> H.Integration (File a)
fakeItH filePath = do
  return $ File filePath
{-
  TODO: check that the file actually exists.
  TODO: Is there a simple function in the test framework for that ?
-}

fakeIt :: FilePath -> File a
fakeIt = File

getVKeyPath ::  (File (a VKey), File (a SKey)) -> FilePath
getVKeyPath = unFile . fst

getSKeyPath ::  (File (a VKey), File (a SKey)) -> FilePath
getSKeyPath = unFile . snd

-- Byron
cliKeyGen :: TmpDir -> FilePath -> H.Integration (File ByronKey)
cliKeyGen tmp key = do
  let keyPath = tmp </> key
  execCli_
      [ "keygen"
      , "--secret", keyPath
      ]
  return $ File keyPath

cliSigningKeyAddress
  :: TmpDir
  -> Int
  -> File ByronKey
  -> FilePath
  -> H.Integration (File ByronAddress)
cliSigningKeyAddress tmp testnetMagic (File key) destPath = do
  let addrPath = tmp </> destPath
  addr <- execCli
      [ "signing-key-address"
      , "--testnet-magic", show testnetMagic
      , "--secret", tmp </> key
      ]
  H.writeFile addrPath addr
  return $ File addrPath
