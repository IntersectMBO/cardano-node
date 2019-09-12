{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Genesis
  ( NewDirectory(..)
  , GenesisFile(..)
  , GenesisParameters(..)
  , mkGenesis
  , readGenesis
  , dumpGenesis
  )
where

import           Prelude (String)
import           Cardano.Prelude hiding (option, show, trace)
import           Test.Cardano.Prelude (canonicalDecodePretty)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.String (IsString)
import           Data.Time (UTCTime)
import           Text.Printf (printf)

import           System.Directory (createDirectory, doesPathExist)
import           System.FilePath ((</>))
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions)
#endif

import qualified Cardano.Chain.Common as Common
import           Cardano.Chain.Delegation hiding (Map, epoch)
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Genesis (GeneratedSecrets(..))
import           Cardano.Crypto (SigningKey (..))
import qualified Cardano.Crypto as Crypto

import           Cardano.CLI.Key
import           Cardano.CLI.Ops


newtype NewDirectory =
  NewDirectory FilePath
  deriving (Eq, Ord, Show, IsString)

newtype GenesisFile =
  GenesisFile FilePath
  deriving (Eq, Ord, Show, IsString)

-- | Parameters required for generation of new genesis.
data GenesisParameters = GenesisParameters
  { gpStartTime :: !UTCTime
  , gpProtocolParamsFile :: !FilePath
  , gpK :: !Common.BlockCount
  , gpProtocolMagic :: !Crypto.ProtocolMagic
  , gpTestnetBalance :: !Genesis.TestnetBalanceOptions
  , gpFakeAvvmOptions :: !Genesis.FakeAvvmOptions
  , gpAvvmBalanceFactor :: !Common.LovelacePortion
  , gpSeed :: !(Maybe Integer)
  }

mkGenesisSpec :: GenesisParameters -> ExceptT CliError IO Genesis.GenesisSpec
mkGenesisSpec gp = do
  protoParamsRaw <- lift . LB.readFile $ gpProtocolParamsFile gp

  protocolParameters <- withExceptT
    (ProtocolParametersParseFailed (gpProtocolParamsFile gp)) $
    ExceptT . pure $ canonicalDecodePretty protoParamsRaw

  -- We're relying on the generator to fake AVVM and delegation.
  genesisDelegation <- withExceptT (DelegationError) $
    Genesis.mkGenesisDelegation []

  seed <- lift . getSeed $ gpSeed gp

  withExceptT GenesisSpecError $
    ExceptT . pure $ Genesis.mkGenesisSpec
      (Genesis.GenesisAvvmBalances mempty)
      genesisDelegation
      protocolParameters
      (gpK gp)
      (gpProtocolMagic gp)
      (mkGenesisInitialiser True seed)

  where
    mkGenesisInitialiser :: Bool -> Integer -> Genesis.GenesisInitializer
    mkGenesisInitialiser useHeavyDlg seed =
      Genesis.GenesisInitializer
      (gpTestnetBalance gp)
      (gpFakeAvvmOptions gp)
      (gpAvvmBalanceFactor gp)
      useHeavyDlg
      seed

    getSeed :: Maybe Integer -> IO Integer
    getSeed (Just x) = pure x
    getSeed Nothing  = Crypto.runSecureRandom . Crypto.randomNumber $ shiftL 1 32

-- | Generate a genesis, for given blockchain start time, protocol parameters,
-- security parameter, protocol magic, testnet balance options, fake AVVM options,
-- AVVM balance factor and seed.  Throw an error in the following cases: if the
-- protocol parameters file can't be read or fails parse, if genesis delegation
-- couldn't be generated, if the parameter-derived genesis specification is wrong,
-- or if the genesis fails generation.
mkGenesis
  :: GenesisParameters
  -> IO (Either CliError (Genesis.GenesisData, Genesis.GeneratedSecrets))
mkGenesis gp = runExceptT $ do
  genesisSpec <- mkGenesisSpec gp

  withExceptT GenesisGenerationError $
    Genesis.generateGenesisData (gpStartTime gp) genesisSpec

-- | Read genesis from a file.  Throw an error if it fails to parse.
readGenesis :: GenesisFile -> IO (Genesis.GenesisData, Genesis.GenesisHash)
readGenesis (GenesisFile fp) = do
  gdE <- runExceptT (Genesis.readGenesisData fp)
  case gdE of
    Left e -> throwIO $ GenesisReadError fp e
    Right x -> pure x

-- | Write out genesis into a directory that must not yet exist.  An error is
-- thrown if the directory already exists, or the genesis has delegate keys that
-- are not delegated to.
dumpGenesis
  :: CLIOps IO
  -> NewDirectory
  -> Genesis.GenesisData
  -> Genesis.GeneratedSecrets
  -> IO ()
dumpGenesis co (NewDirectory outDir) genesisData gs = do
  exists <- doesPathExist outDir
  if exists
    then throwIO $ OutputMustNotAlreadyExist outDir
    else createDirectory outDir

  let genesisJSONFile = outDir <> "/genesis.json"
  LB.writeFile genesisJSONFile =<< coSerialiseGenesis co genesisData

  let dlgCertMap = Genesis.unGenesisDelegation $ Genesis.gdHeavyDelegation genesisData
      isCertForSK :: SigningKey -> Certificate -> Bool
      isCertForSK sk cert = delegateVK cert == Crypto.toVerification sk
      findDelegateCert :: SigningKey -> IO Certificate
      findDelegateCert sk =
        case flip find (Map.elems dlgCertMap) . isCertForSK $ sk of
          Nothing -> throwIO . NoGenesisDelegationForKey
                     . prettyPublicKey . Crypto.toVerification $ sk
          Just x  -> pure x
      wOut :: String -> String -> (a -> IO LB.ByteString) -> [a] -> IO ()
      wOut = writeSecrets outDir
  dlgCerts <- mapM findDelegateCert (gsRichSecrets gs)

  wOut "genesis-keys" "key" (coSerialiseGenesisKey co) (gsDlgIssuersSecrets gs)
  wOut "delegate-keys" "key" (coSerialiseDelegateKey co) (gsRichSecrets gs)
  wOut "poor-keys" "key" (coSerialisePoorKey co) (gsPoorSecrets gs)
  wOut "delegation-cert" "json" (coSerialiseDelegationCert co) dlgCerts
  wOut "avvm-seed" "seed" (pure . LB.fromStrict) (gsFakeAvvmSeeds gs)

writeSecrets :: FilePath -> String -> String -> (a -> IO LB.ByteString) -> [a] -> IO ()
writeSecrets outDir prefix suffix secretOp xs =
  forM_ (zip xs $ [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    secretOp secret >>= LB.writeFile filename
#ifdef UNIX
    setFileMode    filename ownerReadMode
#else
    setPermissions filename (emptyPermissions {readable = True})
#endif
