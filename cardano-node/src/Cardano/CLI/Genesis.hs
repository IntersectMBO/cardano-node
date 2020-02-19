{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.CLI.Genesis
  ( NewDirectory(..)
  , GenesisParameters(..)
  , dumpGenesis
  , mkGenesis
  )
where

import           Prelude (String, show)
import           Cardano.Prelude hiding (option, show, trace)
import           Test.Cardano.Prelude (canonicalDecodePretty)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (hoistEither,left, right)
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
import           Cardano.Config.Protocol (Protocol(..))
import           Cardano.Crypto (SigningKey (..))
import qualified Cardano.Crypto as Crypto

import           Cardano.CLI.Key
import           Cardano.CLI.Ops


newtype NewDirectory =
  NewDirectory FilePath
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
  } deriving Show

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
      (Common.lovelacePortionToRational (gpAvvmBalanceFactor gp))
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
  -> ExceptT CliError IO (Genesis.GenesisData, Genesis.GeneratedSecrets)
mkGenesis gp = do
  genesisSpec <- mkGenesisSpec gp

  withExceptT GenesisGenerationError $
    Genesis.generateGenesisData (gpStartTime gp) genesisSpec

-- | Write out genesis into a directory that must not yet exist.  An error is
-- thrown if the directory already exists, or the genesis has delegate keys that
-- are not delegated to.
dumpGenesis
  :: Protocol
  -> NewDirectory
  -> Genesis.GenesisData
  -> Genesis.GeneratedSecrets
  -> ExceptT CliError IO ()
dumpGenesis ptcl (NewDirectory outDir) genesisData gs = do
  exists <- liftIO $ doesPathExist outDir
  if exists
  then left $ OutputMustNotAlreadyExist outDir
  else liftIO $ createDirectory outDir
  genesis <- hoistEither $ serialiseGenesis ptcl genesisData
  liftIO $ LB.writeFile genesisJSONFile genesis

  dlgCerts <- mapM findDelegateCert $ gsRichSecrets gs

  liftIO $ wOut "genesis-keys" "key" (pure . serialiseSigningKey ptcl) (gsDlgIssuersSecrets gs)
  liftIO $ wOut "delegate-keys" "key" (pure . serialiseDelegateKey ptcl) (gsRichSecrets gs)
  liftIO $ wOut "poor-keys" "key" (pure . serialisePoorKey ptcl) (gsPoorSecrets gs)
  liftIO $ wOut "delegation-cert" "json" (pure . serialiseDelegationCert ptcl) dlgCerts
  liftIO $ wOut "avvm-seed" "seed" (pure . (Right <$> LB.fromStrict)) (gsFakeAvvmSeeds gs)
 where
  dlgCertMap :: Map Common.KeyHash Certificate
  dlgCertMap = Genesis.unGenesisDelegation $ Genesis.gdHeavyDelegation genesisData
  findDelegateCert :: SigningKey -> ExceptT CliError IO Certificate
  findDelegateCert sk =
    case flip find (Map.elems dlgCertMap) . isCertForSK $ sk of
      Nothing -> left . NoGenesisDelegationForKey
                 . prettyPublicKey . Crypto.toVerification $ sk
      Just x  -> right x
  genesisJSONFile :: FilePath
  genesisJSONFile = outDir <> "/genesis.json"
  -- Compare a given 'SigningKey' with a 'Certificate' 'VerificationKey'
  isCertForSK :: SigningKey -> Certificate -> Bool
  isCertForSK sk cert = delegateVK cert == Crypto.toVerification sk
  wOut :: String -> String -> (a -> IO (Either CliError LB.ByteString)) -> [a] -> IO ()
  wOut = writeSecrets outDir


writeSecrets :: FilePath -> String -> String -> (a -> IO (Either CliError LB.ByteString)) -> [a] -> IO ()
writeSecrets outDir prefix suffix secretOp xs =
  forM_ (zip xs $ [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    result <- secretOp secret
    case result of
      Left cliError -> panic . toS $ show cliError
      Right bs -> LB.writeFile filename bs
#ifdef UNIX
    setFileMode    filename ownerReadMode
#else
    setPermissions filename (emptyPermissions {readable = True})
#endif
