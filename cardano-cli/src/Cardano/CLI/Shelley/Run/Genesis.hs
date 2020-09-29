{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Shelley.Run.Genesis
  ( ShelleyGenesisCmdError
  , renderShelleyGenesisCmdError
  , runGenesisCmd
  ) where

import           Cardano.Prelude
import           Prelude (id)

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isDigit)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath (takeExtension, takeExtensions, (</>))
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                     newExceptT)

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Api.Shelley.Genesis
import           Cardano.Api.TextView (TextViewDescription (..))
import           Cardano.Api.Typed

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley)
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesisStaking(..))

import qualified Shelley.Spec.Ledger.Address as Ledger
import qualified Shelley.Spec.Ledger.API as Ledger
import qualified Shelley.Spec.Ledger.Keys as Ledger
import qualified Shelley.Spec.Ledger.BaseTypes as Ledger

import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Parsers (renderTxIn)
import           Cardano.CLI.Shelley.Run.Address
                   (ShelleyAddressCmdError(..), SomeAddressVerificationKey(..),
                    buildShelleyAddress, readAddressVerificationKeyFile, renderShelleyAddressCmdError, runAddressKeyGen)
import           Cardano.CLI.Shelley.Run.Node
                   (ShelleyNodeCmdError(..), renderShelleyNodeCmdError,
                    runNodeIssueOpCert, runNodeKeyGenKES, runNodeKeyGenVRF, runNodeKeyGenCold)
import           Cardano.CLI.Shelley.Run.Pool
                   (ShelleyPoolCmdError(..), renderShelleyPoolCmdError)
import           Cardano.CLI.Shelley.Run.StakeAddress
                   (ShelleyStakeAddressCmdError(..),
                    renderShelleyStakeAddressCmdError, runStakeAddressKeyGen)
import           Cardano.CLI.Types

{- HLINT ignore "Reduce duplication" -}

data ShelleyGenesisCmdError
  = ShelleyGenesisCmdAesonDecodeError !FilePath !Text
  | ShelleyGenesisCmdGenesisFileError !(FileError ())
  | ShelleyGenesisCmdMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | ShelleyGenesisCmdFilesNoIndex [FilePath]
  | ShelleyGenesisCmdFilesDupIndex [FilePath]
  | ShelleyGenesisCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | ShelleyGenesisCmdUnexpectedAddressVerificationKey !VerificationKeyFile !Text !SomeAddressVerificationKey
  | ShelleyGenesisCmdAddressCmdError !ShelleyAddressCmdError
  | ShelleyGenesisCmdNodeCmdError !ShelleyNodeCmdError
  | ShelleyGenesisCmdPoolCmdError !ShelleyPoolCmdError
  | ShelleyGenesisCmdStakeAddressCmdError !ShelleyStakeAddressCmdError
  deriving Show

renderShelleyGenesisCmdError :: ShelleyGenesisCmdError -> Text
renderShelleyGenesisCmdError err =
  case err of
    ShelleyGenesisCmdAesonDecodeError fp decErr ->
      "Error while decoding Shelley genesis at: " <> textShow fp <> " Error: " <> textShow decErr
    ShelleyGenesisCmdGenesisFileError fe -> Text.pack $ displayError fe
    ShelleyGenesisCmdMismatchedGenesisKeyFiles gfiles dfiles vfiles ->
      "Mismatch between the files found:\n"
        <> "Genesis key file indexes:      " <> textShow gfiles
        <> "delegate key file indexes:     " <> textShow dfiles
        <> "delegate VRF key file indexes: " <> textShow vfiles
    ShelleyGenesisCmdFilesNoIndex files ->
      "The genesis keys files are expected to have a numeric index but these do not:\n"
        <> Text.unlines (map Text.pack files)
    ShelleyGenesisCmdFilesDupIndex files ->
      "The genesis keys files are expected to have a unique numeric index but these do not:\n"
        <> Text.unlines (map Text.pack files)
    ShelleyGenesisCmdTextEnvReadFileError fileErr -> Text.pack $ displayError fileErr
    ShelleyGenesisCmdUnexpectedAddressVerificationKey (VerificationKeyFile file) expect got -> mconcat
      [ "Unexpected address verification key type in file ", Text.pack file
      , ", expected: ", expect, ", got: ", textShow got
      ]
    ShelleyGenesisCmdAddressCmdError e -> renderShelleyAddressCmdError e
    ShelleyGenesisCmdNodeCmdError e -> renderShelleyNodeCmdError e
    ShelleyGenesisCmdPoolCmdError e -> renderShelleyPoolCmdError e
    ShelleyGenesisCmdStakeAddressCmdError e -> renderShelleyStakeAddressCmdError e


runGenesisCmd :: GenesisCmd -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCmd (GenesisKeyGenGenesis vk sk) = runGenesisKeyGenGenesis vk sk
runGenesisCmd (GenesisKeyGenDelegate vk sk ctr) = runGenesisKeyGenDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenUTxO vk sk) = runGenesisKeyGenUTxO vk sk
runGenesisCmd (GenesisCmdKeyHash vk) = runGenesisKeyHash vk
runGenesisCmd (GenesisVerKey vk sk) = runGenesisVerKey vk sk
runGenesisCmd (GenesisTxIn vk nw mOutFile) = runGenesisTxIn vk nw mOutFile
runGenesisCmd (GenesisAddr vk nw mOutFile) = runGenesisAddr vk nw mOutFile
runGenesisCmd (GenesisCreate gd gn un ms am nw) = runGenesisCreate gd gn un ms am nw
runGenesisCmd (GenesisCreateStaked gd gn gp gl un ms am ds nw) = runGenesisCreateStaked gd gn gp gl un ms am ds nw
runGenesisCmd (GenesisHashFile gf) = runGenesisHashFile gf

--
-- Genesis command implementations
--

runGenesisKeyGenGenesis :: VerificationKeyFile -> SigningKeyFile
                        -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenGenesis (VerificationKeyFile vkeyPath)
                        (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewDescription
    skeyDesc = TextViewDescription "Genesis Signing Key"
    vkeyDesc = TextViewDescription "Genesis Verification Key"


runGenesisKeyGenDelegate :: VerificationKeyFile
                         -> SigningKeyFile
                         -> OpCertCounterFile
                         -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenDelegate (VerificationKeyFile vkeyPath)
                         (SigningKeyFile skeyPath)
                         (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisDelegateKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just certCtrDesc)
      $ OperationalCertificateIssueCounter
          initialCounter
          (castVerificationKey vkey)  -- Cast to a 'StakePoolKey'
  where
    skeyDesc, vkeyDesc, certCtrDesc :: TextViewDescription
    skeyDesc = TextViewDescription "Genesis delegate operator key"
    vkeyDesc = TextViewDescription "Genesis delegate operator key"
    certCtrDesc = TextViewDescription $ "Next certificate issue number: " <> BS.pack (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runGenesisKeyGenDelegateVRF :: VerificationKeyFile -> SigningKeyFile
                            -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenDelegateVRF (VerificationKeyFile vkeyPath)
                            (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewDescription
    skeyDesc = TextViewDescription "VRF Signing Key"
    vkeyDesc = TextViewDescription "VRF Verification Key"


runGenesisKeyGenUTxO :: VerificationKeyFile -> SigningKeyFile
                     -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyGenUTxO (VerificationKeyFile vkeyPath)
                     (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisUTxOKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT ShelleyGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextViewDescription
    skeyDesc = TextViewDescription "Genesis Initial UTxO Signing Key"
    vkeyDesc = TextViewDescription "Genesis Initial UTxO Verification Key"


runGenesisKeyHash :: VerificationKeyFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisKeyHash (VerificationKeyFile vkeyPath) = do
    vkey <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelopeAnyOf
              [ FromSomeType (AsVerificationKey AsGenesisKey)
                             AGenesisKey
              , FromSomeType (AsVerificationKey AsGenesisDelegateKey)
                             AGenesisDelegateKey
              , FromSomeType (AsVerificationKey AsGenesisUTxOKey)
                             AGenesisUTxOKey
              ]
              vkeyPath
    liftIO $ BS.putStrLn (renderKeyHash vkey)
  where
    renderKeyHash :: SomeGenesisKey VerificationKey -> ByteString
    renderKeyHash (AGenesisKey         vk) = renderVerificationKeyHash vk
    renderKeyHash (AGenesisDelegateKey vk) = renderVerificationKeyHash vk
    renderKeyHash (AGenesisUTxOKey     vk) = renderVerificationKeyHash vk

    renderVerificationKeyHash :: Key keyrole => VerificationKey keyrole -> ByteString
    renderVerificationKeyHash = serialiseToRawBytesHex
                              . verificationKeyHash


runGenesisVerKey :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisVerKey (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelopeAnyOf
              [ FromSomeType (AsSigningKey AsGenesisKey)
                             AGenesisKey
              , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                             AGenesisDelegateKey
              , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                             AGenesisUTxOKey
              ]
              skeyPath

    let vkey :: SomeGenesisKey VerificationKey
        vkey = case skey of
          AGenesisKey         sk -> AGenesisKey         (getVerificationKey sk)
          AGenesisDelegateKey sk -> AGenesisDelegateKey (getVerificationKey sk)
          AGenesisUTxOKey     sk -> AGenesisUTxOKey     (getVerificationKey sk)

    firstExceptT ShelleyGenesisCmdGenesisFileError . newExceptT . liftIO $
      case vkey of
        AGenesisKey         vk -> writeFileTextEnvelope vkeyPath Nothing vk
        AGenesisDelegateKey vk -> writeFileTextEnvelope vkeyPath Nothing vk
        AGenesisUTxOKey     vk -> writeFileTextEnvelope vkeyPath Nothing vk

data SomeGenesisKey f
     = AGenesisKey         (f GenesisKey)
     | AGenesisDelegateKey (f GenesisDelegateKey)
     | AGenesisUTxOKey     (f GenesisUTxOKey)


runGenesisTxIn :: VerificationKeyFile -> NetworkId -> Maybe OutputFile
               -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisTxIn (VerificationKeyFile vkeyPath) network mOutFile = do
    vkey <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) vkeyPath
    let txin = genesisUTxOPseudoTxIn network (verificationKeyHash vkey)
    liftIO $ writeOutput mOutFile (renderTxIn txin)


runGenesisAddr :: VerificationKeyFile -> NetworkId -> Maybe OutputFile
               -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisAddr (VerificationKeyFile vkeyPath) network mOutFile = do
    vkey <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) vkeyPath
    let vkh  = verificationKeyHash (castVerificationKey vkey)
        addr = makeShelleyAddress network (PaymentCredentialByKey vkh)
                                  NoStakeAddress
    liftIO $ writeOutput mOutFile (serialiseAddress addr)

writeOutput :: Maybe OutputFile -> Text -> IO ()
writeOutput (Just (OutputFile fpath)) = Text.writeFile fpath
writeOutput Nothing                   = Text.putStrLn


--
-- Create Genesis command implementation
--

runGenesisCreate :: GenesisDir
                 -> Word  -- ^ num genesis & delegate keys to make
                 -> Word  -- ^ num utxo keys to make
                 -> Maybe SystemStart
                 -> Maybe Lovelace
                 -> NetworkId
                 -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCreate (GenesisDir rootdir)
                 genNumGenesisKeys genNumUTxOKeys
                 mStart mAmount network = do
  liftIO $ do
    createDirectoryIfMissing False rootdir
    createDirectoryIfMissing False gendir
    createDirectoryIfMissing False deldir
    createDirectoryIfMissing False utxodir

  template <- readShelleyGenesis (rootdir </> "genesis.spec.json") adjustTemplate

  forM_ [ 1 .. genNumGenesisKeys ] $ \index -> do
    createGenesisKeys  gendir  index
    createDelegateKeys deldir index

  forM_ [ 1 .. genNumUTxOKeys ] $ \index ->
    createUtxoKeys utxodir index

  genDlgs <- readGenDelegsMap gendir deldir
  utxoAddrs <- readInitialFundAddresses utxodir network
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

  let finalGenesis = updateTemplate start genDlgs mAmount utxoAddrs mempty (Lovelace 0) [] template

  writeShelleyGenesis (rootdir </> "genesis.json") finalGenesis
  where
    adjustTemplate t = t { sgNetworkMagic = unNetworkMagic (toNetworkMagic network) }
    gendir  = rootdir </> "genesis-keys"
    deldir  = rootdir </> "delegate-keys"
    utxodir = rootdir </> "utxo-keys"

runGenesisCreateStaked ::
     GenesisDir
  -> Word           -- ^ num genesis & delegate keys to make
  -> Word           -- ^ num utxo keys to make
  -> Word           -- ^ num pools to make
  -> Word           -- ^ num delegators to make
  -> Maybe SystemStart
  -> Maybe Lovelace -- ^ supply going to non-delegators
  -> Lovelace       -- ^ supply going to delegators
  -> NetworkId
  -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisCreateStaked (GenesisDir rootdir)
                 genNumGenesisKeys genNumUTxOKeys genNumPools genNumStDelegs
                 mStart mNonDlgAmount stDlgAmount network = do
  liftIO $ do
    createDirectoryIfMissing False rootdir
    createDirectoryIfMissing False gendir
    createDirectoryIfMissing False deldir
    createDirectoryIfMissing False pooldir
    createDirectoryIfMissing False stdeldir
    createDirectoryIfMissing False utxodir

  template <- readShelleyGenesis (rootdir </> "genesis.spec.json") adjustTemplate

  forM_ [ 1 .. genNumGenesisKeys ] $ \index -> do
    createGenesisKeys  gendir  index
    createDelegateKeys deldir index

  forM_ [ 1 .. genNumUTxOKeys ] $ \index ->
    createUtxoKeys utxodir index

  pools <- forM [ 1 .. genNumPools ] $ \index -> do
    createPoolCredentials pooldir index
    buildPool network pooldir index

  forM_ [ 1 .. genNumStDelegs ] $ \index ->
    createDelegatorCredentials stdeldir index

  delegations :: [Delegation] <-
    -- Distribute M delegates across N pools:
    forM [ (pool, delegIx)
         | (pool, poolIx) <- zip pools [1 ..]
         , delegIxLocal <- [ 1 .. delegsPerPool ] ++
                           -- Add all remaining delegates to the last pool:
                           if delegsRemaining /= 0 && poolIx == genNumPools
                           then [ delegsPerPool + 1 .. delegsPerPool + delegsRemaining ]
                           else []
         , let delegIx = delegIxLocal + delegsPerPool * (poolIx - 1)] $
      uncurry (computeDelegation network stdeldir)

  genDlgs <- readGenDelegsMap gendir deldir
  nonDelegAddrs <- readInitialFundAddresses utxodir network
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

  let poolMap = Map.fromList $ mkDelegationMapEntry <$> delegations
      delegAddrs = dInitialUtxoAddr <$> delegations
      finalGenesis = updateTemplate start genDlgs mNonDlgAmount nonDelegAddrs poolMap stDlgAmount delegAddrs template

  writeShelleyGenesis (rootdir </> "genesis.json") finalGenesis
  liftIO $ Text.putStrLn $ mconcat
    [ "generated genesis with: "
    , textShow genNumGenesisKeys, " genesis keys, "
    , textShow genNumUTxOKeys, " non-delegating UTxO keys, "
    , textShow genNumPools, " stake pools, "
    , textShow genNumStDelegs, " delegating UTxO keys, "
    , textShow (length delegations), " delegation relationships, "
    , textShow (Map.size poolMap), " delegation map entries, "
    , textShow (length delegAddrs), " delegating addresses"
    ]

  where
    (,) delegsPerPool delegsRemaining = divMod genNumStDelegs genNumPools
    adjustTemplate t = t { sgNetworkMagic = unNetworkMagic (toNetworkMagic network) }
    mkDelegationMapEntry :: Delegation -> (Ledger.KeyHash Ledger.Staking StandardShelley, Ledger.PoolParams StandardShelley)
    mkDelegationMapEntry d = (dDelegStaking d, dPoolParams d)

    gendir   = rootdir </> "genesis-keys"
    deldir   = rootdir </> "delegate-keys"
    pooldir  = rootdir </> "pools"
    stdeldir = rootdir </> "stake-delegator-keys"
    utxodir  = rootdir </> "utxo-keys"

-- -------------------------------------------------------------------------------------------------

createDelegateKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createDelegateKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  runGenesisKeyGenDelegate
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vkey")
        coldSK
        opCertCtr
  runGenesisKeyGenDelegateVRF
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vrf.vkey")
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".vrf.skey")
  firstExceptT ShelleyGenesisCmdNodeCmdError $ do
    runNodeKeyGenKES
        kesVK
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".kes.skey")
    runNodeIssueOpCert
        kesVK
        coldSK
        opCertCtr
        (KESPeriod 0)
        (OutputFile $ dir </> "opcert" ++ strIndex ++ ".cert")
 where
   strIndex = show index
   kesVK = VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".kes.vkey"
   coldSK = SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".skey"
   opCertCtr = OpCertCounterFile $ dir </> "delegate" ++ strIndex ++ ".counter"

createGenesisKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createGenesisKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenGenesis
        (VerificationKeyFile $ dir </> "genesis" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "genesis" ++ strIndex ++ ".skey")


createUtxoKeys :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createUtxoKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenUTxO
        (VerificationKeyFile $ dir </> "utxo" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "utxo" ++ strIndex ++ ".skey")

createPoolCredentials :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createPoolCredentials dir index = do
  liftIO $ createDirectoryIfMissing False dir
  firstExceptT ShelleyGenesisCmdNodeCmdError $ do
    runNodeKeyGenKES
        kesVK
        (SigningKeyFile $ dir </> "kes" ++ strIndex ++ ".skey")
    runNodeKeyGenVRF
        (VerificationKeyFile $ dir </> "vrf" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "vrf" ++ strIndex ++ ".skey")
    runNodeKeyGenCold
        (VerificationKeyFile $ dir </> "cold" ++ strIndex ++ ".vkey")
        coldSK
        opCertCtr
    runNodeIssueOpCert
        kesVK
        coldSK
        opCertCtr
        (KESPeriod 0)
        (OutputFile $ dir </> "opcert" ++ strIndex ++ ".cert")
  firstExceptT ShelleyGenesisCmdStakeAddressCmdError $
    runStakeAddressKeyGen
        (VerificationKeyFile $ dir </> "staking-reward" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "staking-reward" ++ strIndex ++ ".skey")
 where
   strIndex = show index
   kesVK = VerificationKeyFile $ dir </> "kes" ++ strIndex ++ ".vkey"
   coldSK = SigningKeyFile $ dir </> "cold" ++ strIndex ++ ".skey"
   opCertCtr = OpCertCounterFile $ dir </> "opcert" ++ strIndex ++ ".counter"

createDelegatorCredentials :: FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO ()
createDelegatorCredentials dir index = do
  liftIO $ createDirectoryIfMissing False dir
  firstExceptT ShelleyGenesisCmdAddressCmdError $ do
    runAddressKeyGen
        AddressKeyShelley
        addrVK
        (SigningKeyFile $ dir </> "payment" ++ strIndex ++ ".skey")
  firstExceptT ShelleyGenesisCmdStakeAddressCmdError $
    runStakeAddressKeyGen
        (VerificationKeyFile $ dir </> "staking" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "staking" ++ strIndex ++ ".skey")
 where
   strIndex = show index
   addrVK = VerificationKeyFile $ dir </> "payment" ++ strIndex ++ ".vkey"

data Delegation
  = Delegation
    { dInitialUtxoAddr  :: Address Shelley
    , dDelegStaking     :: Ledger.KeyHash Ledger.Staking StandardShelley
    , dPoolParams       :: Ledger.PoolParams StandardShelley
    }

buildPool :: NetworkId -> FilePath -> Word -> ExceptT ShelleyGenesisCmdError IO (Ledger.PoolParams StandardShelley)
buildPool nw dir index = do
    StakePoolVerificationKey poolColdVK <- firstExceptT (ShelleyGenesisCmdPoolCmdError
                                                         . ShelleyPoolCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) poolColdVKF
    VrfVerificationKey poolVrfVK <- firstExceptT (ShelleyGenesisCmdNodeCmdError
                                                  . ShelleyNodeCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsVrfKey) poolVrfVKF
    rewardsSVK <- firstExceptT (ShelleyGenesisCmdStakeAddressCmdError
                                . ShelleyStakeAddressCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) poolRewardVKF
    pure Ledger.PoolParams
      { Ledger._poolPubKey = Ledger.hashKey poolColdVK
      , Ledger._poolVrf    = Ledger.hashVerKeyVRF poolVrfVK
      , Ledger._poolPledge = Ledger.Coin 0
      , Ledger._poolCost   = Ledger.Coin 0
      , Ledger._poolMargin = Ledger.truncateUnitInterval 0
      , Ledger._poolRAcnt  =
          toShelleyStakeAddr $ makeStakeAddress nw $ StakeCredentialByKey (verificationKeyHash rewardsSVK)
      , Ledger._poolOwners = mempty
      , Ledger._poolRelays = Seq.empty
      , Ledger._poolMD     = Ledger.SNothing
      }
 where
   strIndex = show index
   poolColdVKF = dir </> "cold" ++ strIndex ++ ".vkey"
   poolVrfVKF = dir </> "vrf" ++ strIndex ++ ".vkey"
   poolRewardVKF = dir </> "staking-reward" ++ strIndex ++ ".vkey"

computeDelegation :: NetworkId -> FilePath -> Ledger.PoolParams StandardShelley -> Word -> ExceptT ShelleyGenesisCmdError IO Delegation
computeDelegation nw delegDir pool delegIx = do
    paySVK <- firstExceptT (ShelleyGenesisCmdAddressCmdError
                           . ShelleyAddressCmdReadFileError) $
                 readAddressVerificationKeyFile payVKF
    initialUtxoAddr <- case paySVK of
      APaymentVerificationKey payVK ->
        firstExceptT ShelleyGenesisCmdAddressCmdError
        $ buildShelleyAddress payVK (Just $ VerificationKeyFile stakeVKF) nw
      _ -> left $ ShelleyGenesisCmdUnexpectedAddressVerificationKey payVKF "APaymentVerificationKey" paySVK

    StakeVerificationKey stakeVK <- firstExceptT (ShelleyGenesisCmdStakeAddressCmdError . ShelleyStakeAddressCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stakeVKF

    pure Delegation
      { dInitialUtxoAddr = initialUtxoAddr
      , dDelegStaking = Ledger.hashKey stakeVK
      , dPoolParams = pool
      }
 where
   strIndexDeleg = show delegIx
   payVKF = VerificationKeyFile $ delegDir </> "payment" ++ strIndexDeleg ++ ".vkey"
   stakeVKF = delegDir </> "staking" ++ strIndexDeleg ++ ".vkey"

-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT ShelleyGenesisCmdError IO UTCTime
getCurrentTimePlus30 =
    plus30sec <$> liftIO getCurrentTime
  where
    plus30sec :: UTCTime -> UTCTime
    plus30sec = addUTCTime (30 :: NominalDiffTime)


readShelleyGenesis
  :: FilePath
  -> (ShelleyGenesis StandardShelley -> ShelleyGenesis StandardShelley)
  -> ExceptT ShelleyGenesisCmdError IO (ShelleyGenesis StandardShelley)
readShelleyGenesis fpath adjustDefaults = do
    readAndDecode
      `catchError` \err ->
        case err of
          ShelleyGenesisCmdGenesisFileError (FileIOError _ ioe)
            | isDoesNotExistError ioe -> writeDefault
          _                           -> left err
  where
    readAndDecode = do
      lbs <- handleIOExceptT (ShelleyGenesisCmdGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
      firstExceptT (ShelleyGenesisCmdAesonDecodeError fpath . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs

    defaults :: ShelleyGenesis StandardShelley
    defaults = adjustDefaults shelleyGenesisDefaults

    writeDefault = do
      handleIOExceptT (ShelleyGenesisCmdGenesisFileError . FileIOError fpath) $
        LBS.writeFile fpath (encodePretty defaults)
      return defaults


updateTemplate
    :: SystemStart
    -- Genesis delegation (not stake-based):
    -> Map (Hash GenesisKey) (Hash GenesisDelegateKey, Hash VrfKey)
    -- Non-delegated initial UTxO spec:
    -> Maybe Lovelace
    -> [Address Shelley]
    -- Genesis staking: pools/delegation map & delegated initial UTxO spec:
    -> Map (Ledger.KeyHash 'Ledger.Staking StandardShelley) (Ledger.PoolParams StandardShelley)
    -> Lovelace
    -> [Address Shelley]
    -> ShelleyGenesis StandardShelley
    -> ShelleyGenesis StandardShelley
updateTemplate (SystemStart start)
               genDelegMap mAmountNonDeleg utxoAddrsNonDeleg
               poolSpecs (Lovelace amountDeleg) utxoAddrsDeleg
               template =
    template
      { sgSystemStart = start
      , sgMaxLovelaceSupply = fromIntegral $ nonDelegCoin + delegCoin
      , sgGenDelegs = shelleyDelKeys
      , sgInitialFunds = Map.fromList
                          [ (toShelleyAddr addr, toShelleyLovelace v)
                          | (addr, v) <-
                            distribute nonDelegCoin utxoAddrsNonDeleg ++
                            distribute delegCoin    utxoAddrsDeleg ]
      , sgStaking =
        ShelleyGenesisStaking
          { sgsPools = Map.fromList
                        [ (Ledger._poolPubKey poolParams, poolParams)
                        | poolParams <- Map.elems poolSpecs ]
          , sgsStake = Ledger._poolPubKey <$> poolSpecs
          }
      }
  where
    nonDelegCoin, delegCoin :: Integer
    nonDelegCoin = fromIntegral $ fromMaybe (sgMaxLovelaceSupply template) (unLovelace <$> mAmountNonDeleg)
    delegCoin = fromIntegral amountDeleg

    distribute :: Integer -> [Address Shelley] -> [(Address Shelley, Lovelace)]
    distribute funds addrs =
      fst $ List.foldl' folder ([], fromIntegral funds) addrs
     where
       nAddrs, coinPerAddr, splitThreshold :: Integer
       nAddrs = fromIntegral $ length addrs
       coinPerAddr = funds `div` nAddrs
       splitThreshold = coinPerAddr + nAddrs

       folder :: ([(Address Shelley, Lovelace)], Integer)
              -> Address Shelley
              -> ([(Address Shelley, Lovelace)], Integer)
       folder (acc, rest) addr
         | rest > splitThreshold =
             ((addr, Lovelace coinPerAddr) : acc, rest - coinPerAddr)
         | otherwise = ((addr, Lovelace rest) : acc, 0)

    shelleyDelKeys =
      Map.fromList
        [ (gh, Ledger.GenDelegPair gdh h)
        | (GenesisKeyHash gh,
           (GenesisDelegateKeyHash gdh, VrfKeyHash h)) <- Map.toList genDelegMap
        ]

    unLovelace :: Integral a => Lovelace -> a
    unLovelace (Lovelace coin) = fromIntegral coin

writeShelleyGenesis :: FilePath -> ShelleyGenesis StandardShelley -> ExceptT ShelleyGenesisCmdError IO ()
writeShelleyGenesis fpath sg =
  handleIOExceptT (ShelleyGenesisCmdGenesisFileError . FileIOError fpath) $ LBS.writeFile fpath (encodePretty sg)

toShelleyAddr :: Address Shelley -> Ledger.Addr StandardShelley
toShelleyAddr (ByronAddress addr)        = Ledger.AddrBootstrap
                                             (Ledger.BootstrapAddress addr)
toShelleyAddr (ShelleyAddress nw pc scr) = Ledger.Addr nw pc scr

toShelleyLovelace :: Lovelace -> Ledger.Coin
toShelleyLovelace (Lovelace l) = Ledger.Coin l


-- -------------------------------------------------------------------------------------------------

readGenDelegsMap :: FilePath -> FilePath
                 -> ExceptT ShelleyGenesisCmdError IO
                            (Map (Hash GenesisKey)
                                 (Hash GenesisDelegateKey, Hash VrfKey))
readGenDelegsMap gendir deldir = do
    gkm <- readGenesisKeys gendir
    dkm <- readDelegateKeys deldir
    vkm <- readDelegateVrfKeys deldir

    let combinedMap :: Map Int (VerificationKey GenesisKey,
                                (VerificationKey GenesisDelegateKey,
                                 VerificationKey VrfKey))
        combinedMap =
          Map.intersectionWith (,)
            gkm
            (Map.intersectionWith (,)
               dkm vkm)

    -- All the maps should have an identical set of keys. Complain if not.
    let gkmExtra = gkm Map.\\ combinedMap
        dkmExtra = dkm Map.\\ combinedMap
        vkmExtra = vkm Map.\\ combinedMap
    unless (Map.null gkmExtra && Map.null dkmExtra && Map.null vkmExtra) $
      throwError $ ShelleyGenesisCmdMismatchedGenesisKeyFiles
                     (Map.keys gkm) (Map.keys dkm) (Map.keys vkm)

    let delegsMap :: Map (Hash GenesisKey)
                         (Hash GenesisDelegateKey, Hash VrfKey)
        delegsMap =
          Map.fromList [ (gh, (dh, vh))
                       | (g,(d,v)) <- Map.elems combinedMap
                       , let gh = verificationKeyHash g
                             dh = verificationKeyHash d
                             vh = verificationKeyHash v
                       ]

    pure delegsMap


readGenesisKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO
                                       (Map Int (VerificationKey GenesisKey))
readGenesisKeys gendir = do
  files <- liftIO (listDirectory gendir)
  fileIxs <- extractFileNameIndexes [ gendir </> file
                                    | file <- files
                                    , takeExtension file == ".vkey" ]
  firstExceptT ShelleyGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKeyFile file
        | (file, ix) <- fileIxs ]
  where
    readKeyFile = newExceptT
                . readFileTextEnvelope (AsVerificationKey AsGenesisKey)

readDelegateKeys :: FilePath
                 -> ExceptT ShelleyGenesisCmdError IO
                            (Map Int (VerificationKey GenesisDelegateKey))
readDelegateKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <- extractFileNameIndexes [ deldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vkey" ]
  firstExceptT ShelleyGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKeyFile file
        | (file, ix) <- fileIxs ]
  where
    readKeyFile = newExceptT
                . readFileTextEnvelope (AsVerificationKey AsGenesisDelegateKey)

readDelegateVrfKeys :: FilePath -> ExceptT ShelleyGenesisCmdError IO
                                           (Map Int (VerificationKey VrfKey))
readDelegateVrfKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <- extractFileNameIndexes [ deldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vrf.vkey" ]
  firstExceptT ShelleyGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKeyFile file
        | (file, ix) <- fileIxs ]
  where
    readKeyFile = newExceptT
                . readFileTextEnvelope (AsVerificationKey AsVrfKey)


-- | The file path is of the form @"delegate-keys/delegate3.vkey"@.
-- This function reads the file and extracts the index (in this case 3).
--
extractFileNameIndex :: FilePath -> Maybe Int
extractFileNameIndex fp =
  case filter isDigit fp of
    [] -> Nothing
    xs -> readMaybe xs

extractFileNameIndexes :: [FilePath]
                       -> ExceptT ShelleyGenesisCmdError IO [(FilePath, Int)]
extractFileNameIndexes files = do
    case [ file | (file, Nothing) <- filesIxs ] of
      []     -> return ()
      files' -> throwError (ShelleyGenesisCmdFilesNoIndex files')
    case filter (\g -> length g > 1)
       . groupBy ((==) `on` snd)
       . sortBy (compare `on` snd)
       $ [ (file, ix) | (file, Just ix) <- filesIxs ] of
      [] -> return ()
      (g:_) -> throwError (ShelleyGenesisCmdFilesDupIndex (map fst g))

    return [ (file, ix) | (file, Just ix) <- filesIxs ]
  where
    filesIxs = [ (file, extractFileNameIndex file) | file <- files ]

readInitialFundAddresses :: FilePath -> NetworkId
                         -> ExceptT ShelleyGenesisCmdError IO [Address Shelley]
readInitialFundAddresses utxodir nw = do
    files <- liftIO (listDirectory utxodir)
    vkeys <- firstExceptT ShelleyGenesisCmdTextEnvReadFileError $
               sequence
                 [ newExceptT $
                     readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey)
                                          (utxodir </> file)
                 | file <- files
                 , takeExtension file == ".vkey" ]
    return [ addr | vkey <- vkeys
           , let vkh  = verificationKeyHash (castVerificationKey vkey)
                 addr = makeShelleyAddress nw (PaymentCredentialByKey vkh)
                                           NoStakeAddress
           ]


-- | Hash a genesis file
runGenesisHashFile :: GenesisFile -> ExceptT ShelleyGenesisCmdError IO ()
runGenesisHashFile (GenesisFile fpath) = do
   content <- handleIOExceptT (ShelleyGenesisCmdGenesisFileError . FileIOError fpath) $
              BS.readFile fpath
   let gh :: Crypto.Hash Crypto.Blake2b_256 ByteString
       gh = Crypto.hashWith id content
   liftIO $ Text.putStrLn (Crypto.hashToTextAsHex gh)
