{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Ledger.Address (decodeRewardAcnt)
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Ledger.Compactible (Compactible (..))
import qualified Cardano.Ledger.Core as LC
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Shelley.API as L
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Rewards as L
import qualified Cardano.Ledger.Shelley.RewardUpdate as L
import qualified Cardano.Ledger.UMap as UM
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

import qualified Cardano.Api.Ledger as L
import qualified Cardano.Ledger.Coin as L
import qualified Codec.Binary.Bech32 as Bech32
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Fail.String
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Char (ord)
import           Data.Foldable (toList)
import           Data.List (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Text as T
import qualified Data.VMap as VMap
import           Lens.Micro ((^.))
import           Options.Applicative (Parser, (<**>), (<|>))
import qualified Options.Applicative as Opt

data State = State
  { lastCheckpoint    :: SlotNo
  , lastBalanceCheck  :: EpochNo
  , lastRewStartEpoch :: EpochNo
  , lastRewEndEpoch   :: EpochNo
  , lastEra           :: String
  , lastProtocolVer   :: L.ProtVer
  }

startingState :: State
startingState = State
  { lastCheckpoint    = SlotNo 0
  , lastBalanceCheck  = EpochNo 0
  , lastRewStartEpoch = EpochNo 0
  , lastRewEndEpoch   = EpochNo 0
  , lastEra           = "byron"
  , lastProtocolVer   = L.ProtVer (L.natVersion @0) 0
  }

data IsOwner = IsOwnerYes | IsOwnerNo
  deriving (Show)

data IsPoolRewardAccount = IsPoolRewardAccountYes | IsPoolRewardAccountNo
  deriving (Show)

data Event c
  = CheckPointEvent SlotNo
  | NewEraEvent EpochNo SlotNo String
  | NewPVEvent EpochNo SlotNo L.ProtVer
  | StakeRegistrationEvent EpochNo SlotNo
  | StakeDeRegistrationEvent EpochNo SlotNo
  | DelegationEvent SlotNo (Hash StakePoolKey)
  | PoolRegistrationEvent SlotNo (Hash StakePoolKey) IsOwner IsPoolRewardAccount
  | MIREvent EpochNo SlotNo Lovelace L.MIRPot
  | WithdrawalEvent EpochNo SlotNo Lovelace
  | RewardBalanceEvent EpochNo SlotNo Lovelace
  | RewardStartEvent EpochNo SlotNo Lovelace
  | RewardEndEvent EpochNo SlotNo (Set (L.Reward c))
  | Error String

msg :: Event c -> IO ()
msg ev = putStrLn (message ev)
  where
    message :: Event c -> String
    message (CheckPointEvent slotNo)       =
      "CHECKPOINT -------- "
        <> show slotNo
    message (NewEraEvent e slotNo name)    =
      "NEW-ERA ----------- "
        <> show e      <> ", "
        <> show slotNo <> ", "
        <> name
    message (NewPVEvent e slotNo pv)    =
      "NEW-PROTOCOL-VER-- "
        <> show e      <> ", "
        <> show slotNo <> ", "
        <> show (L.pvMajor pv) <> "," <> show (L.pvMinor pv)
    message (StakeRegistrationEvent epochNo slotNo)       =
      "REGISTRATION ------ "
        <> show epochNo <> ", "
        <> show slotNo
    message (StakeDeRegistrationEvent epochNo slotNo)     =
      "DE-REGISTRATION --- "
        <> show epochNo <> ", "
        <> show slotNo
    message (DelegationEvent slotNo poolId)       =
      "DELEGATION -------- "
        <> show slotNo <> ", "
        <> "PoolID " <> (tail . init $ show poolId)
    message (PoolRegistrationEvent slotNo poolId owner rewardAccount) =
      "POOL-REGISTRATION - "
        <> show slotNo
        <> ", PoolID " <> (tail . init $ show poolId)
        <> dispOwner owner
        <> dispRewardAcnt rewardAccount
    message (MIREvent epochNo slotNo love pot) =
      "MIR --------------- "
        <> show epochNo <> ", "
        <> show slotNo <> ", "
        <> show pot <> ", "
        <> show love
    message (WithdrawalEvent epochNo slotNo w)  =
      "WDRL -------------- "
        <> show epochNo <> ", "
        <> show slotNo <> ", "
        <> show w
    message (RewardBalanceEvent e slotNo balance) =
      "BALANCE ----------- "
        <> show e      <> ", "
        <> show slotNo <> ", "
        <> "balance: " <> show balance
    message (RewardStartEvent e slotNo stake) =
      "REWARD-START ------ "
        <> show e      <> ", "
        <> show slotNo <> ", "
        <> "stake: " <> show stake
    message (RewardEndEvent e slotNo rewards)   =
      "REWARD-END -------- "
        <> show e      <> ", "
        <> show slotNo <> ", "
        <> intercalate ", " (map displayReward $ toList rewards)
    message (Error e)   =
      "ERROR ------------- "
        <> show e

    dispOwner IsOwnerYes = ", Owner"
    dispOwner IsOwnerNo  = ""

    dispRewardAcnt IsPoolRewardAccountYes = ", Reward-Account"
    dispRewardAcnt IsPoolRewardAccountNo  = ""

    displayReward (L.Reward t (L.KeyHash kh) love) =
      show t <> "-" <> (tail . init $ show kh) <> "-" <> show (fromShelleyLovelace love)

decodeStakeAddress :: BS.ByteString -> Either String (L.RewardAcnt StandardCrypto)
decodeStakeAddress bs = runFail $ decodeRewardAcnt bs

decodeStakeAddressAsHex :: String -> Either String (L.Credential 'L.Staking StandardCrypto)
decodeStakeAddressAsHex s = do
  a <- Base16.decode . (BS.pack . map (fromIntegral . ord)) $ s
  (L.RewardAcnt _ sc) <- decodeStakeAddress a
  pure sc

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a Nothing   = Left a
maybeToRight _ (Just b) = Right b

first :: (a -> c) -> Either a b -> Either c b
first f (Left a) = Left (f a)
first _ (Right b) = Right b

isPermittedPrefix :: T.Text -> Bool
isPermittedPrefix prefix = prefix `elem` ["stake", "stake_test"]

decodeStakeAddressAsBech32 :: String -> Either String (L.Credential 'L.Staking StandardCrypto)
decodeStakeAddressAsBech32 s = do
  (p, d) <- first show $ Bech32.decode (T.pack s)
  let prefix = Bech32.humanReadablePartToText p
  if isPermittedPrefix prefix then Right () else Left $ "unexpected prefix: " <> show prefix
  b <- maybeToRight "invalid bech32" $ Bech32.dataPartToBytes d
  (L.RewardAcnt _ sc) <- decodeStakeAddress b
  pure sc

pStakeAddressHex :: Parser (L.Credential 'L.Staking StandardCrypto)
pStakeAddressHex = Opt.option (Opt.eitherReader decodeStakeAddressAsHex)
             (  Opt.long "stake-address-hex"
             <> Opt.metavar "HEX"
             <> Opt.help "Hex-encoded stake credential"
             )

pStakeAddressBech32 :: Parser (L.Credential 'L.Staking StandardCrypto)
pStakeAddressBech32 = Opt.option (Opt.eitherReader decodeStakeAddressAsBech32)
             (  Opt.long "stake-address-bech32"
             <> Opt.metavar "BECH32"
             <> Opt.help "Bech32-encoded stake address"
             )

data CheckPoint = CheckPointOff | CheckPointSize SlotNo

pCheckPoint :: Parser CheckPoint
pCheckPoint =
  pOff <|> pOn
 where
   pOff :: Parser CheckPoint
   pOff =
    Opt.flag CheckPointOff CheckPointOff
      (  Opt.long "checkpoints-off"
      <> Opt.help "no checkpoints (default)"
      )
   pOn :: Parser CheckPoint
   pOn =
     CheckPointSize . SlotNo <$>
       Opt.option Opt.auto
         (  Opt.long "check-point-size"
         <> Opt.metavar "NATURAL"
         <> Opt.help "display checkpoints"
         )

data Args = Args
  { conf         :: String
  , socket       :: String
  , target       :: L.Credential 'L.Staking StandardCrypto
  , checkpoint   :: CheckPoint }

parser :: Parser Args
parser = Args
  <$> Opt.strOption
      ( Opt.long "conf"
     <> Opt.short 'c'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "configuration file" )
  <*> Opt.strOption
      ( Opt.long "socket"
     <> Opt.short 's'
     <> Opt.metavar "FILEPATH"
     <> Opt.help "socket" )
  <*> (pStakeAddressHex <|> pStakeAddressBech32)
  <*> pCheckPoint

main :: IO ()
main = do
  args <- Opt.execParser $
    Opt.info (parser <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Stake Credential History")
  let targetCred = target args
      targetCredAsAPI :: StakeCredential
      targetCredAsAPI = fromShelleyStakeCredential targetCred
      f = either (error . T.unpack . renderFoldBlocksError) id
  !_ <- fmap f $ runExceptT $ foldBlocks
         (File (conf args))
         (File (socket args))
         QuickValidation
         startingState
         (\_env
           !ledgerState
           _
           (BlockInMode
             (Block (BlockHeader slotNo _blockHeaderHash (BlockNo _blockNoI)) transactions)
             eim)
           state -> do
             let getGoSnapshot = L.unStake . L.ssStake . L.ssStakeGo . L.esSnapshots . L.nesEs
                 getBalances = UM.rewardMap
                             . L.dsUnified
                             . L.certDState
                             . L.lsCertState
                             . L.esLState
                             . L.nesEs
                 getPV :: LC.EraPParams era => L.NewEpochState era -> L.ProtVer
                 getPV nes = L.esPp (L.nesEs nes) ^. LC.ppProtocolVersionL


             -- in non-byron eras, get the necessary components of the ledger state
             let (name, epochNo, shelleyState) =
                   case ledgerState of
                     LedgerStateByron _                                     ->
                       ("byron",   EpochNo 0, Nothing)
                     LedgerStateShelley (Shelley.ShelleyLedgerState _ ls _ _) ->
                       ("shelley", L.nesEL ls, Just (L.nesRu ls, getGoSnapshot ls, getBalances ls, getPV ls))
                     LedgerStateAllegra (Shelley.ShelleyLedgerState _ ls _ _) ->
                       ("allegra", L.nesEL ls, Just (L.nesRu ls, getGoSnapshot ls, getBalances ls, getPV ls))
                     LedgerStateMary    (Shelley.ShelleyLedgerState _ ls _ _) ->
                       ("mary",    L.nesEL ls, Just (L.nesRu ls, getGoSnapshot ls, getBalances ls, getPV ls))
                     LedgerStateAlonzo    (Shelley.ShelleyLedgerState _ ls _ _) ->
                       ("alonzo",  L.nesEL ls, Just (L.nesRu ls, getGoSnapshot ls, getBalances ls, getPV ls))
                     LedgerStateBabbage  (Shelley.ShelleyLedgerState _ ls _ _) ->
                       ("babbage", L.nesEL ls, Just (L.nesRu ls, getGoSnapshot ls, getBalances ls, getPV ls))
                     LedgerStateConway  (Shelley.ShelleyLedgerState _ ls _ _) ->
                       ("conway", L.nesEL ls, Just (L.nesRu ls, getGoSnapshot ls, getBalances ls, getPV ls))

             let txBodyComponents = map ( (\(TxBody txbc) -> txbc) . getTxBody ) transactions
             let sbe'm = shelleyBasedEraFromEraInMode eim
             mapM_ (delegationEvents sbe'm targetCredAsAPI epochNo slotNo) txBodyComponents
             mapM_ (withdrawalEvents targetCredAsAPI epochNo slotNo) txBodyComponents

             lastcheck <- displayCheckpoint slotNo (lastCheckpoint state) (checkpoint args)

             case shelleyState of
               Just (L.SJust (L.Complete ru), goSnap, _, _) -> do

                 es <- rewardStartEvent (lastRewStartEpoch state) epochNo slotNo (fmap fromCompact (VMap.toMap goSnap)) targetCred
                 ee <- rewardEndEvent   (lastRewEndEpoch   state) epochNo slotNo (L.rs ru)                              targetCred
                 return $ state { lastCheckpoint = lastcheck
                                , lastRewStartEpoch = es
                                , lastRewEndEpoch = ee
                                }
               Just (_, _, balances, cpv) -> do
                 eb  <- rewardBalance (lastBalanceCheck  state) epochNo slotNo balances targetCred
                 era <- newEraEvent name epochNo slotNo (lastEra state)
                 pv <- newProtocolVersion cpv epochNo slotNo (lastProtocolVer state)
                 return $ state { lastCheckpoint = lastcheck
                                , lastBalanceCheck = eb
                                , lastEra = era
                                , lastProtocolVer = pv
                                }
               _ -> return $ state {lastCheckpoint = lastcheck}
    )

  return ()
  where
    -- CheckPoints --
    displayCheckpoint :: SlotNo -> SlotNo -> CheckPoint -> IO SlotNo
    displayCheckpoint _ lastcheck CheckPointOff = return lastcheck
    displayCheckpoint slot lastcheck (CheckPointSize checkpointSize) =
      if slot - lastcheck >= checkpointSize
        then msg (CheckPointEvent slot) >> return slot
        else return lastcheck

    -- New Eras --
    newEraEvent name epochNo slotNo eraLast =
      if name /= eraLast
        then msg (NewEraEvent epochNo slotNo name) >> return name
        else return eraLast

    -- New PV --
    newProtocolVersion pv epochNo slotNo pvLast =
      if pv /= pvLast
        then msg (NewPVEvent epochNo slotNo pv) >> return pv
        else return pvLast

    -- Delegation Events --
    delegationEvents :: Maybe (ShelleyBasedEra era) -> StakeCredential -> EpochNo -> SlotNo -> TxBodyContent ViewTx era -> IO ()
    delegationEvents sbe'm t epochNo slotNo txbc = do
      case (txCertificates txbc, sbe'm) of
        (TxCertificates _ cs _, Just sbe) -> mapM_ msg $ mapMaybe (targetedCert sbe t epochNo slotNo) cs
        (_, _)    -> return ()

    targetedCert :: ShelleyBasedEra era -> StakeCredential -> EpochNo -> SlotNo -> Certificate era -> Maybe (Event c)
    targetedCert sbe t epochNo slotNo = shelleyBasedEraConstraints sbe $ \case
      ShelleyRelatedCertificate _ c ->
          case c of
            L.ShelleyTxCertDelegCert (L.ShelleyRegCert cred) ->
              if t == fromShelleyStakeCredential cred then Just (StakeRegistrationEvent epochNo slotNo) else Nothing
            L.ShelleyTxCertDelegCert (L.ShelleyUnRegCert cred) ->
              if t == fromShelleyStakeCredential cred then Just (StakeDeRegistrationEvent epochNo slotNo) else Nothing
            L.ShelleyTxCertDelegCert (L.ShelleyDelegCert cred poolId) ->
              if t == fromShelleyStakeCredential cred then Just (DelegationEvent slotNo (StakePoolKeyHash poolId)) else Nothing
            L.ShelleyTxCertPool (L.RetirePool _poolId _retirementEpoch) ->
              Nothing
            L.ShelleyTxCertPool (L.RegPool poolParams) ->
              inPoolCert t slotNo (fromShelleyPoolParams poolParams)
            L.ShelleyTxCertGenesisDeleg (L.GenesisDelegCert _genesisKeyHash _delegateKeyHash _vrfKeyHash) ->
              Nothing
            L.ShelleyTxCertMir (L.MIRCert pot (L.StakeAddressesMIR mir)) -> do
              let addrs = flip map (Map.assocs mir) $ \(cred, L.DeltaCoin coin) -> (fromShelleyStakeCredential cred, Lovelace coin)
              inMir t epochNo slotNo addrs pot
            L.ShelleyTxCertMir (L.MIRCert _pot (L.SendToOppositePotMIR _coin)) -> do
              Nothing -- TODO: unsure if Nothing

      -- TODO: Any events for ConwayCertificates?
      ConwayCertificate w cert ->
        conwayEraOnwardsConstraints w $
          case cert of
            L.RegDRepTxCert _credential _coin ->
              Nothing
            L.UnRegDRepTxCert _credential _coin ->
              Nothing
            L.AuthCommitteeHotKeyTxCert (Shelley.KeyHash _coldKey) (Shelley.KeyHash _hotKey) ->
              Nothing
            L.ResignCommitteeColdTxCert (Shelley.KeyHash _coldKey) ->
              Nothing
            L.RegTxCert _stakeCredential ->
              Nothing
            L.UnRegTxCert _stakeCredential ->
              Nothing
            L.RegDepositTxCert _stakeCredential _deposit ->
              Nothing
            L.UnRegDepositTxCert _stakeCredential _refund ->
              Nothing
            L.DelegTxCert _stakeCredential _delegatee ->
              Nothing
            L.RegDepositDelegTxCert _stakeCredential _delegatee _deposit ->
              Nothing
            L.RegPoolTxCert _poolParams ->
              Nothing
            L.RetirePoolTxCert (Shelley.KeyHash _kh) _epoch ->
              Nothing
            L.DelegStakeTxCert _stakeCredential (Shelley.KeyHash _kh) ->
              Nothing

    stakeCredentialFromStakeAddress (StakeAddress _ cred) = fromShelleyStakeCredential cred

    inPoolCert t slotNo pool =
      case (owner, rewardAccount) of
        (IsOwnerNo, IsPoolRewardAccountNo) -> Nothing
        _ -> Just (PoolRegistrationEvent slotNo (stakePoolId pool) owner rewardAccount)
     where
       owner = case t of
         StakeCredentialByKey kh -> if kh `elem` stakePoolOwners pool then IsOwnerYes else IsOwnerNo
         StakeCredentialByScript _ -> IsOwnerNo
       isRewardAccount = t == (stakeCredentialFromStakeAddress . stakePoolRewardAccount $ pool)
       rewardAccount = if isRewardAccount then IsPoolRewardAccountYes else IsPoolRewardAccountNo

    inMir t epochNo slotNo mir pot =
      case filter ((t ==) . fst) mir of
        []          -> Nothing
        [(_, love)] -> Just $ MIREvent epochNo slotNo love pot
        _           -> Just . Error $ "MIR keys should be unique: " <> show mir

    -- Withdrawal Events --
    withdrawalEvents t epochNo slotNo txbc = case txWithdrawals txbc of
      TxWithdrawalsNone  -> return ()
      TxWithdrawals _ ws -> mapM_ (withdrawalEvent epochNo slotNo) $ filter (targetWithdrawal t) ws

    withdrawalEvent epochNo slotNo (_, c, _) = msg $ WithdrawalEvent epochNo slotNo c
    targetWithdrawal t (sa, _, _) = t == stakeCredentialFromStakeAddress sa

    -- Once Per Epoch Events --
    epochEvent epochLast epochCurrent slot mp t makeEvent =
      if epochLast < epochCurrent
        then case Map.lookup t mp of
               Nothing   -> return epochCurrent
               Just love -> msg (makeEvent epochCurrent slot love)
                              >> return epochCurrent
        else return epochLast

    -- Reward Balance Check Event --
    rewardBalance epochLast epochCurrent slot rs t =
      epochEvent epochLast epochCurrent slot rs t
        (\e s v -> RewardBalanceEvent e s (fromShelleyLovelace v))

    -- Reward Calculation Start Event --
    rewardStartEvent epochLast epochCurrent slot ss t =
      epochEvent epochLast epochCurrent slot ss t
        (\e s v -> RewardStartEvent e s (fromShelleyLovelace v))

    -- Reward Calculation End Event --
    rewardEndEvent epochLast epochCurrent slot rs t =
      epochEvent epochLast epochCurrent slot rs t RewardEndEvent

shelleyBasedEraFromEraInMode :: EraInMode era mode -> Maybe (ShelleyBasedEra era)
shelleyBasedEraFromEraInMode = \case
  ByronEraInByronMode     -> Nothing
  ByronEraInCardanoMode   -> Nothing
  ShelleyEraInCardanoMode -> Just ShelleyBasedEraShelley
  ShelleyEraInShelleyMode -> Just ShelleyBasedEraShelley
  AllegraEraInCardanoMode -> Just ShelleyBasedEraAllegra
  MaryEraInCardanoMode    -> Just ShelleyBasedEraMary
  AlonzoEraInCardanoMode  -> Just ShelleyBasedEraAlonzo
  BabbageEraInCardanoMode -> Just ShelleyBasedEraBabbage
  ConwayEraInCardanoMode  -> Just ShelleyBasedEraConway

