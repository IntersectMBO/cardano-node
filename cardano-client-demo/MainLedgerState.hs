{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import Cardano.Api
    (Hash, SlotNo, ByronEra, ChainSyncClient,  Block(..),
      BlockHeader(BlockHeader),
      ChainPoint(..),
      ChainTip(ChainTip),
      NetworkId(Mainnet),
      BlockNo(BlockNo),
      EraInMode(..) )
import qualified Cardano.Api.Block as Block
import Cardano.Binary (Raw)
import qualified Cardano.Api.IPC as IPC
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron
import qualified Cardano.Chain.Slotting as Byron (EpochSlots(..))
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Block (BlockValidationMode (..))
import Cardano.Chain.UTxO (TxValidationMode (..))

-- TODO: Export this via Cardano.Api
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic(..))
import Ouroboros.Network.Protocol.ChainSync.Client
import Network.TypedProtocol.Pipelined
    ( N(..), Nat(..), natToInt, unsafeIntToNat )

import Ouroboros.Consensus.Byron.Ledger.Block
import Ouroboros.Consensus.Ledger.Abstract (tickThenApply)
import Control.Monad (when)
import Control.Monad.Except
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Proxy
import Data.Time
import qualified GHC.TypeLits as GHC
import System.Environment (getArgs)
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)


main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketDir : _ <- getArgs
  let socketPath = socketDir </> "node.sock"

  -- TODO Byron config
  --
  --
  -- db-sync gets this in Cardano.DbSync.runDbSyncNode
  --
  --    enc <- readDbSyncNodeConfig (enpConfigFile enp)
  --    genCfg :: GenesisConfig <- readCardanoGenesisConfig enc
  --
  -- data GenesisConfig = GenesisCardano !DbSyncNodeConfig !Byron.Config !ShelleyConfig
  --
  -- (enp :: DbSyncNodeParams) is an arg parsed from the command line arguments
  -- in cardano-db-sync.hs
  --
  --
  -- readCardanoGenesisConfig :: DbSyncNodeConfig -> ExceptT DbSyncNodeError IO GenesisConfig
  -- readCardanoGenesisConfig enc =
  --   ...
  --   GenesisCardano enc <$> readByronGenesisConfig enc <*> readShelleyGenesisConfig enc
  --
  -- readByronGenesisConfig :: DbSyncNodeConfig -> ExceptT DbSyncNodeError IO Byron.Config
  -- readByronGenesisConfig enc = do
  --   let file = unGenesisFile $ dncByronGenesisFile enc
  --   genHash <- firstExceptT NEError
  --                 . hoistEither
  --                 $ decodeAbstractHash (unGenesisHashByron $ dncByronGenesisHash enc)
  --   firstExceptT (NEByronConfig file)
  --                 $ Byron.mkConfigFromFile (dncRequiresNetworkMagic enc) file genHash
  --
  --Cardano.Chain.Genesis.Config
  --
  -- -- | Construct a 'Config' from an external genesis file.
  -- --
  -- -- The 'FilePath' refers to a canonical JSON file. It will be hashed and
  -- -- checked against the expected hash, which should be known from config.
  -- --
  -- mkConfigFromFile
  --   :: (MonadError ConfigurationError m, MonadIO m)
  --   => RequiresNetworkMagic
  --   -> FilePath
  --   -> Hash Raw  -- ^ The expected hash of the file
  --   -> m Config
  -- mkConfigFromFile rnm fp expectedHash
  --
  --
  -- OK! Turns out there is a much simpler function! see `readConfigFile` below
  --
  --
  config <- readConfigFile configFilePath RequiresMagic

  -- Connect to the node.
  putStrLn $ "Connecting to socket: " <> socketPath
  IPC.connectToLocalNode
    (connectInfo socketPath)
    (protocols config)
  where
  connectInfo :: FilePath -> IPC.LocalNodeConnectInfo IPC.CardanoMode
  connectInfo socketPath =
      IPC.LocalNodeConnectInfo {
        IPC.localConsensusModeParams = IPC.CardanoModeParams (IPC.EpochSlots 21600),
        IPC.localNodeNetworkId       = Mainnet,
        IPC.localNodeSocketPath      = socketPath
      }

  protocols :: Genesis.Config -> IPC.LocalNodeClientProtocolsInMode IPC.CardanoMode
  protocols config =
      IPC.LocalNodeClientProtocols {
        IPC.localChainSyncClient    = Just (Right (chainSyncClient config)),
        IPC.localTxSubmissionClient = Nothing,
        IPC.localStateQueryClient   = Nothing
      }

-- | Defines the client side of the chain sync protocol.
chainSyncClient :: Genesis.Config
                -> ChainSyncClient
                     (IPC.BlockInMode IPC.CardanoMode)
                     ChainPoint
                     ChainTip
                     IO ()
chainSyncClient config = ChainSyncClient $ do
  let initialLedgerState :: LedgerState
      initialLedgerState = Byron.initByronLedgerState
                            config
                            Nothing -- don't override UTxO

      clientStIdle :: Map SlotNo LedgerState -- ^ Known Ledger states. Must be complete up to the current BlockNo.
                   -> IO (ClientStIdle (IPC.BlockInMode IPC.CardanoMode)
                                  ChainPoint ChainTip IO ())
      clientStIdle knownLedgerStates = do
        -- putStrLn "Chain Sync: requesting next"
        return $ SendMsgRequestNext
          -- There's more to get immediately
          (clientStNext knownLedgerStates)

          -- The node is asking us to wait. This is because we reached the
          -- tip. We can certainly carry on here, but for this demo we are
          -- going to stop when we hit the current chain tip.
          clientDone

      clientStNext :: Map SlotNo LedgerState -- ^ Known Ledger states. Must be complete up to the current BlockNo.
                   -> ClientStNext (IPC.BlockInMode IPC.CardanoMode)
                                  ChainPoint ChainTip IO ()
      clientStNext knownLedgerStates =
        ClientStNext {
            recvMsgRollForward = \(IPC.BlockInMode block@(Block (BlockHeader slotNo _ blockNo@(BlockNo blockNoI)) _) era) _tip -> case era of
              ByronEraInCardanoMode -> ChainSyncClient $ do
                let prevLedgerState = maybe initialLedgerState snd  (Map.lookupLT slotNo knownLedgerStates)
                    currLedgerState = applyBlock config prevLedgerState block
                    knownLedgerStates' = Map.insert slotNo currLedgerState knownLedgerStates
                when (blockNoI `mod` 1000 == 0) $ do
                  printLedgerState currLedgerState
                clientStIdle knownLedgerStates'

          , recvMsgRollBackward = \chainPoint _ -> case chainPoint of
              ChainPointAtGenesis -> error "TODO handle ChainPointAtGenesis"
              ChainPoint slotNo _ -> ChainSyncClient $ do
                let (truncatedKnownLedgerStates, _, _) = Map.splitLookup slotNo knownLedgerStates
                clientStIdle truncatedKnownLedgerStates
          }

      -- We're still in the "Next" state here, but we've decided to stop
      -- as soon as we get the reply, no matter which reply.
      clientDone :: IO (ClientStNext (IPC.BlockInMode IPC.CardanoMode)
                                  ChainPoint ChainTip IO ())
      clientDone = do
        putStrLn "Chain Sync: done!"
        return $ ClientStNext {
          recvMsgRollForward  = \_ _ -> ChainSyncClient (pure (SendMsgDone ())),
          recvMsgRollBackward = \_ _ -> ChainSyncClient (pure (SendMsgDone ()))
        }

      printLedgerState :: LedgerState -> IO ()
      printLedgerState _ = return () -- TODO

  clientStIdle Map.empty

--------------------------------------------------------------------------------
-- TODO move this stuff to a new ledger state module in cardano-api and also
-- export it from Cardano.Api
--------------------------------------------------------------------------------

type LedgerState = Byron.LedgerState ByronBlock

applyBlock :: Genesis.Config -> LedgerState -> Block ByronEra -> LedgerState
applyBlock config ledgerState (Block.ByronBlock block) = either (error . show) id
            $ runExcept
            $ tickThenApply -- TODO validation check as is done in `Cardano.DbSync.LedgerState` `applyBlock`
                config
                block
                ledgerState

-- | The easy way to get a `Config`. Coppied from `ouroboros-network/ouroboros-consensus-cardano/tools/db-analyser/Block/Byron.hs`
readConfigFile ::
     FilePath
  -- -> Maybe (Hash Raw)
  -> RequiresNetworkMagic
  -> IO Genesis.Config
readConfigFile configFile requiresNetworkMagic = do
    -- genesisHash <- case mHash of
    --   Nothing -> either (error . show) return =<< runExceptT
    --     (Genesis.unGenesisHash . snd <$> Genesis.readGenesisData configFile)
    --   Just hash -> return hash
    genesisHash <- either (error . show) return =<< runExceptT
      (Genesis.unGenesisHash . snd <$> Genesis.readGenesisData configFile)
    genesisConfig <- either (error . show) return =<< runExceptT
      (Genesis.mkConfigFromFile
        requiresNetworkMagic
        configFile
        genesisHash)
    return genesisConfig

--Ouroboros.Consensus.Byron.Ledger.Ledger
--
-- data instance LedgerState ByronBlock = ByronLedgerState {
--       byronLedgerTipBlockNo :: !(WithOrigin BlockNo)     -- data WithOrigin t = Origin | At !t
--     , byronLedgerState      :: !CC.ChainValidationState
--     , byronLedgerTransition :: !ByronTransition
--     }
--   deriving (Eq, Show, Generic, NoThunks)
--
-- applyByronBlock :: Cardano.Chain.ValidationMode.ValidationMode
--                 -> LedgerConfig ByronBlock
--                 -> ByronBlock
--                 -> TickedLedgerState ByronBlock
--                 -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
--
-- -- | The ticked Byron ledger state
-- data instance Ticked (LedgerState ByronBlock) = TickedByronLedgerState {
--       tickedByronLedgerState        :: !CC.ChainValidationState
--     , untickedByronLedgerTransition :: !ByronTransition
--     }
--   deriving (Generic, NoThunks)
--
-- type instance LedgerCfg (LedgerState ByronBlock) = Cardano.Chain.Genesis.Config

--Ouroboros.Consensus.Ledger.Basics
--
-- type LedgerConfig      blk = LedgerCfg (LedgerState blk)
-- type LedgerError       blk = LedgerErr (LedgerState blk)
-- type TickedLedgerState blk = Ticked    (LedgerState blk)

--Cardano.Chain.ValidationMode
--
-- data ValidationMode = ValidationMode
--     { blockValidationMode :: !BlockValidationMode
--     , txValidationMode    :: !TxValidationMode
--     } deriving (Show)

--Cardano.Chain.Block.Validation
--
-- data ChainValidationState = ChainValidationState
--   { cvsLastSlot        :: !SlotNumber
--   , cvsPreviousHash    :: !(Either GenesisHash HeaderHash)
--   -- ^ GenesisHash for the previous hash of the zeroth boundary block and
--   --   HeaderHash for all others.
--   , cvsUtxo            :: !UTxO
--   , cvsUpdateState     :: !UPI.State
--   , cvsDelegationState :: !DI.State
--   } deriving (Eq, Show, Generic, NFData, NoThunks)



--Ouroboros.Consensus.Byron.Ledger.Ledger
--
-- -- | Information required to determine the transition from Byron to Shelley
-- data ByronTransition =
--     -- | Per candidate proposal, the 'BlockNo' in which it became a candidate
--     --
--     -- The HFC needs to know when a candidate proposal becomes stable. We cannot
--     -- reliably do this using 'SlotNo': doing so would mean that if we were to
--     -- switch to a denser fork, something that was previously deemed stable is
--     -- suddenly not deemed stable anymore (although in actuality it still is).
--     -- We therefore must do this based on 'BlockNo' instead, but unfortunately
--     -- the Byron ledger does not record this information. Therefore, we record
--     -- it here instead.
--     --
--     -- Invariant: the domain of this map should equal the set of candidate
--     -- proposals.
--     ByronTransitionInfo !(Map Update.ProtocolVersion BlockNo)
--   deriving (Eq, Show, Generic, NoThunks)
