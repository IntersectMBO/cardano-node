{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module NewApiStuff
  (
  applyBlock,
  LedgerState,
  initialLedgerState,
  RequiresNetworkMagic(..),

  ) where

import           Cardano.Api
import           Cardano.Api.Block
import qualified Cardano.Api.Block as Block
import           Cardano.Api.Eras (ShelleyLedgerEra)
import qualified Cardano.Api.IPC as IPC
import qualified Cardano.Binary
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update
import           Cardano.Crypto.Hash (ByteString)
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hashing
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import qualified Cardano.Ledger.Era
import           Cardano.Node.Configuration.POM
import           Cardano.Node.Types
import           Control.Monad.Except
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Short
import           Data.SOP.Strict
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import           Ouroboros.Consensus.Byron.Ledger.Conversions as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Node
import qualified Ouroboros.Consensus.Cardano as Cardano
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import qualified Ouroboros.Consensus.Cardano.Node as Cardano
import qualified Ouroboros.Consensus.Config as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract (tickThenApply)
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.Extended as Ledger
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import qualified Ouroboros.Consensus.Shelley.Eras as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Shelley
import qualified Ouroboros.Consensus.Shelley.Protocol as Consensus
import           Ouroboros.Consensus.Util.Counting
--------------------------------------------------------------------------------
-- TODO move this stuff to a new ledger state module in cardano-api and also
-- export it from Cardano.Api
--------------------------------------------------------------------------------

data LedgerState = LedgerState
  { lsConfig :: !(Ledger.ExtLedgerCfg (Cardano.HardForkBlock (Cardano.CardanoEras Cardano.StandardCrypto)))
  , lsState :: !(Ledger.ExtLedgerState (Cardano.CardanoBlock Cardano.StandardCrypto))
  }

applyBlock :: LedgerState -> Block era -> LedgerState
applyBlock (LedgerState config extLS) block = case block of
  ShelleyBlock shelleyEra shelleyBlock -> case shelleyEra of
    ShelleyBasedEraShelley -> go (Cardano.BlockShelley shelleyBlock)
    ShelleyBasedEraAllegra -> go (Cardano.BlockAllegra shelleyBlock)
    ShelleyBasedEraMary -> go (Cardano.BlockMary shelleyBlock)
  ByronBlock byronBlock -> go (Cardano.BlockByron byronBlock)
  where
    go :: Cardano.CardanoBlock Cardano.StandardCrypto -> LedgerState
    go x = LedgerState config
            $ either (error . show) id
            $ runExcept
            $ tickThenApply -- TODO validation check as is done in `Cardano.DbSync.LedgerState` `applyBlock`
                config
                x
                extLS

data GenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
  | GenesisDecodeError !FilePath !Text
  | GenesisConfigError !FilePath !Text
  | TODOError Text

-- | The easy way to get the initial ledger state. Adapte from `ouroboros-network/ouroboros-consensus-cardano/tools/db-analyser/Block/Byron.hs`
initialLedgerState
  :: FilePath -- ^ The config file path e.g. cardano-node/configuration/cardano/mainnet-config.json
  -> IO LedgerState
initialLedgerState configFilePath = fmap (either (error . Text.unpack . renderShelleyGenesisError) id) . runExceptT $ do

  -- Load the configuration file e.g. cardano-node/configuration/cardano/mainnet-config.json
  -- TODO perhaps we can make due with a partial config?
  nodeConfig <- do
    partialConfig <- lift $ parseNodeConfigurationFP (Just (ConfigYamlFilePath configFilePath))
    case makeNodeConfiguration partialConfig of
      Left err -> throwError (GenesisConfigError configFilePath (Text.pack err))
      Right config -> return config

  -- Extract protocol configs
  ( nodeByronProtocolConfiguration
   , nodeShelleyProtocolConfiguration
   , nodeHardForkProtocolConfiguration
   ) <- case ncProtocolConfig nodeConfig of
      NodeProtocolConfigurationCardano a b c -> return (a, b, c)
      _ -> throwError $ GenesisConfigError configFilePath "Byron, Shelley, and HardFork protocol configurations must all be present."

  -- Get the byron config
  genesisByron :: Genesis.Config <- do
    let GenesisFile byronGenesisFilePath = npcByronGenesisFile nodeByronProtocolConfiguration
        requiresNetworkMagic = npcByronReqNetworkMagic nodeByronProtocolConfiguration
    Crypto.UnsafeHash (hashDigest :: ShortByteString) :: Crypto.Hash Crypto.Blake2b_256 ByteString <- case npcByronGenesisFileHash nodeByronProtocolConfiguration of
      Nothing -> throwError (GenesisConfigError configFilePath "Missing ByronGenesisFileHash")
      Just (GenesisHash (h)) -> return h
    let hash' :: Cardano.Crypto.Hashing.Hash Cardano.Binary.Raw -- Cardano.Crypto.Hashing.AbstractHash Crypto.Blake2b_256 Cardano.Binary.Raw
        hash' = Cardano.Crypto.Hashing.unsafeAbstractHashFromShort hashDigest
    firstExceptT (GenesisConfigError byronGenesisFilePath . Text.pack . show)
      $ Genesis.mkConfigFromFile
        requiresNetworkMagic
        byronGenesisFilePath
        hash'




  -- -- TODO something like this??? Taken from cardano-node/cardano-node/src/Cardano/Node/Protocol/Shelley.hs
  -- (shelleyGenesis, shelleyGenesisHash) <-
  --   firstExceptT CardanoProtocolInstantiationErrorShelley $
  --     Shelley.readGenesis npcShelleyGenesisFile
  --                         npcShelleyGenesisFileHash
  -- do
  --   let GenesisFile shellyGenesisFilePath = npcShelleyGenesisFile nodeShelleyProtocolConfiguration

  --   return ()




  genesisShelley :: Cardano.ShelleyGenesis Shelley.StandardShelley <- readGenesis
    (npcShelleyGenesisFile nodeShelleyProtocolConfiguration)
    (npcShelleyGenesisFileHash nodeShelleyProtocolConfiguration)

  let genesisAllegra :: Cardano.ShelleyGenesis Shelley.StandardAllegra
      genesisAllegra = Cardano.Ledger.Era.translateEra' () genesisShelley

      genesisMary :: Cardano.ShelleyGenesis Shelley.StandardMary
      genesisMary = Cardano.Ledger.Era.translateEra' () genesisAllegra

      kByron :: Consensus.SecurityParam
      kByron = Byron.genesisSecurityParam genesisByron

      historyShape :: History.Shape (Cardano.CardanoEras Cardano.StandardCrypto)
      historyShape = History.Shape $ Exactly $
            K (Byron.byronEraParams     genesisByron)
          :* K (Shelley.shelleyEraParams genesisShelley)
          :* K (Shelley.shelleyEraParams genesisAllegra) -- genesisAllegra
          :* K (Shelley.shelleyEraParams genesisMary) -- genesisMary
          :* Nil









      -- Byron
      Consensus.ProtocolInfo {
          pInfoConfig = topLevelConfigByron@Consensus.TopLevelConfig {
              topLevelConfigProtocol = consensusConfigByron
            , topLevelConfigLedger   = ledgerConfigByron
            , topLevelConfigBlock    = blockConfigByron
            }
        , pInfoInitLedger = initExtLedgerStateByron
        } = Ouroboros.Consensus.Byron.Node.protocolInfoByron (Genesis.gdProtocolParameters $ Genesis.configGenesisData genesisByron)

      -- partialConsensusConfigByron :: PartialConsensusConfig (BlockProtocol ByronBlock)
      -- partialConsensusConfigByron = consensusConfigByron








      extLedgerConfig :: Ledger.ExtLedgerCfg (Cardano.CardanoBlock Cardano.StandardCrypto)
      extLedgerConfig = Ledger.ExtLedgerCfg $ Consensus.TopLevelConfig

        -- NOTE: look at ouroboros-network/ouroboros-consensus-cardano/src/Ouroboros/Consensus/Cardano/Node.hs   protocolInfoCardano

        { topLevelConfigProtocol =
            Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkConsensusConfig
            { hardForkConsensusConfigK = kByron
            , hardForkConsensusConfigShape = historyShape -- :: !(History.Shape xs)
            , hardForkConsensusConfigPerEra = Ouroboros.Consensus.HardFork.Combinator.PerEraConsensusConfig
                (  Ouroboros.Consensus.HardFork.Combinator.WrapPartialConsensusConfig partialConsensusConfigByron
                :* Ouroboros.Consensus.HardFork.Combinator.WrapPartialConsensusConfig partialConsensusConfigShelley
                :* Ouroboros.Consensus.HardFork.Combinator.WrapPartialConsensusConfig partialConsensusConfigAllegra
                :* Ouroboros.Consensus.HardFork.Combinator.WrapPartialConsensusConfig partialConsensusConfigMary
                :* Nil
                )
            }
            -- :: !(ConsensusConfig (BlockProtocol blk))  -- data family
            -- ~  ConsensusConfig (Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkProtocol [..]) -- instance ConsensusConfig (HardForkProtocol xs)
        , topLevelConfigLedger   = undefined
            -- :: !(LedgerConfig blk) -- type family
            -- ~  Cardano.HardForkLedgerConfig [...]
        , topLevelConfigBlock    = undefined
            -- :: !(BlockConfig blk) -- data family
            -- ~
        , topLevelConfigCodec    = undefined
            -- :: !(CodecConfig blk) -- data family
            -- ~
        , topLevelConfigStorage  = undefined
            -- :: !(StorageConfig blk) -- data family
            -- ~
        }

      initialExtLedgerState :: Ledger.ExtLedgerState (Cardano.CardanoBlock Cardano.StandardCrypto)
      initialExtLedgerState = undefined

  return $ LedgerState extLedgerConfig initialExtLedgerState

-- NOTE copied from cardano-node/src/Cardano/Node/Protocol/Shelley.hs, but the
-- genesis hash at that source is made optional.
readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisError IO (Cardano.ShelleyGenesis Shelley.StandardShelley)
readGenesis (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (const $ TODOError "GenesisReadError file") $
                 BS.readFile file
    let genesisHash = GenesisHash (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (const $ TODOError "GenesisDecodeError file") $ hoistEither $
                 Aeson.eitherDecodeStrict' content
    return genesis
  where
    checkExpectedGenesisHash :: GenesisHash
                             -> ExceptT GenesisError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> throwError (TODOError "GenesisHashMismatch actual expected")
        _ -> return ()

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

renderShelleyGenesisError :: GenesisError -> Text
renderShelleyGenesisError sge =
    case sge of
      GenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      GenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Shelley genesis file: the actual hash is ", renderHash actual
          , ", but the expected Shelley genesis hash given in the node "
          , "configuration file is ", renderHash expected, "."
          ]

      GenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      GenesisConfigError fp err ->
        mconcat
          [ "There was an error with the config file: ", Text.pack fp
          , " Error: ", err
          ]

      TODOError err ->
        mconcat
          [ "TODOError: ", err
          ]
  where
    renderHash :: GenesisHashShelley -> Text
    renderHash (GenesisHashShelley h) = Text.decodeUtf8 $ Base16.encode (Crypto.hashToBytes h)

