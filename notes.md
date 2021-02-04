# Notes

## How to make a ProtocolInfo (TopLevelConfig/ExtLedgerConfig + initial ExtLedgerState):

```haskell
data ProtocolInfo m b = ProtocolInfo {
        pInfoConfig       :: TopLevelConfig b
      , pInfoInitLedger   :: ExtLedgerState b -- ^ At genesis
      , pInfoBlockForging :: m [BlockForging m b]
      }
```

```haskell
mkProtocolInfoCardano :: GenesisConfig -> ProtocolInfo IO CardanoBlock
mkProtocolInfoCardano = Consensus.protocolInfo . mkProtocolCardano

mkProtocolCardano :: GenesisConfig -> Protocol m CardanoBlock CardanoProtocol
mkProtocolCardano = "Converts a GenesisConfig to a CardanoProtocol"


-- So we need a `GenesisConfig`. How does db-sync create this?


readCardanoGenesisConfig
        :: DbSyncNodeConfig
        -> ExceptT DbSyncNodeError IO GenesisConfig
readCardanoGenesisConfig enc =
  case dncProtocol enc of
    DbSyncProtocolCardano ->
      GenesisCardano enc
        <$> readByronGenesisConfig enc
        <*> readShelleyGenesisConfig enc

readByronGenesisConfig :: DbSyncNodeConfig -> ExceptT DbSyncNodeError IO Byron.Config
readByronGenesisConfig enc = do

  -- TRUE INPUTS
  let file = unGenesisFile $ dncByronGenesisFile enc
      hash = unGenesisHashByron $ dncByronGenesisHash enc
      requiresNetMagic = dncRequiresNetworkMagic enc

  genHash <- firstExceptT NEError
                . hoistEither
                $ decodeAbstractHash hash
  firstExceptT (NEByronConfig file)
                $ Byron.mkConfigFromFile requiresNetMagic file genHash


readShelleyGenesisConfig :: DbSyncNodeConfig -> ExceptT DbSyncNodeError IO ShelleyConfig
readShelleyGenesisConfig enc = do

  -- TRUE INPUTS
  let file = unGenesisFile $ dncShelleyGenesisFile enc

  firstExceptT (NEShelleyConfig file . renderShelleyGenesisError)
    $ readGenesis (GenesisFile file) Nothing


-- So it looks like we needed a `DbSyncNodeConfig` but actually we only need a subset:
--
--  * Byron Genesis config File + hash + requiresNetMagic
--  * Shelly config File
--

-- How does db-sync get hold of these things? Via a `DbSyncNodeConfig`.

readDbSyncNodeConfig :: ConfigFile -> IO DbSyncNodeConfig
                    --  ^^^^^^^^^^ Just a FilePath

-- The stuff we actually want (non db-sync related things) comes from cardano-node/configuration/cardano/mainnet-config.json
-- which is parsed with:

instance FromJSON NodeConfig where
    ...

-- It's odd though, because from the db-sync's readDbSyncNodeConfig, it looks like the file is
-- parsed as YAML not JSON. To make things more confusing, the yaml package is based on Aeson
-- and uses the To/FromJSON type class.

-- Note that the paths in the JSON file are relative to that file so may need to
-- be adjusted as is done in db-sync
```

## TODO

So It looks like I just need to use Aeson to parse the NodeConfig file from
cardano-node/configuration/cardano/mainnet-config.json (maybe take this path as
a CLI argument). Then I work my way backwards through the notes above to get to
the TopLevelConfig/ExtLedgerConfig + initial ExtLedgerState


