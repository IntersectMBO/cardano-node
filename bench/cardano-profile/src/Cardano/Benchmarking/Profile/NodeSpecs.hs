{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- A beta version / direct replacement of `jq` code (not efficient or nice).
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.NodeSpecs (
  nodeSpecs
) where

--------------------------------------------------------------------------------

import           Prelude
import           GHC.Generics
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: cardano-topology.
import qualified Cardano.Benchmarking.Topology.Types as TopologyTypes
-- Package: containers.
import qualified Data.Map.Strict as Map
-- Package: text.
import qualified Data.Text as Text
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

-- JSON object like:
--  {
--    "node-0": {
--      "i": 0,
--      "kind": "pool",
--      "pools": 1,
--      "autostart": true,
--      "name": "node-0",
--      "isProducer": true,
--      "port": 30000,
--      "region": "loopback",
--      "shutdown_on_slot_synced": null,
--      "shutdown_on_block_synced": 3
--    },
--  {
--  ...
data NodeSpec = NodeSpec
  { i :: Integer
  , kind :: NodeSpecKind
  , pools :: Integer
  , autostart :: Bool
  , name :: String
  , isProducer :: Bool
  , port :: Integer
  , region :: TopologyTypes.Location
  , shutdown_on_slot_synced :: Maybe Integer
  , shutdown_on_block_synced :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON NodeSpec

instance Aeson.FromJSON NodeSpec

data NodeSpecKind = BFT | Pool | Proxy | ChaindbServer | Explorer
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON NodeSpecKind where
  toJSON BFT           = Aeson.toJSON ("bft"            :: Text.Text)
  toJSON Pool          = Aeson.toJSON ("pool"           :: Text.Text)
  toJSON Proxy         = Aeson.toJSON ("proxy"          :: Text.Text)
  toJSON ChaindbServer = Aeson.toJSON ("chaindb-server" :: Text.Text)
  toJSON Explorer      = Aeson.toJSON ("explorer"       :: Text.Text)

instance Aeson.FromJSON NodeSpecKind where
  parseJSON = Aeson.withText "NodeSpecKind" $ \t -> case t of
    "bft"            -> return BFT
    "pool"           -> return Pool
    "proxy"          -> return Proxy
    "chaindb-server" -> return ChaindbServer
    "explorer"       -> return Explorer
    _                -> fail $ "Unknown NodeSpecKind: \"" ++ Text.unpack t ++ "\""

--------------------------------------------------------------------------------

nodeSpecs :: Types.Profile -> TopologyTypes.Topology -> Map.Map String NodeSpec
nodeSpecs profile topology =
  Map.fromList $
    map (\n -> (name n, n)) (nodeSpecs' profile topology)

nodeSpecs' :: Types.Profile -> TopologyTypes.Topology -> [NodeSpec]
nodeSpecs' profile topology =
  let n_bfts = Types.n_bft_hosts $ Types.composition profile
      n_pools = Types.n_pool_hosts $ Types.composition profile
      n_singular_pools = Types.n_singular_hosts $ Types.composition profile
      with_proxy =Types.with_proxy $ Types.composition profile
      with_chaindb_server =
        case Types.with_chaindb_server $ Types.composition profile of
          Nothing -> False
          (Just b) -> b
      with_explorer = Types.with_explorer $ Types.composition profile
      bfts =
        map
          (\i' ->
            ( i'
            , BFT
            , 0
            , True
            )
          )
          [
            0
            ..
            n_bfts - 1
          ]
      _pools =
        map
          (\i' ->
            ( i'
            , Pool
            , if i' - n_bfts < n_singular_pools
              then 1
              else (Types.dense_pool_density $ Types.composition profile)
            , True
            )
          )
          [
            n_bfts
            ..
            (n_bfts + n_pools) - 1
          ]
      proxies =
        map
          (\i' ->
            ( i'
            , Proxy
            , 0
            , True
            )
          )
          [
            (n_bfts + n_pools)
            ..
            (n_bfts + n_pools + if with_proxy then 1 else 0) - 1
          ]
      chaindbs =
        map
          (\i' ->
            ( i'
            , ChaindbServer
            , 0
            , True
            )
          )
          [
            (n_bfts + n_pools + if with_proxy then 1 else 0)
            ..
            (n_bfts + n_pools + if with_proxy then 1 else 0 + if with_chaindb_server then 1 else 0) - 1
          ]
      explorers =
        map
          (\i' ->
            ( i'
            , Explorer
            , 0
            , False
            )
          )
          [
            (n_bfts + n_pools + if with_proxy then 1 else 0 + if with_chaindb_server then 1 else 0)
            ..
            (n_bfts + n_pools + if with_proxy then 1 else 0 + if with_chaindb_server then 1 else 0 + if with_explorer then 1 else 0) - 1
          ]
  in 
    map
      (\(i', kind', pools', autostart') ->
        NodeSpec {
          i = i'
        , kind = kind'
        , pools = pools'
        , autostart = autostart'
        , name = "node-" ++ (show i')
        , isProducer = kind' == BFT || kind' == Pool
        , port = 30000 + i'
        , region =
            let nodes = filter
                        (\coreNode -> (toEnum $ TopologyTypes.nodeId coreNode) == i')
                        (TopologyTypes.coreNodes topology)
            in case nodes of
                 (node:_) -> TopologyTypes.region node
                 [] -> error $ "WTF! " ++ (show i')
        , shutdown_on_slot_synced = Types.shutdown_on_slot_synced (Types.node profile)
        , shutdown_on_block_synced = Types.shutdown_on_block_synced (Types.node profile)
        }
      )
      (bfts ++ _pools ++ proxies ++ chaindbs)
    ++
    map
      (\(i', kind', pools', autostart') ->
        NodeSpec {
          i = i'
        , kind = kind'
        , pools = pools'
        , autostart = autostart'
        , name = "explorer"
        , isProducer = False
        , port = 30000 + i'
        , region =
            let nodes = filter
                        (\relayNode -> (toEnum $ TopologyTypes.nodeId relayNode) == i')
                        (TopologyTypes.relayNodes topology)
            in case nodes of
                 (node:_) -> TopologyTypes.region node
                 [] -> error $ "WTF! " ++ (show i')
        , shutdown_on_slot_synced = Types.shutdown_on_slot_synced (Types.node profile)
        , shutdown_on_block_synced = Types.shutdown_on_block_synced (Types.node profile)
        }
      )
      explorers
