{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-name-shadowing #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use concatMap" -}

--------------------------------------------------------------------------------

import           Prelude hiding (id)

import qualified Data.Aeson as Aeson
-- Package: bytestring.
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Printing as G
import qualified Data.Text.Lazy.IO as T
import           Options.Applicative

import qualified Cardano.Benchmarking.Topology as Topo
import qualified Cardano.Benchmarking.Topology.Projection as Projection

--------------------------------------------------------------------------------

data Cli =
    Make (Topo.CoreNodesParams, FilePath, Maybe FilePath, Bool)
  | ProjectionFor FilePath ProjectionFor

data ProjectionFor =
    BFT Int Int Bool
  | Pool Int Int Bool
  | Explorer Int Int
  | ChaindbServer
  | Proxy
  deriving Show

--------------------------------------------------------------------------------

main :: IO ()
main = do
  cli <- getOpts
  case cli of
    Make (coreNodesParams, topoJson, topoDot, withExplorer) -> do
      let cores = Topo.mkCoreNodes coreNodesParams
      let relays = [
                       Topo.mkExplorer (Topo.AWS Topo.EU_CENTRAL_1) cores
                     | withExplorer
                   ]
      writeTopo cores relays topoJson
      maybe (pure ()) (writeDot cores) topoDot
    (ProjectionFor topologyPath projectionFor) -> do
      eitherTopology <- Aeson.eitherDecodeFileStrict topologyPath
      let topology = case eitherTopology of
                      (Left errorMsg) ->
                        error $ "Not a valid topology: " ++ errorMsg
                      (Right value) -> value
      writeProjectionFor topology projectionFor

--------------------------------------------------------------------------------

-- | Locations from the CLI are parsed first using the "legacy mode" for
-- backward compatibility, in this mode locations have a default AWS region that
-- are the ones cardano-ops is using. The new format is either "loopback" or a
-- supported AWS Region.
cliLocation :: String -> Either String Topo.Location
cliLocation = \case
  -- Legacy mode.
  "LO" -> Right Topo.Loopback
  "AP" -> Right (Topo.AWS Topo.AP_SOUTHEAST_2)
  "EU" -> Right (Topo.AWS Topo.EU_CENTRAL_1)
  "US" -> Right (Topo.AWS Topo.US_EAST_2)
  -- New format.
  str -> Aeson.eitherDecode
    -- Make the string JSON valid by enclosing it with quotes.
    (BSL8.pack $ "\"" ++  str ++ "\"")

getOpts :: IO Cli
getOpts = execParser $ info
  (
       (hsubparser $
          command "make"
          (info
            (Make <$> cliParserMake)
            (  fullDesc
            <> header "make"
            <> progDesc "Create a cluster topology"
            )
          )
       <>
          command "projection-for"
          (info
            (   ProjectionFor
            <$> strOption
                  (  long "topology-input"
                  <> help "Topology file"
                  <> metavar "INPUTFILE"
                  )
            <*> cliParserProjection
            )
            (  fullDesc
            <> header "projection-for"
            <> progDesc "Create an individual topology"
            )
          )
       )
  <**> helper
  )
  (  fullDesc
  <> progDesc "Cardano topology generation for Performance & Tracing"
  <> header "Cardano node topologies tool"
  )

cliParserMake :: Parser (Topo.CoreNodesParams, FilePath, Maybe FilePath, Bool)
cliParserMake =
  let
   coreNodesParamsParser =
     command "line"
     (info
      (Topo.Line
       <$> parseSize
       <*> parseLocation
       <*> parseRoleSelector)
      (progDesc "Line"
                <> fullDesc
                <> header "Generate a line topology"))
     <>
     command "uni-circle"
     (info
       (Topo.UniCircle
        <$> parseSize
        <*> parseLocation
        <*> parseRoleSelector)
       (progDesc "Unidirectional circle"
                 <> fullDesc
                 <> header "Generate a unidirectional circle topology"))
     <>
     command "torus"
     (info
       (Topo.Torus
        <$> parseSize
        <*> some parseLocation
        <*> parseRoleSelector)
       (progDesc "Toroidal mesh"
                 <> fullDesc
                 <> header "Generate a toroidal mesh topology"))
     <>
     command "torus-dense"
     (info
       (Topo.TorusDense
        <$> parseSize
        <*> some parseLocation
        <*> parseRoleSelector)
       (progDesc "Toroidal mesh (dense)"
                 <> fullDesc
                 <> header "Generate a toroidal mesh topology (dense)"))

   parseSize =
     option auto
     ( long "size"
        <> metavar "SIZE"
        <> help "Node count" )

   parseLocation =
     option (eitherReader cliLocation)
     ( long "loc"
       <> help "Region (at least one)"
       <> metavar "LOCNAME" )

   parseRoleSelector =
     roleSelector <$>
     flag False True
     ( long "with-bft-node-0"
       <> help "Include a BFT node-0")

   roleSelector withBft = \case
     -- TODO:  prepare for deprecation of BFT nodes by switching 1 & 0
      1 ->      Just 1  -- Normal pools are just that -- a single pool
      0 -> if withBft
           then Nothing -- The BFT node has no pools
           else Just 1  -- Dense pools are denoted by any amount >1
      _ ->      Just 2
  in
    (,,,)
      <$> subparser coreNodesParamsParser
      <*>  strOption
           (  long "topology-output"
           <> help "Topology file to write"
           <> metavar "OUTFILE"
           )
      <*> optional
         (strOption
         ( long "dot-output"
        <> help "Dot file to write"
        <> metavar "OUTFILE" ))
      <*> flag False True
          ( long "with-explorer"
         <> help "Add an explorer to the topology")

cliParserProjection :: Parser ProjectionFor
cliParserProjection =
  let
     parseBasePort =
       option auto
         (  long "baseport"
         <> metavar "BASEPORT"
         <> help "Base port"
         )
     parseNodeNumber =
       option auto
         (  long "node-number"
         <> short 'i'
         <> metavar "NODENUMBER"
         <> help "Base port"
         )
     parseEnableP2P =
        flag False True
          ( long "enable-p2p"
          <> help "Create a P2P topology"
          )
  in subparser $
         command "bft"
           (info
             (BFT <$> parseNodeNumber <*> parseBasePort <*> parseEnableP2P)
             (  progDesc "BFT"
             <> fullDesc
             <> header "Generate the topology file for a BFT node"
             )
           )
      <> command "pool"
           (info
             (Pool <$> parseNodeNumber <*> parseBasePort <*> parseEnableP2P)
             (  progDesc "Pool"
             <> fullDesc
             <> header "Generate the topology file for a pool node"
             )
           )
      <> command "explorer"
           (info
             (Explorer <$> parseNodeNumber <*> parseBasePort)
             (  progDesc "Explorer"
             <> fullDesc
             <> header "Generate the topology file for an explorer node"
             )
           )
      <> command "chaindb-server"
           (info
             (pure ChaindbServer)
             (  progDesc "ChainDB Server"
             <> fullDesc
             <> header "Generate the topology file for a ChainDB server node"
             )
           )
      <> command "proxy"
           (info
             (pure Proxy)
             (  progDesc "Proxy"
             <> fullDesc
             <> header "Generate the topology file for a proxy node"
             )
           )

--------------------------------------------------------------------------------

--- * To JSON topology
---
writeTopo :: [Topo.Node] -> [Topo.Node] -> FilePath -> IO ()
writeTopo cores relays f = Aeson.encodeFile f (Topo.Topology cores relays)

--------------------------------------------------------------------------------

--- * To Graphviz
---
writeDot :: [Topo.Node] -> FilePath -> IO ()
writeDot topo f =
  T.writeFile f $
    G.renderDot $ G.toDot $
      uncurry (G.graphElemsToDot params) (toGV topo)
 where
   params = G.nonClusteredParams
     { G.globalAttributes =
       [ G.GraphAttrs
         [G.Scale $ G.DVal 5]
       ]
     , G.fmtNode =
       \(_, Topo.Node{..})->
         [ G.FillColor . G.toColorList . (:[]) $
           case nodeId of
             0 -> G.RGB 250 250 150
             1 -> G.RGB 150 250 250
             _ -> locationColor region
         , G.Style [G.SItem G.Filled []]
         ]
     }

toGV :: [Topo.Node] -> ([(String, Topo.Node)], [(String, String, String)])
toGV xs = (,)
  ((\n@Topo.Node{..} -> ("node-" <> show nodeId, n)) <$> xs)
    (concat $
          (\Topo.Node{..} ->
              ("node-" <> show nodeId, , "")
            . ("node-" <>)
            . show <$> producers
          )
      <$>
          xs
    )

locationColor :: Topo.Location -> G.Color
locationColor = \case
  (Topo.AWS Topo.AP_SOUTHEAST_2) -> G.RGB 250 200 200
  (Topo.AWS Topo.EU_CENTRAL_1)   -> G.RGB 200 200 250
  (Topo.AWS Topo.US_EAST_1)      -> G.RGB 200 250 200
  (Topo.AWS Topo.US_EAST_2)      -> G.RGB 200 250 200
  Topo.Loopback                  -> G.RGB 200 200 250

--------------------------------------------------------------------------------

writeProjectionFor :: Topo.Topology -> ProjectionFor -> IO ()
writeProjectionFor topology projectionFor = do
  BSL8.putStrLn $ writeProjectionFor' topology projectionFor

writeProjectionFor' :: Topo.Topology -> ProjectionFor -> BSL8.ByteString
writeProjectionFor' topology (BFT  i basePort p2pEnabled) = writeProjectionForProducer topology i basePort p2pEnabled
writeProjectionFor' topology (Pool i basePort p2pEnabled) = writeProjectionForProducer topology i basePort p2pEnabled
writeProjectionFor' topology (Explorer i basePort) = Aeson.encode $ Projection.projectionExplorer topology i basePort
writeProjectionFor' _        ChaindbServer = "{Producers:[]}" -- ChainDB servers are just that.
writeProjectionFor' _ Proxy = error "Nodes of kind \"proxy\" are not supported, Nix handles this case!"

writeProjectionForProducer :: Topo.Topology -> Int -> Int -> Bool -> BSL8.ByteString
writeProjectionForProducer topology i basePort enableP2P =
  if enableP2P
  then Aeson.encode $ Projection.projectionP2P topology i basePort
  else Aeson.encode $ Projection.projection    topology i basePort
