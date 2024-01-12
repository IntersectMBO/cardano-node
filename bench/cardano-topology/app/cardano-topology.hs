{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-name-shadowing #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use concatMap" -}

--------------------------------------------------------------------------------

import           Prelude hiding (id)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Printing as G
import qualified Data.Text.Lazy.IO as T
import           Options.Applicative

import qualified Cardano.Benchmarking.Topology as Topo

--------------------------------------------------------------------------------

main :: IO ()
main = do
  (coreNodesParams, topoJson, topoDot, withExplorer) <- execParser cliOpts
  let cores = Topo.mkCoreNodes coreNodesParams
      relays = [
                   Topo.mkExplorer (Topo.AWS Topo.EU_CENTRAL_1) cores
                 | withExplorer
               ]
  writeTopo cores relays topoJson
  maybe (pure ()) (writeDot cores) topoDot

--------------------------------------------------------------------------------

-- | Locations from the CLI are parsed first using the "legacy mode" for
-- backward compatiblity, in this mode locations have a default AWS region that
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
    (LBS.pack $ "\"" ++  str ++ "\"")


cliOpts :: ParserInfo (Topo.CoreNodesParams, FilePath, Maybe FilePath, Bool)
cliOpts = info (cliParser <**> helper)
  ( fullDesc
  <> progDesc "Cardano topology generator"
  <> header "make-topology - generate Cardano node topologies" )
  where
   cliParser :: Parser (Topo.CoreNodesParams, FilePath, Maybe FilePath, Bool)
   cliParser =
     (,,,)
     <$> subparser coreNodesParamsParser
     <*> strOption
        ( long "topology-output"
       <> help "Topology file to write"
       <> metavar "OUTFILE" )
     <*> optional
        (strOption
        ( long "dot-output"
       <> help "Dot file to write"
       <> metavar "OUTFILE" ))
     <*> flag False True
         ( long "with-explorer"
        <> help "Add an explorer to the topology")

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
