{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

-- package: base.
import System.Environment (getArgs)
import qualified GHC.Generics as Generics
-- Package: aeson.
import qualified Data.Aeson as Aeson
-- Package: bytestring.
import qualified Data.ByteString.Lazy as BS
-- Package: containers.
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

main :: IO ()
main = do
  (nodesFile:_) <- getArgs
  jsonBs <- BS.readFile nodesFile
  case (Aeson.eitherDecode jsonBs) of
    (Left err) -> do
      putStrLn "JSON parser failed"
      putStrLn err
    -- TODO/FIXME: Check that the key name (Like node-0) is equal to the "name"
    --             key inside the node object ???
    (Right nodesList) -> do
      --print (nodesList::NodesMap)
      putStrLn $ toYaml nodesList

--------------------------------------------------------------------------------

type NodesMap = Map.Map NodeName Node

type NodeName = String

-- | A single node description from the output of `wb profile node-specs PNAME`.
--   For the full list of profile names run: `wb profile list`.
data Node = Node
  {   nodeId  :: Int
    , nodeName :: NodeName
    , nodeKind :: String
    , nodePools :: Int
    , nodeAutostarts :: Bool
    , nodeIsProducer :: Bool
    , nodePort :: Int
  } deriving (Show, Generics.Generic)

-- The current profiles names that are running on the enchmark cluster are
-- The value one is called "k51-5ep-360kTx-4000kU-1000kD-64kbs":
-- "k51-7ep-14kTx-4000kU-1000kD-73kbs-12MUTx-10BStTx-50MUBk-40BStBk-1i-1o--null"

{--
$ wb profile node-specs default-bage
{
  "node-0": {
    "i": 0,
    "kind": "pool",
    "pools": 1,
    "autostart": true,
    "name": "node-0",
    "isProducer": true,
    "port": 30000
  },
...
...
--}

instance Aeson.FromJSON Node where
      parseJSON = Aeson.withObject "Node" $ \v -> do
        nId    <- v Aeson..: "i"
        nKind  <- v Aeson..: "kind"
        nPools <- v Aeson..: "pools"
        nAutoStarts <- v Aeson..: "autostart"
        nName  <- v Aeson..: "name"
        nIsProducer <- v Aeson..: "isProducer"
        nPort <- v Aeson..: "port"
        return $ Node
          {   nodeId = nId
            , nodeName = nName
            , nodeKind = nKind
            , nodePools = nPools
            , nodeAutostarts = nAutoStarts
            , nodeIsProducer = nIsProducer
            , nodePort = nPort
          }

-- Testing how a docker-compose file could be built.
--------------------------------------------------------------------------------

toYaml :: NodesMap -> String
toYaml nodesMap =
     "services:\n"
  ++ (concat (map toYamlService (Map.toList nodesMap)))

toYamlService :: (NodeName, Node) -> String
toYamlService (nName, node) =
     "  " ++ nName ++ ":\n"
  ++ "    ports:\n"
  ++ "      - \"" ++ (show $ nodePort node) ++ ":" ++ (show $ nodePort node) ++ "\"\n"
