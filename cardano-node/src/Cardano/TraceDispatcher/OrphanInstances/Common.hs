{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.TraceDispatcher.OrphanInstances.Common
  () where

import           Cardano.Prelude


-- import           Prelude (fail)
--
-- import           Data.Aeson
-- import qualified Data.ByteString.Base16 as B16
-- import qualified Data.ByteString.Short as SBS
-- import           Data.Scientific (coefficient)
-- -- import qualified Data.Text as Text
-- import qualified Data.Text.Encoding as Text
-- import           Network.Socket (PortNumber)
--
-- import qualified Cardano.Chain.Update as Update
-- import           Cardano.Slotting.Block (BlockNo (..))
-- import           Ouroboros.Consensus.Byron.Ledger.Block (ByronHash (..))
-- import           Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
-- import           Ouroboros.Network.Block (HeaderHash, Tip (..))
import           Cardano.Logging


-- | A bit of a weird one, but needed because some of the very general
-- consensus interfaces are sometimes instantiated to 'Void', when there are
-- no cases needed.
--
instance LogFormatting Void where
  forMachine _dtal x = case x of {}

instance LogFormatting () where

-- instance FromJSON DetailLevel where
--   parseJSON (String str) = case str of
--     "MinimalVerbosity" -> pure MinimalVerbosity
--     "MaximalVerbosity" -> pure MaximalVerbosity
--     "NormalVerbosity" -> pure NormalVerbosity
--     invalid -> fail $ "Parsing of TracingVerbosity failed, "
--                     <> Text.unpack invalid <> " is not a valid TracingVerbosity"
--   parseJSON invalid  = fail $ "Parsing of TracingVerbosity failed due to type mismatch. "
--                             <> "Encountered: " <> show invalid

-- instance FromJSON PortNumber where
--   parseJSON (Number portNum) = case readMaybe . show $ coefficient portNum of
--     Just port -> pure port
--     Nothing -> fail $ show portNum <> " is not a valid port number."
--   parseJSON invalid  = fail $ "Parsing of port number failed due to type mismatch. "
--                             <> "Encountered: " <> show invalid
--
-- instance FromJSON Update.ApplicationName where
--   parseJSON (String x) = pure $ Update.ApplicationName x
--   parseJSON invalid  =
--     fail $ "Parsing of application name failed due to type mismatch. "
--     <> "Encountered: " <> show invalid

-- instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
--   toJSON TipGenesis = object [ "genesis" .= True ]
--   toJSON (Tip slotNo headerHash blockNo) =
--     object
--       [ "slotNo"     .= slotNo
--       , "headerHash" .= headerHash
--       , "blockNo"    .= blockNo
--       ]
--
-- instance ToJSON (OneEraHash xs) where
--   toJSON (OneEraHash bs) =
--     toJSON . Text.decodeLatin1 . B16.encode . SBS.fromShort $ bs
--
-- deriving newtype instance ToJSON ByronHash
-- deriving newtype instance ToJSON BlockNo

-- instance Transformable Text IO ResourceStats where
--   trTransformer = trStructured
--
-- instance LogFormatting ResourceStats where
--   LogFormatting _dtal stats =
--     case toJSON stats of
--       Object x -> x
--       _ -> mempty
