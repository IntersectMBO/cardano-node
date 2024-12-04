{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Configuration for lightweight checkpointing.
module Cardano.Node.Protocol.Checkpoints
  ( CheckpointsReadError(..)
  , readCheckpointsMap
  ) where

import           Cardano.Api

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Node.Types
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Cardano
import           Ouroboros.Consensus.Config (CheckpointsMap (..), emptyCheckpointsMap)

import           Control.Exception (IOException)
import           Control.Monad (forM, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Foldable (forM_)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data CheckpointsReadError =
       CheckpointsReadFileError !FilePath !IOException
     | CheckpointsHashMismatch
         !FilePath
         -- | Actual
         !CheckpointsHash
         -- | Expected
         !CheckpointsHash
     | CheckpointsDecodeError !FilePath !String
   deriving Show

instance Error CheckpointsReadError where
  prettyError (CheckpointsReadFileError fp err) =
        "There was an error reading the checkpoints file: "
     <> pshow fp <> " Error: " <> pshow err

  prettyError (CheckpointsHashMismatch fp actual expected) =
        "Hash mismatch for checkpoints file " <> pshow fp <> ": "
     <> "the actual hash is " <> pshow actual <> ", but the expected "
     <> "hash given in the node configuration file is " <> pshow expected

  prettyError (CheckpointsDecodeError fp err) =
        "There was an error parsing the checkpoints file: "
     <> pshow fp <> " Error: " <> pshow err

readCheckpointsMap
  :: NodeCheckpointsConfiguration
  -> ExceptT CheckpointsReadError IO (CheckpointsMap (CardanoBlock StandardCrypto))
readCheckpointsMap NodeCheckpointsConfiguration {
                       npcCheckpointsFile,
                       npcCheckpointsFileHash = mExpectedHash
                     } = case npcCheckpointsFile of
    Nothing -> pure emptyCheckpointsMap
    Just (CheckpointsFile file) -> do
      content <-
        handleIOExceptT (CheckpointsReadFileError file) $ BS.readFile file

      let actualHash = CheckpointsHash $ Crypto.hashWith id content
      forM_ mExpectedHash $ \expectedHash ->
        when (actualHash /= expectedHash) $
          throwError (CheckpointsHashMismatch file actualHash expectedHash)

      WrapCheckpointsMap checkpointsMap <-
        firstExceptT (CheckpointsDecodeError file) $ hoistEither $
          Aeson.eitherDecodeStrict' content
      pure checkpointsMap

newtype WrapCheckpointsMap =
        WrapCheckpointsMap (CheckpointsMap (CardanoBlock StandardCrypto))

instance Aeson.FromJSON WrapCheckpointsMap where
  parseJSON = Aeson.withObject "CheckpointsMap" $ \o -> do
      checkpointList :: [Aeson.Object] <- o Aeson..: "checkpoints"

      checkpoints :: [(BlockNo, HeaderHash (CardanoBlock StandardCrypto))] <-
        forM checkpointList $ \c -> do
          bno <- c Aeson..: "blockNo"
          hash <- parseCardanoHash =<< c Aeson..: "hash"
          pure (bno, hash)

      let duplicates :: Set BlockNo
          duplicates =
            Map.keysSet $ Map.filter (> 1) $ Map.fromListWith (+) $
              (\(bno, _) -> (bno, 1 :: Int)) <$> checkpoints
      unless (Set.null duplicates) $
        fail $ "Duplicate checkpoints for block numbers "
            <> show (Set.toList duplicates)

      pure $ WrapCheckpointsMap $ CheckpointsMap $ Map.fromList checkpoints
    where
      parseCardanoHash
        :: Aeson.Value
        -> Aeson.Parser (HeaderHash (CardanoBlock StandardCrypto))
      parseCardanoHash = Aeson.withText "CheckpointHash" $ \t ->
          case B16.decode $ Text.encodeUtf8 t of
            Right h -> do
              when (BS.length h /= fromIntegral (hashSize p)) $
                fail $ "Invalid hash size for " <> Text.unpack t
              pure $ fromRawHash p h
            Left e  ->
              fail $ "Invalid base16 for " <> Text.unpack t <> ": " <> e
        where
          p = Proxy @(CardanoBlock StandardCrypto)
