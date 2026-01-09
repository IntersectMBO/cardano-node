{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Handlers.Shutdown.Config
  ( ShutdownOn (..)
  , parseShutdownOn

  -- * Generalised shutdown handling
  , ShutdownConfig (..)
  )
where


import           Ouroboros.Network.Block (BlockNo (..), SlotNo (..))

import           Control.DeepSeq (NFData)
import           Control.Monad (when)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Foldable (asum)
import           GHC.Generics (Generic)
import qualified Options.Applicative as Opt
import           System.Posix.Types (Fd)
import qualified Text.Read as Read

import           Generic.Data.Orphans ()

data ShutdownOn
  = ASlot  !SlotNo
  | ABlock !BlockNo
  | NoShutdown
  deriving (Generic, Eq, Show)

deriving instance FromJSON ShutdownOn
deriving instance ToJSON ShutdownOn
deriving instance NFData ShutdownOn

parseShutdownOn :: Opt.Parser ShutdownOn
parseShutdownOn = asum
  [ Opt.option (ASlot . SlotNo <$> bounded "SLOT") $ mconcat
    [ Opt.long "shutdown-on-slot-synced"
    , Opt.metavar "SLOT"
    , Opt.help "Shut down the process after ChainDB is synced up to the specified slot"
    , Opt.hidden
    ]
  , Opt.option (ABlock . BlockNo <$> bounded "BLOCK") $ mconcat
    [ Opt.long "shutdown-on-block-synced"
    , Opt.metavar "BLOCK"
    , Opt.help "Shut down the process after ChainDB is synced up to the specified block"
    , Opt.hidden
    ]
  , pure NoShutdown
  ]
  where
    bounded :: forall a. (Bounded a, Integral a, Show a) => String -> Opt.ReadM a
    bounded t = Opt.eitherReader $ \s -> do
      i <- Read.readEither @Integer s
      when (i < fromIntegral (minBound @a)) $ Left $ t <> " must not be less than " <> show (minBound @a)
      when (i > fromIntegral (maxBound @a)) $ Left $ t <> " must not greater than " <> show (maxBound @a)
      pure (fromIntegral i)

data ShutdownConfig
  = ShutdownConfig
    { scIPC         :: !(Maybe Fd)
    , scOnSyncLimit :: !(Maybe ShutdownOn)
    }
    deriving (Eq, Show)

