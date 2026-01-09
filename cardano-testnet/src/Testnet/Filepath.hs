{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Testnet.Filepath
  ( TmpAbsolutePath(..)
  , makeLogDir
  , makeSocketDir
  , makeSprocket
  , makeTmpBaseAbsPath
  ) where

import           Prelude

import           Data.String (IsString (..))
import           System.FilePath

import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))

import           RIO (Display (..))


makeSprocket
  :: TmpAbsolutePath
  -> String -- ^ node name
  -> Sprocket
makeSprocket tmpAbsPath node
  = Sprocket (makeTmpBaseAbsPath tmpAbsPath) (makeSocketDir tmpAbsPath </> node)

-- TODO rename me: since the introduction of --output-dir in the cardano-testnet
-- executable, this is a directory that can persist after the test ends.
-- Temporary path used at runtime
newtype TmpAbsolutePath = TmpAbsolutePath
  { unTmpAbsPath :: FilePath
  } deriving (Eq, Show, IsString)

instance Display TmpAbsolutePath where
  textDisplay = fromString . unTmpAbsPath

makeTmpRelPath :: TmpAbsolutePath -> FilePath
makeTmpRelPath (TmpAbsolutePath fp) = makeRelative (makeTmpBaseAbsPath (TmpAbsolutePath fp)) fp

makeSocketDir :: TmpAbsolutePath -> FilePath
makeSocketDir fp = makeTmpRelPath fp </> "socket"

makeTmpBaseAbsPath :: TmpAbsolutePath -> FilePath
makeTmpBaseAbsPath (TmpAbsolutePath fp) = addTrailingPathSeparator $ takeDirectory fp

makeLogDir :: TmpAbsolutePath -> FilePath
makeLogDir (TmpAbsolutePath fp) = addTrailingPathSeparator $ fp </> "logs"
