module Chairman.IO.Network.Win32
  ( doesSocketExist
  , adjustSocketPath
  , maxSocketNameLength
  ) where

import           Data.Bool
import           Data.Char
import           Data.Eq
import           Data.Functor
import           Data.Int
import           Data.Semigroup
import           Data.String
import           System.IO (FilePath, IO)

import qualified System.Win32.NamedPipes as W32

doesSocketExist :: FilePath -> IO Bool
doesSocketExist path = W32.waitNamedPipe path 1

-- | Adjust the socket name to be Win32 friendly
adjustSocketPath :: FilePath -> FilePath
adjustSocketPath path = "\\\\.\\pipe" <> dedupBackslash ("\\" <> fmap slackToBack path)
  where slackToBack :: Char -> Char
        slackToBack c = if c == '/' then '\\' else c
        dedupBackslash :: String -> String
        dedupBackslash ('\\':'\\':xs) = dedupBackslash ('\\':xs)
        dedupBackslash (x:xs) = x:dedupBackslash xs
        dedupBackslash [] = []

maxSocketNameLength :: Int
maxSocketNameLength = 255
