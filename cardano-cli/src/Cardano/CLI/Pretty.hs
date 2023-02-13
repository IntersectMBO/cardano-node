module Cardano.CLI.Pretty
  ( Ann(..),
    putLn,
    hPutLn,
    renderDefault,
    renderStringDefault,
  ) where

import           Control.Exception (bracket_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Eq (Eq)
import           Data.Function (($), (.))
import           Data.String (String)
import           Prelude (IO)
import           Text.Show (Show)

import qualified Control.Concurrent.QSem as IO
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.IO as LT
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

data Ann = Ann deriving (Eq, Show)

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putLn :: MonadIO m => PP.Doc Ann -> m ()
putLn = liftIO . consoleBracket . LT.putStrLn . renderDefault

hPutLn :: MonadIO m => IO.Handle -> PP.Doc Ann -> m ()
hPutLn h = liftIO . consoleBracket . LT.hPutStr h . renderDefault

renderStringDefault :: PP.Doc ann -> String
renderStringDefault =  TextLazy.unpack . PP.renderLazy . PP.layoutPretty PP.defaultLayoutOptions { PP.layoutPageWidth = PP.Unbounded }

renderDefault :: PP.Doc ann -> TextLazy.Text
renderDefault =  PP.renderLazy . PP.layoutPretty PP.defaultLayoutOptions { PP.layoutPageWidth = PP.Unbounded }
