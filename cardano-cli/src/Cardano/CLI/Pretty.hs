module Cardano.CLI.Pretty
  ( Ann,
    putLn,
    hPutLn,
    renderDefault,
    renderStringDefault,

    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
  ) where

import           Control.Exception (bracket_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Function (($), (.))
import           Data.String (String)
import           Prelude (IO)

import qualified Control.Concurrent.QSem as IO
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.IO as LT
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

type Ann = PP.AnsiStyle

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putLn :: MonadIO m => PP.Doc PP.AnsiStyle -> m ()
putLn = liftIO . consoleBracket . LT.putStrLn . renderDefault

hPutLn :: MonadIO m => IO.Handle -> PP.Doc PP.AnsiStyle -> m ()
hPutLn h = liftIO . consoleBracket . LT.hPutStr h . renderDefault

renderStringDefault :: PP.Doc PP.AnsiStyle -> String
renderStringDefault =  TextLazy.unpack . renderDefault

renderDefault :: PP.Doc PP.AnsiStyle -> TextLazy.Text
renderDefault =  PP.renderLazy . PP.layoutPretty PP.defaultLayoutOptions

black :: PP.Doc PP.AnsiStyle -> PP.Doc PP.AnsiStyle
black = PP.annotate (PP.color PP.Black)

red :: PP.Doc PP.AnsiStyle -> PP.Doc PP.AnsiStyle
red = PP.annotate (PP.color PP.Red)

green :: PP.Doc PP.AnsiStyle -> PP.Doc PP.AnsiStyle
green = PP.annotate (PP.color PP.Green)

yellow :: PP.Doc PP.AnsiStyle -> PP.Doc PP.AnsiStyle
yellow = PP.annotate (PP.color PP.Yellow)

blue :: PP.Doc PP.AnsiStyle -> PP.Doc PP.AnsiStyle
blue = PP.annotate (PP.color PP.Blue)

magenta :: PP.Doc PP.AnsiStyle -> PP.Doc PP.AnsiStyle
magenta = PP.annotate (PP.color PP.Magenta)

cyan :: PP.Doc PP.AnsiStyle -> PP.Doc PP.AnsiStyle
cyan = PP.annotate (PP.color PP.Cyan)

white :: PP.Doc PP.AnsiStyle -> PP.Doc PP.AnsiStyle
white = PP.annotate (PP.color PP.White)
