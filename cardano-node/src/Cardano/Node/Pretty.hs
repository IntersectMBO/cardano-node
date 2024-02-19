module Cardano.Node.Pretty
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

import qualified Control.Concurrent.QSem as IO
import           Control.Exception (bracket_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.IO as TextLazy
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

type Ann = AnsiStyle

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putLn :: MonadIO m => Doc AnsiStyle -> m ()
putLn = liftIO . consoleBracket . TextLazy.putStrLn . renderDefault

hPutLn :: MonadIO m => IO.Handle -> Doc AnsiStyle -> m ()
hPutLn h = liftIO . consoleBracket . TextLazy.hPutStr h . renderDefault

renderStringDefault :: Doc AnsiStyle -> String
renderStringDefault =  TextLazy.unpack . renderDefault

renderDefault :: Doc AnsiStyle -> TextLazy.Text
renderDefault =  renderLazy . layoutPretty defaultLayoutOptions

black :: Doc AnsiStyle -> Doc AnsiStyle
black = annotate (color Black)

red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate (color Blue)

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate (color Magenta)

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate (color Cyan)

white :: Doc AnsiStyle -> Doc AnsiStyle
white = annotate (color White)
