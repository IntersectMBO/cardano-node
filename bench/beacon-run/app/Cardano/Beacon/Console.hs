module  Cardano.Beacon.Console
        ( ConsoleStyle(..)

        , printFatalAndDie
        , printStyled
        ) where

import           Control.Exception (bracket_)
import qualified System.Console.ANSI as Console
import           System.Exit (exitFailure)


data ConsoleStyle =
    StyleNone
  | StyleWarning
  | StyleInfo
  | StyleFatal
  | StyleEcho


consoleStyle :: ConsoleStyle -> [[Console.SGR]]
consoleStyle StyleNone    = []
consoleStyle StyleWarning = [[Console.SetColor Console.Foreground Console.Vivid Console.Yellow]]
consoleStyle StyleInfo    = [[Console.SetColor Console.Foreground Console.Dull Console.Green]]
consoleStyle StyleFatal   = [[Console.SetColor Console.Foreground Console.Vivid Console.Red]]
consoleStyle StyleEcho    = [[Console.SetColor Console.Foreground Console.Dull Console.Blue]]

printStyled :: ConsoleStyle -> String -> IO ()
printStyled style str =
  case consoleStyle style of
    []    -> putStrLn str
    sgrs  -> bracket_
      (mapM_ Console.setSGR sgrs)
      (Console.setSGR [Console.Reset])
      (putStrLn str)

printFatalAndDie :: String -> IO a
printFatalAndDie msg = do
  printStyled StyleFatal $ "FATAL: " ++ msg
  exitFailure
