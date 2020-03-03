{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)
import           Prelude (String)

--import           Control.Monad.Trans.Except.Extra (runExceptT)
--import qualified Options.Applicative as Opt
--import           Options.Applicative (Parser, ParserInfo, ParserPrefs,
--                                      showHelpOnEmpty)
--import           System.Exit (exitFailure)

--import           Cardano.CLI.Parsers
--import           Cardano.CLI.Run
--import           Cardano.Common.TopHandler


main :: IO ()
main = do
    putStrLn ("Hi!" :: String)
