{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Cardano.Chairman (chairmanTest)
import           Cardano.Chairman.Options
import           Cardano.Node.Types
import           Cardano.Prelude hiding (option)
import           Control.Tracer (stdoutTracer)
import           Options.Applicative

main :: IO ()
main = do
  ChairmanArgs
    { caRunningTime
    , caMinProgress
    , caSocketPaths
    , caConfigYaml
    , caSlotLength
    , caSecurityParam
    , caNetworkMagic
    } <- execParser opts

  nc <- liftIO $ parseNodeConfigurationFP caConfigYaml

  let someNodeClientProtocol = mkNodeClientProtocol $ ncProtocol nc

  chairmanTest
    stdoutTracer
    caSlotLength
    caSecurityParam
    caRunningTime
    caMinProgress
    caSocketPaths
    someNodeClientProtocol
    caNetworkMagic
