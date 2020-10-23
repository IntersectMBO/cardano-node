{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Cardano.Chairman (chairmanTest)
import           Cardano.Chairman.Options
import           Cardano.Node.Configuration.POM (parseNodeConfigurationFP, pncProtocol)
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

  partialNc <- liftIO . parseNodeConfigurationFP $ Just caConfigYaml

  ptcl <- case pncProtocol partialNc of
            Left err -> panic $ "Chairman error: " <> err
            Right protocol -> return protocol

  let someNodeClientProtocol = mkNodeClientProtocol ptcl

  chairmanTest
    stdoutTracer
    caSlotLength
    caSecurityParam
    caRunningTime
    caMinProgress
    caSocketPaths
    someNodeClientProtocol
    caNetworkMagic
