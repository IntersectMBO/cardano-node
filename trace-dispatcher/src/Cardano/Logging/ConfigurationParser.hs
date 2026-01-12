{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Logging.ConfigurationParser
  ( readConfiguration
  , readConfigurationWithDefault
  , configToRepresentation
  ) where

import           Cardano.Logging.Types

import           Control.Applicative ((<|>))
import           Control.Exception (throwIO)
import qualified Data.Aeson as AE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List as List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text, intercalate, splitOn)
import           Data.Yaml

-- -----------------------------------------------------------------------------
-- Configuration file

-- | The external representation of a configuration file
data ConfigRepresentation = ConfigRepresentation {
    traceOptions                      :: OptionsRepresentation
  , traceOptionForwarder              :: Maybe TraceOptionForwarder
  , traceOptionApplicationName        :: Maybe Text
  , traceOptionMetricsPrefix          :: Maybe Text
  , traceOptionPeriodic               :: Map.Map Text Int
  }
  deriving Show

instance AE.FromJSON ConfigRepresentation where
    parseJSON = withObject "TraceDispatcher" $ \obj ->
      parseAsInner obj <|> parseAsTopLevel obj
      where
        parseAsTopLevel obj =
          ConfigRepresentation
              <$> obj .:  "Options"
              <*> obj .:? "Forwarder"
              <*> obj .:? "ApplicationName"
              <*> obj .:? "MetricsPrefix"
              <*> obj .:? "Periodic"        .!= Map.empty
        parseAsInner obj =
          obj .: "TraceDispatcher" >>= parseAsTopLevel

instance AE.ToJSON ConfigRepresentation where
  toJSON ConfigRepresentation{..} = object $
    [ "Options"                  .= traceOptions
    , "Forwarder"                .= traceOptionForwarder
    , "AppicationName"           .= traceOptionApplicationName
    , "MetricsPrefix"            .= traceOptionMetricsPrefix
    ]
    <>
    [ "Periodic" .= traceOptionPeriodic | (not . Map.null) traceOptionPeriodic ]

type OptionsRepresentation = Map.Map Text ConfigOptionRep

-- | In the external configuration representation for configuration files
-- all options for a namespace are part of a record
data ConfigOptionRep = ConfigOptionRep
    { severity      :: Maybe SeverityF
    , detail        :: Maybe DetailLevel
    , backends      :: Maybe [BackendConfig]
    , maxFrequency  :: Maybe Double
    }
  deriving Show

instance AE.FromJSON ConfigOptionRep where
  parseJSON = withObject "ConfigOptionRep" $ \obj ->
    ConfigOptionRep
      <$> obj .:? "severity"
      <*> obj .:? "detail"
      <*> obj .:? "backends"
      <*> obj .:? "maxFrequency"

instance AE.ToJSON ConfigOptionRep where
  toJSON ConfigOptionRep{..} = object $
    catMaybes
      [ ("severity" .=)     <$> severity
      , ("detail" .=)       <$> detail
      , ("backends" .=)     <$> backends
      , ("maxFrequency" .=) <$> maxFrequency 
      ]

instance AE.ToJSON TraceConfig where
  toJSON tc = toJSON (configToRepresentation tc)

-- | Read a configuration file and returns the internal representation
readConfiguration :: FilePath -> IO TraceConfig
readConfiguration = readConfigurationInt id

-- | Read a configuration file and returns the internal representation
-- Uses values which are not in the file fram the defaultConfig
readConfigurationWithDefault :: TraceConfig -> FilePath -> IO TraceConfig
readConfigurationWithDefault defaultConf = readConfigurationInt (mergeWithDefault defaultConf)

readConfigurationInt :: (TraceConfig -> TraceConfig) -> FilePath -> IO TraceConfig
readConfigurationInt modify fp = do
    !fileConf <- either throwIO pure . parseRepresentation =<< BS.readFile fp
    pure $ modify fileConf

mergeWithDefault :: TraceConfig -> TraceConfig -> TraceConfig
mergeWithDefault defaultConf fileConf =
  TraceConfig
    (if (not . Map.null) (tcOptions fileConf)
      then tcOptions fileConf
      else tcOptions defaultConf)
    (tcForwarder fileConf <|> tcForwarder defaultConf)
    (tcApplicationName fileConf <|> tcApplicationName defaultConf)
    (tcMetricsPrefix fileConf <|> tcMetricsPrefix defaultConf)
    (if Map.null (tcPeriodic fileConf)
      then tcPeriodic defaultConf
      else tcPeriodic fileConf
    )

-- | Parse the byteString as external representation and converts to internal
-- representation
parseRepresentation :: ByteString -> Either ParseException TraceConfig
parseRepresentation bs = transform (decodeEither' bs)
  where
    -- these are JSON string aliases for the Haskell namespace root value [] :: [Text]
    isNamespaceRoot :: Text -> Bool
    isNamespaceRoot ns = ns == "" || ns == "_root_"

    transform ::
         Either ParseException ConfigRepresentation
         -> Either ParseException TraceConfig
    transform (Left e)   = Left e
    transform (Right rl) = Right $ transform' emptyTraceConfig rl
    transform' :: TraceConfig -> ConfigRepresentation -> TraceConfig
    transform' TraceConfig {tcOptions=to'} cr =
      let to''  = List.foldl' (\ tci (nsp, opts') ->
                              let ns' = if isNamespaceRoot nsp then [] else splitOn "." nsp
                              in Map.insertWith
                                  (++)
                                  ns'
                                  (toConfigOptions opts')
                                  tci)
                           to' (Map.toList (traceOptions cr))
      in TraceConfig
          to''
          (traceOptionForwarder cr)
          (traceOptionApplicationName cr)
          (traceOptionMetricsPrefix cr)
          (traceOptionPeriodic cr)


    -- | Convert from external to internal representation
    toConfigOptions :: ConfigOptionRep -> [ConfigOption]
    toConfigOptions ConfigOptionRep {..} =
      catMaybes
        [ ConfSeverity <$> severity
        , ConfDetail <$> detail
        , ConfBackend <$> backends
        , ConfLimiter <$> maxFrequency]


-- | Convert from internal to external representation
configToRepresentation :: TraceConfig -> ConfigRepresentation
configToRepresentation traceConfig =
     ConfigRepresentation
        (toOptionRepresentation (tcOptions traceConfig))
        (tcForwarder traceConfig)
        (tcApplicationName traceConfig)
        (tcMetricsPrefix traceConfig)
        (tcPeriodic traceConfig)
  where
    toOptionRepresentation :: Map.Map [Text] [ConfigOption]
                              ->  Map.Map Text ConfigOptionRep
    toOptionRepresentation internalOptMap =
      List.foldl' conversion Map.empty (Map.toList internalOptMap)

    conversion :: Map.Map Text ConfigOptionRep
                -> ([Text],[ConfigOption])
                -> Map.Map Text ConfigOptionRep
    conversion accuMap (ns, options) =
      let nssingle = intercalate "." ns
          optionRep = fromOptions options
      in  Map.insert nssingle optionRep accuMap

    fromOptions :: [ConfigOption] -> ConfigOptionRep
    fromOptions opts =
      ConfigOptionRep
      { severity     = listToMaybe [d | ConfSeverity d <- opts]
      , detail       = listToMaybe [d | ConfDetail d <- opts]
      , backends     = listToMaybe [d | ConfBackend d <- opts]
      , maxFrequency = listToMaybe [d | ConfLimiter d <- opts]
      }
