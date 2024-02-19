{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Logging.ConfigurationParser
  (
    readConfiguration
  , readConfigurationWithDefault
  , configToRepresentation
  ) where

import           Cardano.Logging.Types

import           Control.Applicative ((<|>))
import           Control.Exception (throwIO)
import qualified Data.Aeson as AE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Text (Text, intercalate, split)
import           Data.Yaml

-- -----------------------------------------------------------------------------
-- Configuration file

-- | The external representation of a configuration file
data ConfigRepresentation = ConfigRepresentation {
    traceOptions                  :: OptionsRepresentation
  , traceOptionForwarder          :: Maybe TraceOptionForwarder
  , traceOptionNodeName           :: Maybe Text
  , traceOptionPeerFrequency      :: Maybe Int
  , traceOptionResourceFrequency  :: Maybe Int
  }
  deriving (Eq, Ord, Show)

instance AE.FromJSON ConfigRepresentation where
    parseJSON (Object obj) = ConfigRepresentation
                           <$> obj .: "TraceOptions"
                           <*> obj .:? "TraceOptionForwarder"
                           <*> obj .:? "TraceOptionNodeName"
                           <*> obj .:? "TraceOptionPeerFrequency"
                           <*> obj .:? "TraceOptionResourceFrequency"
    parseJSON _ = mempty

instance AE.ToJSON ConfigRepresentation where
  toJSON ConfigRepresentation{..} = object
    [ "TraceOptions"                  .= traceOptions
    , "TraceOptionForwarder"          .= traceOptionForwarder
    , "TraceOptionNodeName"           .= traceOptionNodeName
    , "TraceOptionPeerFrequency"      .= traceOptionPeerFrequency
    , "TraceOptionResourceFrequency"  .= traceOptionResourceFrequency
    ]

type OptionsRepresentation = Map.Map Text ConfigOptionRep

-- | In the external configuration representation for configuration files
-- all options for a namespace are part of a record
data ConfigOptionRep = ConfigOptionRep
    { severity :: Maybe SeverityF
    , detail :: Maybe DetailLevel
    , backends :: Maybe [BackendConfig]
    , maxFrequency :: Maybe Double
    }
  deriving (Eq, Ord, Show)

instance AE.FromJSON ConfigOptionRep where
  parseJSON (Object obj) = ConfigOptionRep
                         <$> obj .:? "severity"
                         <*> obj .:? "detail"
                         <*> obj .:? "backends"
                         <*> obj .:? "maxFrequency"
  parseJSON _ = mempty

instance AE.ToJSON ConfigOptionRep where
  toJSON ConfigOptionRep{..} = object (conss [])
    where
      consMay attr = maybe id ((:) . (attr .=))
      conss = consMay "severity" severity
            . consMay "detail" detail
            . consMay "backends" backends
            . consMay "maxFrequency" maxFrequency

instance AE.ToJSON TraceConfig where
  toJSON tc = toJSON (configToRepresentation tc)

-- | Read a configuration file and returns the internal representation
readConfiguration :: FilePath -> IO TraceConfig
readConfiguration fp =
    either throwIO pure . parseRepresentation =<< BS.readFile fp

-- | Read a configuration file and returns the internal representation
-- Uses values which are not in the file fram the defaultConfig
readConfigurationWithDefault :: FilePath -> TraceConfig -> IO TraceConfig
readConfigurationWithDefault fp defaultConf = do
    fileConf <- either throwIO pure . parseRepresentation =<< BS.readFile fp
    pure $ mergeWithDefault fileConf
  where
    mergeWithDefault ::  TraceConfig -> TraceConfig
    mergeWithDefault fileConf =
      TraceConfig
        (if (not . Map.null) (tcOptions fileConf)
          then tcOptions fileConf
          else tcOptions defaultConf)
        (tcForwarder fileConf <|> tcForwarder defaultConf)
        (tcNodeName fileConf <|> tcNodeName defaultConf)
        (tcPeerFrequency fileConf <|> tcPeerFrequency defaultConf)
        (tcResourceFrequency fileConf <|> tcResourceFrequency defaultConf)

-- | Parse the byteString as external representation and converts to internal
-- representation
parseRepresentation :: ByteString -> Either ParseException TraceConfig
parseRepresentation bs = transform (decodeEither' bs)
  where
    transform ::
         Either ParseException ConfigRepresentation
         -> Either ParseException TraceConfig
    transform (Left e)   = Left e
    transform (Right rl) = Right $ transform' emptyTraceConfig rl
    transform' :: TraceConfig -> ConfigRepresentation -> TraceConfig
    transform' TraceConfig {tcOptions=to'} cr =
      let to''  = foldl' (\ tci (nsp, opts') ->
                              let ns' = split (=='.') nsp
                                  ns'' = if ns' == [""] then [] else ns'
                                  ns''' = case ns'' of
                                            "Cardano" : tl -> tl
                                            other -> other
                              in Map.insertWith
                                  (++)
                                  ns'''
                                  (toConfigOptions opts')
                                  tci)
                           to' (Map.toList (traceOptions cr))
      in TraceConfig
          to''
          (traceOptionForwarder cr)
          (traceOptionNodeName cr)
          (traceOptionPeerFrequency cr)
          (traceOptionResourceFrequency cr)

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
        (tcNodeName traceConfig)
        (tcPeerFrequency traceConfig)
        (tcResourceFrequency traceConfig)
  where
    toOptionRepresentation :: Map.Map [Text] [ConfigOption]
                              ->  Map.Map Text ConfigOptionRep
    toOptionRepresentation internalOptMap =
      foldl' conversion Map.empty (Map.toList internalOptMap)

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





