{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Logging.ConfigurationParser
  (
    readConfiguration
  , readConfigurationWithDefault
  , configToRepresentation
  ) where

import           Control.Exception (throwIO)
import qualified Data.Aeson as AE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Text (Text, split, intercalate)
import           Data.Yaml
import           GHC.Generics

import           Cardano.Logging.Types

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
    , "traceOptionNodeName"           .= traceOptionNodeName
    , "TraceOptionPeerFrequency"      .= traceOptionPeerFrequency
    , "traceOptionResourceFrequency"  .= traceOptionResourceFrequency
    ]

-- | In the external configuration representation for configuration files
-- all options for a namespace are part of a record
data ConfigOptionRep = ConfigOptionRep
    { severity :: Maybe SeverityF
    , detail :: Maybe DetailLevel
    , backends :: Maybe [BackendConfig]
    , maxFrequency :: Maybe Double
    }
  deriving (Eq, Ord, Show, Generic)

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

type OptionsRepresentation = Map.Map Text ConfigOptionRep

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
        (if isJust (tcForwarder fileConf)
            then tcForwarder fileConf
            else tcForwarder defaultConf)
        (if isJust (tcNodeName fileConf)
            then tcNodeName fileConf
            else tcNodeName defaultConf)
        (if isJust (tcPeerFrequency fileConf)
            then tcPeerFrequency fileConf
            else tcPeerFrequency defaultConf)
        (if isJust (tcResourceFrequency fileConf)
            then tcResourceFrequency fileConf
            else tcResourceFrequency defaultConf)

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
      case severity of
        Nothing -> []
        Just sev -> [ConfSeverity sev]
      ++
      case detail of
        Nothing -> []
        Just dtl -> [ConfDetail dtl]
      ++
      case backends of
        Nothing -> []
        Just bcks -> [ConfBackend bcks]
      ++
      case maxFrequency of
        Nothing -> []
        Just lim -> [ConfLimiter lim]


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
      { severity = case filter (\case
                          ConfSeverity _ -> True
                          _ -> False) opts of
                      ConfSeverity sev : _ -> Just sev
                      _ -> Nothing
      , detail   = case filter (\case
                          ConfDetail _ -> True
                          _ -> False) opts of
                      ConfDetail det : _ -> Just det
                      _ -> Nothing
      , backends = case filter (\case
                          ConfBackend _ -> True
                          _ -> False) opts of
                      ConfBackend back : _ -> Just back
                      _ -> Nothing
      , maxFrequency = case filter (\case
                          ConfLimiter _ -> True
                          _ -> False) opts of
                      ConfLimiter freq : _ -> Just freq
                      _ -> Nothing
      }




