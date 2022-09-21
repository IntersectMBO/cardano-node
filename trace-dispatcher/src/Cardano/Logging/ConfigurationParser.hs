{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Cardano.Logging.ConfigurationParser
  (
    readConfiguration
  , readConfigurationWithDefault
  , defaultConfig
  ) where

import           Control.Exception (throwIO)
import qualified Data.Aeson as AE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor
import           Data.List (foldl')
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as Map
import           Data.Text (Text, split)
import           Data.Yaml
import           GHC.Generics


import           Cardano.Logging.Types

defaultConfig :: TraceConfig
defaultConfig = emptyTraceConfig {
  tcOptions = Map.fromList
    [([] :: Namespace,
         [ ConfSeverity (SeverityF (Just Info))
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured]
         ])
    ]
  }

-- -----------------------------------------------------------------------------
-- Configuration file

readConfiguration :: FilePath -> IO TraceConfig
readConfiguration fp =
    either throwIO pure . parseRepresentation =<< BS.readFile fp

readConfigurationWithDefault :: FilePath -> TraceConfig -> IO TraceConfig
readConfigurationWithDefault fp defaultConf = do
    fileConf <- either throwIO pure . parseRepresentation =<< BS.readFile fp
    pure $ mergeWithDefault fileConf defaultConf


mergeWithDefault ::  TraceConfig -> TraceConfig -> TraceConfig
mergeWithDefault fileConf defaultConf =
  TraceConfig
    (mergeOptionsWithDefault (tcOptions fileConf) (tcOptions defaultConf))
    (tcForwarder fileConf)
    (if isJust (tcNodeName fileConf)
        then tcNodeName fileConf
        else tcNodeName defaultConf)
    (if isJust (tcPeerFrequency fileConf)
        then tcPeerFrequency fileConf
        else tcPeerFrequency defaultConf)
    (if isJust (tcResourceFrequency fileConf)
        then tcResourceFrequency fileConf
        else tcResourceFrequency defaultConf)

mergeOptionsWithDefault ::
     Map.Map Namespace [ConfigOption]
  -> Map.Map Namespace [ConfigOption]
  -> Map.Map Namespace [ConfigOption]
mergeOptionsWithDefault fileOpts defaultOpts =
    foldr mergeOptsNs defaultOpts (Map.toList fileOpts)
  where
    mergeOptsNs :: (Namespace,[ConfigOption]) -> Map.Map Namespace [ConfigOption] -> Map.Map Namespace [ConfigOption]
    mergeOptsNs (ns,opts) into =
      case Map.lookup ns into of
        Nothing -> Map.insert ns opts into
        Just currentOpts -> Map.insert ns (mergeOpts opts currentOpts) into

    mergeOpts :: [ConfigOption] ->  [ConfigOption] -> [ConfigOption]
    mergeOpts fromFile fromDefault = foldr mergeOpt fromDefault fromFile

    mergeOpt :: ConfigOption -> [ConfigOption] ->  [ConfigOption]
    mergeOpt (ConfSeverity severityF) configList =
      ConfSeverity severityF : filter (\case
                                            ConfSeverity _ -> False
                                            _ -> True) configList
    mergeOpt (ConfDetail detailLevel) configList =
      ConfDetail detailLevel : filter (\case
                                            ConfDetail _ -> False
                                            _ -> True) configList
    mergeOpt (ConfBackend backendConfig) configList =
      ConfBackend backendConfig : filter (\case
                                            ConfBackend _ -> False
                                            _ -> True) configList
    mergeOpt (ConfLimiter maxFrequency) configList =
      if maxFrequency /= 0.0
        then ConfLimiter maxFrequency : filter (\case
                                            ConfLimiter _ -> False
                                            _ -> True) configList
        else filter (\case
                      ConfLimiter _ -> False
                      _ -> True) configList

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

data ConfigRepresentation = ConfigRepresentation {
    traceOptions                :: OptionsRepresentation
  , traceOptionForwarder        :: TraceOptionForwarder
  , traceOptionNodeName         :: Maybe Text
  , traceOptionPeerFrequency     :: Maybe Int
  , traceOptionResourceFrequency :: Maybe Int
  }
  deriving (Eq, Ord, Show,Generic)

type OptionsRepresentation = Map.Map Text ConfigOptionRep

instance AE.FromJSON ConfigRepresentation where
    parseJSON (Object obj) = ConfigRepresentation
                           <$> obj .: "TraceOptions"
                           <*> (obj .:? "TraceOptionForwarder" <&> fromMaybe defaultForwarder)
                           <*> obj .:? "TraceOptionNodeName"
                           <*> obj .:? "TraceOptionPeerFrequency"
                           <*> obj .:? "TraceOptionResourceFrequency"

data ConfigOptionRep = ConfigOptionRep
    { severity :: Maybe SeverityF
    , detail :: Maybe DetailLevel
    , backends :: Maybe [BackendConfig]
    , maxFrequecy :: Maybe Double
    }
  deriving (Eq, Ord, Show,Generic)

instance AE.FromJSON ConfigOptionRep where
  parseJSON (Object obj) = ConfigOptionRep
                         <$> obj .:? "severity"
                         <*> obj .:? "detail"
                         <*> obj .:? "backends"
                         <*> obj .:? "maxFrequency"

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
  case maxFrequecy of
    Nothing -> []
    Just lim -> [ConfLimiter lim]
