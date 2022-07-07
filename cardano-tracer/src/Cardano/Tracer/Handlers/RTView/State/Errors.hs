{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Tracer.Handlers.RTView.State.Errors
  ( ErrorIx
  , ErrorInfo
  , Errors
  , addError
  , getError
  , getErrors
  , getErrorsFilteredBySeverity
  , getErrorsFilteredByText
  , getErrorsSortedBy
  , timeAsc
  , timeDesc
  , severityAsc
  , severityDesc
  , initErrors
  , deleteAllErrors
  , errorsToJSON
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Data.Aeson (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import           Data.List (find, sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Types (NodeId)

import           Cardano.Logging (SeverityS)

type ErrorIx = Int
type ErrorInfo = (ErrorIx, TraceObjectInfo)
type Errors = TVar (Map NodeId [ErrorInfo])

-- | We need it to export errors list to JSON-file.
deriving instance ToJSON SeverityS

initErrors :: IO Errors
initErrors = newTVarIO M.empty

addError
  :: Errors
  -> NodeId
  -> TraceObjectInfo
  -> IO ()
addError errors nodeId trObInfo = atomically $
  modifyTVar' errors $ \currentErrors ->
    case M.lookup nodeId currentErrors of
      Nothing -> do
        let errorIx = 0
        M.insert nodeId [(errorIx, trObInfo)] currentErrors
      Just errorsFromNode -> do
        -- All errors here should be unique, so check it first.
        case find (\(_, trObInfo') -> trObInfo == trObInfo') errorsFromNode of
          Nothing -> do
            -- No such error, add it.
            let errorIx = length errorsFromNode
            M.adjust (const $ errorsFromNode ++ [(errorIx, trObInfo)]) nodeId currentErrors
          Just _ -> currentErrors

deleteAllErrors
  :: Errors
  -> NodeId
  -> IO ()
deleteAllErrors errors nodeId = atomically $
  modifyTVar' errors $ \currentErrors ->
    case M.lookup nodeId currentErrors of
      Nothing -> currentErrors
      Just _ -> M.adjust (const []) nodeId currentErrors

getError
  :: ErrorIx
  -> Errors
  -> NodeId
  -> IO (Maybe ErrorInfo)
getError errorIx errors nodeId =
  getErrors errors nodeId >>= \case
    [] -> return Nothing
    allErrors -> return $ find (\(ix, _) -> ix == errorIx) allErrors

getErrors
  :: Errors
  -> NodeId
  -> IO [ErrorInfo]
getErrors errors nodeId =
  getErrorsHandled errors nodeId id

getErrorsSortedBy
  :: (ErrorInfo -> ErrorInfo -> Ordering)
  -> Errors
  -> NodeId
  -> IO [ErrorInfo]
getErrorsSortedBy ordering errors nodeId =
  getErrorsHandled errors nodeId $ sortBy ordering

timeAsc
  , timeDesc
  , severityAsc
  , severityDesc :: ErrorInfo -> ErrorInfo -> Ordering
timeAsc      (_, (_, _,  ts1)) (_, (_, _,  ts2)) = ts1 `compare` ts2
timeDesc     (_, (_, _,  ts1)) (_, (_, _,  ts2)) = ts2 `compare` ts1
severityAsc  (_, (_, s1, _))   (_, (_, s2, _))   = s1  `compare` s2
severityDesc (_, (_, s1, _))   (_, (_, s2, _))   = s2  `compare` s1

getErrorsFilteredBySeverity
  :: SeverityS
  -> Errors
  -> NodeId
  -> IO [ErrorInfo]
getErrorsFilteredBySeverity severity errors nodeId =
  getErrorsHandled errors nodeId $ filter (\(_, (_, sev, _)) -> sev == severity)

getErrorsFilteredByText
  :: Text
  -> Errors
  -> NodeId
  -> IO [ErrorInfo]
getErrorsFilteredByText textToSearch errors nodeId =
  if T.null textToSearch
    then return []
    else getErrorsHandled errors nodeId $ filter (\(_, (msg, _, _)) -> textToSearch `isInfixOf` msg)

getErrorsHandled
  :: Errors
  -> NodeId
  -> ([ErrorInfo] -> [ErrorInfo])
  -> IO [ErrorInfo]
getErrorsHandled errors nodeId handler = do
  errors' <- readTVarIO errors
  case M.lookup nodeId errors' of
    Nothing -> return []
    Just errorsFromNode -> return $ handler errorsFromNode

errorsToJSON
  :: Errors
  -> NodeId
  -> IO (Maybe Text)
errorsToJSON errors nodeId =
  getErrors errors nodeId >>= \case
    [] -> return Nothing
    errorsFromNode -> do
      let errorsList = [eI | (_ix, eI) <- errorsFromNode]
      return . Just . decodeUtf8 . BSL.toStrict $ encodePretty errorsList
