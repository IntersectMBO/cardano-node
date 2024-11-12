import           Cardano.Api (SlotNo (..))

import           Cardano.Unlog.BackendDB
import           Cardano.Unlog.LogObject (LOBody (..), LogObject (..))
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util

import           Prelude hiding (log)

import           Data.Bifunctor (second)
import           Data.List.Split (chop)
import           Data.Maybe
import           System.Environment (getArgs)

import           Database.Sqlite.Easy hiding (Text)


main :: IO ()
main = do
  getArgs >>= \case
    []     -> putStrLn "please specify DB file"
    db : _ -> runDB $ fromString db

-- sample case:
-- we want to know the txns in mempool for each slot

runDB :: ConnectionString -> IO ()
runDB dbName = do
  (summary, res2) <-
    withTimingInfo "withDb/selectMempoolTxs" $
      withDb dbName $
        (,) <$> getSummary <*> run selectMempoolTxs

  let logObjects = map (sqlToLogObject summary) res2

  -- TODO: needs a reducer
  mapM_ (print . second safeLast) (bySlotDomain logObjects)
  where
    safeLast [] = []
    safeLast xs = [last xs]

bySlotDomain :: [LogObject] -> [(SlotNo, [LogObject])]
bySlotDomain logObjs =
  case dropWhile (isNothing . newSlot) logObjs of
    [] -> []
    xs -> chop go xs
  where
    newSlot LogObject{loBody} = case loBody of { LOTraceStartLeadershipCheck s _ _ -> Just s; _ -> Nothing }

    go (lo:los) = let (inSlot, rest) = span (isNothing . newSlot) los in ((fromJust $ newSlot lo, inSlot), rest)
    go []       = error "bySlotDomain/chop: empty list"

selectMempoolTxs :: SQL
selectMempoolTxs = sqlOrdered
  [ sqlGetSlot
  , sqlGetTxns `sqlAppend` "WHERE cons='LOMempoolTxs'"
  ]
