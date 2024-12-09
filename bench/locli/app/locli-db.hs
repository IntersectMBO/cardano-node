{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}


import           Cardano.Api (BlockNo (..), MonadIO, SlotNo (..))

import           Cardano.Analysis.API.Ground (Hash (..), Host (..), TId (..))
import           Cardano.Logging.Resources.Types (ResourceStats, Resources (..))
import           Cardano.Unlog.LogObject (LOAnyType (..), LOBody (..), LogObject (..), fromTextRef)
import           Cardano.Unlog.LogObjectDB
import           Cardano.Util (smaybe, withTimingInfo)

import           Prelude hiding (log)

import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import           Control.Exception
import           Control.Monad
import           Data.Aeson as Aeson
import           Data.Bifunctor (second)
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Coerce
import           Data.Data
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashMap.Strict as HMS
import           Data.List.Split (chop)
import qualified Data.Map.Lazy as ML
import           Data.Maybe
import qualified Data.Text as TS (Text, empty, intercalate, pack, splitOn, unpack)
import qualified Data.Text.Lazy as TL (Text, fromStrict, pack)
import qualified Data.Text.Short as ShortText (ShortText, empty, fromText, pack, toText)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Data.Typeable
import           Data.Word
import           System.Environment (getArgs)

import           Database.Sqlite.Easy hiding (Text)


main :: IO ()
main = do
  getArgs >>= \case
    []     -> putStrLn "please specify DB file"
    db : _ -> tryRead (fromString db)
dbSummaryDum :: SummaryDB
dbSummaryDum = SummaryDB "tryRead" 0 undefined undefined undefined

tryRead :: ConnectionString -> IO ()
tryRead dbName = do
  (summary, res_) <- withTimingInfo "withDb/selectAll" $
    withDb dbName $
      (,) <$> getSummary <*> run selectAll
  let res = take 84 . drop 4096 $ res_

  mapM_ (\r -> print r >> print (sqlToLogObject summary r)) res

  res2 <- map (sqlToLogObject summary)
    <$> withDb dbName (run selectMempoolTxs)

  -- needs a reducer
  mapM_ (print . second safeLast) (bySlotDomain res2)
  where
    safeLast [] = []
    safeLast xs = [last xs]

selectAll :: SQL
selectAll =
  "SELECT at, cons,                                  slot as arg1, block as arg2, null as arg3, hash as arg4  FROM event \
  \UNION \
  \SELECT at, cons,                                  count,        rejected,      null,         tid           FROM txns \
  \UNION \
  \SELECT at, 'LOResources' as cons,                 null,         null,          null,         as_blob       FROM resource \
  \UNION \
  \SELECT at, 'LOTraceStartLeadershipCheck' as cons, slot,         utxo_size,     chain_dens,   null          FROM slot \
  \ORDER BY at"

getSummary :: SQLite SummaryDB
getSummary =
  (fromSqlDataWithArgs . head) <$> run "SELECT * FROM summary"

bySlotDomain :: [LogObject] -> [(SlotNo, [LogObject])]
bySlotDomain logObjs =
  case dropWhile (isNothing . newSlot) logObjs of
    [] -> []
    xs -> chop go xs
  where
    newSlot LogObject{loBody} = case loBody of { LOTraceStartLeadershipCheck s _ _ -> Just s; _ -> Nothing }

    go ~(lo:los) = let (inSlot, rest) = span (isNothing . newSlot) los in ((fromJust $ newSlot lo, inSlot), rest)

selectMempoolTxs :: SQL
selectMempoolTxs =
  "SELECT at, 'LOTraceStartLeadershipCheck' as cons, slot, utxo_size, chain_dens, null FROM slot \
  \UNION \
  \SELECT at, cons, count as arg1, rejected as arg2, null as arg3, tid as arg4 FROM txns WHERE cons='LOMempoolTxs' \
  \ORDER BY at"
