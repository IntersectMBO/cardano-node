module Cardano.RTView.NodeState.CSV
    ( mkCSVWithErrorsForHref
    ) where

import           Data.Csv (ToField (..), ToNamedRecord (..), (.=),
                           encodeByName, header, namedRecord)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime (..))
import           Data.Word (Word8)
import           GHC.Base (unsafeChr)

import           Cardano.BM.Data.Severity (Severity (..))

import           Cardano.RTView.NodeState.Types (NodeError (..))

instance ToField UTCTime where
  toField = BSC.pack . show

instance ToField Severity where
  toField = BSC.pack . show

instance ToNamedRecord NodeError where
  toNamedRecord (NodeError ts sev msg _) =
    namedRecord
      [ "Timestamp" .= ts
      , "Severity"  .= sev
      , "Message"   .= msg
      ]

mkCSVWithErrorsForHref :: [NodeError] -> String
mkCSVWithErrorsForHref allErrors = prepareForHref csv
 where
  csv = map w2c . BSL.unpack . encodeByName header' $ allErrors
  header' = header ["Timestamp", "Severity", "Message"]
  w2c :: Word8 -> Char
  w2c = unsafeChr . fromIntegral

prepareForHref :: String -> String
prepareForHref =
    T.unpack
  . T.replace " " "%20"
  . T.replace "," "%2C"
  . T.replace "!" "%21"
  . T.replace "#" "%23"
  . T.replace "$" "%24"
  . T.replace "&" "%26"
  . T.replace "'" "%27"
  . T.replace "(" "%28"
  . T.replace ")" "%29"
  . T.replace "*" "%2A"
  . T.replace "+" "%2B"
  . T.replace "/" "%2F"
  . T.replace ":" "%3A"
  . T.replace ";" "%3B"
  . T.replace "=" "%3D"
  . T.replace "?" "%3F"
  . T.replace "@" "%40"
  . T.replace "[" "%5B"
  . T.replace "]" "%5D"
  . T.pack
