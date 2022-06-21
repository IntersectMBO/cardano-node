{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE StrictData #-}
module Cardano.Unlog.Org (module Cardano.Unlog.Org) where

import Cardano.Prelude
import Data.Text                qualified as T


data Org
  = Props
    { oProps     :: [(Text, Text)]
    , oBody      :: [Org]
    , oConstants :: [(Text, Text)]
    }
  | Table
    { tColHeaders     :: [Text]
    , tExtended       :: Bool
    , tApexHeader     :: Maybe Text
    , tRowHeaders     :: [Text]
    , tColumns        :: [[Text]]
    , tSummaryHeaders :: [Text]
    , tSummaryValues  :: [[Text]]
    , tFormula        :: [Text]
    }
  deriving (Show)

render :: Org -> [Text]

render Props{..} =
  ((oProps <> [ ("CONSTANTS", T.intercalate " " $ oConstants <&> \(c, x) -> c <> "=" <> x)
              | not (null oConstants)])
   <&> \(name, value) -> mconcat ["#+", name, ": ", value])
  <>
  (oBody <&> render & mconcat)

render Table{..} =
    tableHLine
  : tableRow jusAllHeaders
  : tableHLine
  : fmap tableRow (transpose jusAllColumns)
  & flip (<>)
    jusAllSummaryLines
  & flip (<>)
    (bool [ "#+TBLFM:" <> (tFormula & T.intercalate "::") ] [] (null tFormula))
 where
   jusAllHeaders :: [Text]
   jusAllHeaders = zipWith (`T.justifyRight` ' ')
                           (rowHdrWidth : colWidths)
                           (fromMaybe " " tApexHeader : tColHeaders)
                   & consIfSpecial "!"

   jusAllColumns :: [[Text]]
   jusAllColumns = (tRowHeaders <&> T.justifyRight rowHdrWidth ' ')
                   : justifySourceColumns tColumns
                   & consIfSpecial specialCol

   jusAllSummaryLines :: [Text]
   jusAllSummaryLines =
     if null tSummaryHeaders then [] else
       tableHLine :
       fmap tableRow (zipWith (:)
                      (tSummaryHeaders <&> T.justifyRight rowHdrWidth ' ')
                      (transpose (justifySourceColumns tSummaryValues))
                       <&> consIfSpecial " ")

   rowHdrWidth :: Int
   rowHdrWidth = maximum $ length <$> (maybeToList tApexHeader
                                       <> tRowHeaders
                                       <> tSummaryHeaders)

   justifySourceColumns :: [[Text]] -> [[Text]]
   justifySourceColumns = zipWith (\w-> fmap (T.justifyRight w ' ')) colWidths

   colWidths :: [Int]
   colWidths = maximum . fmap length <$>
               (tColumns
                & zipWith (:)  tColHeaders
                & if null tSummaryValues then identity
                  else zipWith (<>) tSummaryValues)

   specialCol :: [Text]
   specialCol = replicate (length tRowHeaders) "#"

   consIfSpecial :: a -> [a] -> [a]
   consIfSpecial x = bool  identity        (x:)  tExtended

   tableRow :: [Text] -> Text
   tableRow xs = "| " <> T.intercalate " | " xs <> " |"
   tableHLine :: Text
   tableHLine = ("|-" <>) . (<> "-|") . T.intercalate "-+-" . (flip T.replicate "-" <$>) $
                rowHdrWidth
                : colWidths
                & consIfSpecial 1
