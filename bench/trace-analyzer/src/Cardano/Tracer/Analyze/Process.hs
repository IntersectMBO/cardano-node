{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Analyze.Process
  ( parseAndPreprocess
  , extractNamespace
  ) where

import           Cardano.Tracer.Analyze.Types     (FileDBM (..), LineNumber)

import qualified Data.Aeson                       as AE
import qualified Data.Aeson.KeyMap                as AE
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8            as BS
import           Data.Either                      (partitionEithers)
import           Data.Foldable                    (foldl')
import qualified Data.Map                         as Map
import qualified Data.Text                        as Txt
import qualified Data.Vector                      as V

parseAndPreprocess :: FilePath -> IO FileDBM
parseAndPreprocess fp = do
    bs <- BS.readFile fp
    let jsonOrErrorLines =
          map
            (Atto.parseOnly (AE.json <* Atto.endOfInput))
            (BS.lines bs)
        jsonOrErrorLineswithLineNumbers =
          zipWith
            (\e ln -> case e of
              Left l  -> Left (ln, l)
              Right r -> Right (ln, r))
            jsonOrErrorLines
            [1 .. ]
        (errors, sane) = partitionEithers jsonOrErrorLineswithLineNumbers
        kindDict :: Map.Map Txt.Text [(LineNumber, AE.Value)] =
            foldl' (\dict (ln, val) ->
                  Map.insertWith
                    (++)
                    (extractKey val)
                    [(ln, val)]
                    dict)
               Map.empty
               sane
        valueDict ::  Map.Map LineNumber AE.Value = Map.fromList sane
    pure $ FileDBM (Txt.pack fp) (length sane) kindDict errors valueDict

extractKey :: AE.Value -> Txt.Text
extractKey val =
    case extractKeyKind val of
      Just txt -> txt
      Nothing ->
        case extractNamespace val of
          Just txt -> "NS:" <> txt
          Nothing  -> "Unclassified"

extractNamespace :: AE.Value -> Maybe Txt.Text
extractNamespace val =
    case val of
      AE.Object obj ->
        case AE.lookup "ns" obj of
          Just arr -> case arr of
            AE.Array vec ->
              if not (V.null vec)
                then case V.head vec of
                   AE.String txt -> Just txt
                   _             -> Nothing
                else Nothing
            AE.String txt -> Just txt
            _ -> Nothing
          _ -> Nothing
      _ -> Nothing


extractKeyKind :: AE.Value -> Maybe Txt.Text
extractKeyKind val =
    case val of
      AE.Object obj ->
        case AE.lookup "data" obj of
          Just val' -> case val' of
            AE.Object obj' -> case AE.lookup "kind" obj' of
               Just val'' -> case val'' of
                  AE.String txt -> Just txt
                  _             -> Nothing
               _ -> Nothing
            _ -> Nothing
          _ -> Nothing
      _ -> Nothing
