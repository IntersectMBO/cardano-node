{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.Unlog.Render (module Cardano.Unlog.Render) where

import Prelude                  (head, id, last, tail)
import Cardano.Prelude          hiding (head)

import Data.List                (dropWhileEnd)
import Data.Text                qualified as T
import Data.Time.Clock          (NominalDiffTime)

import Data.CDF

import Cardano.Analysis.Run
import Cardano.Util


data RenderMode
  = RenderPretty
  | RenderCsv
  deriving (Eq, Show)

class Show a => RenderDirectCDFs a where
  rdFields :: Run -> [DField a]

class Show a => RenderTimeline a where
  rtFields     :: Run -> [IField a]
  rtCommentary :: a -> [Text]
  rtCommentary _ = []

-- | Incapsulate all information necessary to render a column.
data Field s a
  = Field
  { fWidth   :: Int
  , fLeftPad :: Int
  , fId      :: Text
  , fHead1   :: Text
  , fHead2   :: Text
  , fSelect  :: s a
  }

type DField a = Field DSelect a
type IField a = Field ISelect a

data DSelect a
  = DInt    (a -> DirectCDF Int)
  | DWord64 (a -> DirectCDF Word64)
  | DFloat  (a -> DirectCDF Double)
  | DDeltaT (a -> DirectCDF NominalDiffTime)

data ISelect a
  = IInt    (a -> Int)
  | IWord64 (a -> Word64)
  | IFloat  (a -> Double)
  | IDeltaT (a -> NominalDiffTime)
  | IText   (a -> Text)

mapSomeFieldDirectCDF :: (forall b. DirectCDF b -> c) -> a -> DSelect a -> c
mapSomeFieldDirectCDF f a = \case
  DInt    s -> f (s a)
  DWord64 s -> f (s a)
  DFloat  s -> f (s a)
  DDeltaT s -> f (s a)

renderTimeline :: forall a. RenderTimeline a => Run -> (IField a -> Bool) -> Bool -> [a] -> [Text]
renderTimeline run flt withCommentary xs =
  concatMap (uncurry fLine) $ zip xs [(0 :: Int)..]
 where
   fLine :: a -> Int -> [Text]
   fLine l i = (if i `mod` 33 == 0 then catMaybes [head1, head2] else [])
               <> (entry l : if withCommentary then rtCommentary l else [])

   entry :: a -> Text
   entry v = renderLineDist $
     \Field{..} ->
       case fSelect of
         IInt    (($v)->x) -> T.pack $ printf "%*d" fWidth x
         IWord64 (($v)->x) -> T.pack $ printf "%*d" fWidth x
         IFloat  (($v)->x) -> T.pack $ take fWidth $ printf "%*F" (fWidth - 2) x
         IDeltaT (($v)->x) -> T.pack $ take fWidth $ printf "%-*s" fWidth $ dropWhileEnd (== 's') $ show x
         IText   (($v)->x) -> T.take fWidth . T.dropWhileEnd (== 's') $ x

   fields :: [IField a]
   fields = filter flt $ rtFields run

   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields then Nothing
           else Just (renderLineHead1 (uncurry T.take . ((+1) . fWidth &&& fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields then Nothing
           else Just (renderLineHead2 (uncurry T.take . ((+1) . fWidth &&& fHead2)))

   renderLineHead1 = mconcat . renderLine' (const 0) ((+ 1) . fWidth)
   renderLineHead2 = mconcat . renderLine' fLeftPad  ((+ 1) . fWidth)
   renderLineDist = T.intercalate " " . renderLine' fLeftPad fWidth

   renderLine' ::
     (IField a -> Int) -> (IField a -> Int) -> (IField a -> Text) -> [Text]
   renderLine' lpfn wfn rfn = renderField lpfn wfn rfn <$> fields
   renderField lpfn wfn rfn f = T.replicate (lpfn f) " " <> T.center (wfn f) ' ' (rfn f)

renderDirectCDFs :: forall a. RenderDirectCDFs a =>
     Run -> RenderMode -> (DField a -> Bool) -> Maybe [CentiSpec] -> a
  -> [Text]
renderDirectCDFs run mode flt mCentis x =
  case mode of
    RenderPretty -> catMaybes [head1, head2] <> pLines <> sizeAvg
    RenderCsv    -> headCsv : pLines
     where headCsv = T.intercalate "," $ fId <$> fields

 where
   percSpecs :: [CentiSpec]
   percSpecs = maybe (mapSomeFieldDirectCDF distribCentiSpecs x (fSelect . head $ rdFields run))
                     id
                     mCentis

   -- XXX:  very expensive, the way it's used below..
   subsetDistrib :: DirectCDF b -> DirectCDF b
   subsetDistrib = maybe id subsetCDF mCentis

   pLines :: [Text]
   pLines = fLine <$> [0..(nCentis - 1)]

   fLine :: Int -> Text
   fLine pctIx = (if mode == RenderPretty
                  then renderLineDistPretty
                  else renderLineDistCsv) $
    \Field{..} ->
      let getCapCenti :: forall c. DirectCDF c -> c
          getCapCenti = dCentiIx pctIx
      in T.pack $ case fSelect of
        DInt    (($x)->d) -> (if mode == RenderPretty
                              then printf "%*d" fWidth
                              else printf "%d") (getCapCenti $ subsetDistrib d)
        DWord64 (($x)->d) -> (if mode == RenderPretty
                              then printf "%*d" fWidth
                              else printf "%d") (getCapCenti $ subsetDistrib d)
        DFloat  (($x)->d) -> (if mode == RenderPretty
                              then take fWidth . printf "%*F" (fWidth - 2)
                              else printf "%F") (getCapCenti $ subsetDistrib d)
        DDeltaT (($x)->d) -> (if mode == RenderPretty
                              then take fWidth else id)
                             . dropWhileEnd (== 's') . show $ getCapCenti $ subsetDistrib d

   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields then Nothing
           else Just (renderLineHead1 (uncurry T.take . ((+1) . fWidth &&& fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields then Nothing
           else Just (renderLineHead2 (uncurry T.take . ((+1) . fWidth &&& fHead2)))

   sizeAvg :: [Text]
   sizeAvg = fmap (T.intercalate " ")
     [ (T.center (fWidth (head fields)) ' ' "avg" :) $
       (\f -> flip (renderField fLeftPad fWidth) f $ const $
                mapSomeFieldDirectCDF
                  (T.take (fWidth f) .T.pack . printf "%F" . dAverage)  x (fSelect f))
       <$> tail fields
     , (T.center (fWidth (head fields)) ' ' "size" :) $
       (\f -> flip (renderField fLeftPad fWidth) f $ const $
                mapSomeFieldDirectCDF
                  (T.take (fWidth f) . T.pack . show . dSize)    x (fSelect f))
       <$> tail fields
     ]

   renderLineHead1 = mconcat . renderLine' (const 0) ((+ 1) . fWidth)
   renderLineHead2 = mconcat . renderLine' fLeftPad  ((+ 1) . fWidth)
   renderLineDistPretty = T.intercalate " " . renderLine' fLeftPad fWidth
   renderLineDistCsv    = T.intercalate "," . renderLine' (const 0) (const 0)

   renderLine' ::
     (DField a -> Int) -> (DField a -> Int) -> (DField a -> Text) -> [Text]
   renderLine' lefPad width render = renderField lefPad width render <$> fields
   renderField :: forall f. (f -> Int) -> (f -> Int) -> (f -> Text) -> f -> Text
   renderField lefPad width render f = T.replicate (lefPad f) " " <> T.center (width f) ' ' (render f)

   fields :: [DField a]
   fields = percField : nsamplesField : filter flt (rdFields run)

   percField :: DField a
   percField = Field 6 0 "%tile" "" "%tile" (DFloat $ const percsDistrib)
   nCentis = length $ dCentiles percsDistrib
   percsDistrib = mapSomeFieldDirectCDF
                    (distribCentisAsDistrib . subsetDistrib) x (fSelect $ head (rdFields run))
   distribCentisAsDistrib :: DirectCDF b -> DirectCDF Double
   distribCentisAsDistrib CDF{..} =
     CDF
       (length dCentiles)
       0.5
       0.5
       (head dCentiles & unCentiSpec . fst,
        last dCentiles & unCentiSpec . fst)
       $ (id &&& I . unCentiSpec) . fst <$> dCentiles

   nsamplesField :: DField a
   nsamplesField = Field 6 0 "Nsamp" "" "Nsamp" (DInt $ const nsamplesDistrib)
   nsamplesDistrib = computeCDF percSpecs populationIndices
   populationIndices :: [Int]
   populationIndices = [1..maxPopulationSize]
   maxPopulationSize :: Int
   maxPopulationSize = last . sort $ mapSomeFieldDirectCDF dSize x . fSelect <$> rdFields run

-- * Auxiliaries
--
nChunksEachOf :: Int -> Int -> Text -> [Text]
nChunksEachOf chunks each center =
  T.chunksOf each (T.center (each * chunks) ' ' center)
