{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
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

class RenderCDFs a p where
  rdFields :: [Field DSelect p a]

class RenderTimeline a where
  rtFields     :: Run -> [Field ISelect I a]
  rtCommentary :: a -> [Text]
  rtCommentary _ = []

-- | Incapsulate all information necessary to render a column.
data Field (s :: (Type -> Type) -> k -> Type) (p :: Type -> Type) (a :: k)
  = Field
  { fWidth   :: Int
  , fLeftPad :: Int
  , fId      :: Text
  , fHead1   :: Text
  , fHead2   :: Text
  , fSelect  :: s p a
  }

data DSelect p a
  = DInt    (a p -> CDF p Int)
  | DWord64 (a p -> CDF p Word64)
  | DFloat  (a p -> CDF p Double)
  | DDeltaT (a p -> CDF p NominalDiffTime)

data ISelect p a
  = IInt    (a -> Int)
  | IWord64 (a -> Word64)
  | IFloat  (a -> Double)
  | IDeltaT (a -> NominalDiffTime)
  | IText   (a -> Text)

mapSomeFieldDirectCDF :: forall p c a. (forall b. CDF p b -> c) -> a p -> DSelect p a -> c
mapSomeFieldDirectCDF f a = \case
  DInt    s -> f (s a)
  DWord64 s -> f (s a)
  DFloat  s -> f (s a)
  DDeltaT s -> f (s a)

renderTimeline :: forall (a :: Type). RenderTimeline a => Run -> (Field ISelect I a -> Bool) -> Bool -> [a] -> [Text]
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

   fields :: [Field ISelect I a]
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
     (Field ISelect I a -> Int) -> (Field ISelect I a -> Int) -> (Field ISelect I a -> Text) -> [Text]
   renderLine' lpfn wfn rfn = renderField lpfn wfn rfn <$> fields
   renderField lpfn wfn rfn f = T.replicate (lpfn f) " " <> T.center (wfn f) ' ' (rfn f)

renderCDFs :: forall p a. (RenderCDFs a p, KnownCDF p) => Anchor -> RenderMode -> (Field DSelect p a -> Bool) -> Maybe [Centile] -> a p
  -> [Text]
renderCDFs a mode flt mCentis x =
  case mode of
    RenderPretty -> renderAnchor a : catMaybes [head1, head2] <> pLines <> sizeAvg
    RenderCsv    -> headCsv : pLines
     where headCsv = T.intercalate "," $ fId <$> fields

 where
   percSpecs :: [Centile]
   percSpecs = maybe (mapSomeFieldDirectCDF centilesCDF x (fSelect . head $ rdFields @a @p))
                     id
                     mCentis

   -- XXX:  very expensive, the way it's used below..
   subsetDistrib :: CDF p b -> CDF p b
   subsetDistrib = maybe id (filterCDF . \cs c -> elem (fst c) cs) mCentis

   pLines :: [Text]
   pLines = fLine <$> [0..(nCentis - 1)]

   ix :: CDFIx p
   ix = cdfIx

   fLine :: Int -> Text
   fLine pctIx = (if mode == RenderPretty
                  then renderLineDistPretty
                  else renderLineDistCsv) $
    \Field{..} ->
      let getCapCentile :: forall c. CDF p c -> p c
          getCapCentile = indexCDF pctIx
      in T.pack $ case fSelect of
        DInt    (($x)->d) -> (if mode == RenderPretty
                              then printf "%*d" fWidth
                              else printf "%d") . unliftCDFVal ix . getCapCentile $ subsetDistrib d
        DWord64 (($x)->d) -> (if mode == RenderPretty
                              then printf "%*d" fWidth
                              else printf "%d") . unliftCDFVal ix . getCapCentile $ subsetDistrib d
        DFloat  (($x)->d) -> (if mode == RenderPretty
                              then take fWidth . printf "%*F" (fWidth - 2)
                              else printf "%F") . unliftCDFVal ix . getCapCentile $ subsetDistrib d
        DDeltaT (($x)->d) -> (if mode == RenderPretty
                              then take fWidth else id)
                             . dropWhileEnd (== 's') . show . unliftCDFVal ix . getCapCentile $ subsetDistrib d

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
                  (T.take (fWidth f) .T.pack . printf "%F" . cdfAverage)  x (fSelect f))
       <$> tail fields
     , (T.center (fWidth (head fields)) ' ' "size" :) $
       (\f -> flip (renderField fLeftPad fWidth) f $ const $
                mapSomeFieldDirectCDF
                  (T.take (fWidth f) . T.pack . show . cdfSize)    x (fSelect f))
       <$> tail fields
     ]

   renderLineHead1 = mconcat . renderLine' (const 0) ((+ 1) . fWidth)
   renderLineHead2 = mconcat . renderLine' fLeftPad  ((+ 1) . fWidth)
   renderLineDistPretty = T.intercalate " " . renderLine' fLeftPad fWidth
   renderLineDistCsv    = T.intercalate "," . renderLine' (const 0) (const 0)

   renderLine' ::
     (Field DSelect p a -> Int) -> (Field DSelect p a -> Int) -> (Field DSelect p a -> Text) -> [Text]
   renderLine' lefPad width render = renderField lefPad width render <$> fields
   renderField :: forall f. (f -> Int) -> (f -> Int) -> (f -> Text) -> f -> Text
   renderField lefPad width render f = T.replicate (lefPad f) " " <> T.center (width f) ' ' (render f)

   fields :: [Field DSelect p a]
   fields = percField : nsamplesField : filter flt rdFields

   percField :: Field DSelect p a
   percField = Field 6 0 "%tile" "" "%tile" (DFloat $ const percsDistrib)
   nCentis = length $ cdfSamples percsDistrib
   percsDistrib = mapSomeFieldDirectCDF
                    (distribCentisAsDistrib . subsetDistrib) x (fSelect $ head rdFields)
   distribCentisAsDistrib :: CDF p b -> CDF p Double
   distribCentisAsDistrib CDF{..} =
     CDF
       (length cdfSamples)
       0.5
       0.5
       (head cdfSamples & unCentile . fst,
        last cdfSamples & unCentile . fst)
       $ (id &&& flip liftCDFVal cdfIx . unCentile) . fst <$> cdfSamples

   nsamplesField :: Field DSelect p a
   nsamplesField = Field 6 0 "Nsamp" "" "Nsamp" (DInt $ const nsamplesDistrib)
   nsamplesDistrib = cdf percSpecs populationIndices
   populationIndices :: [Int]
   populationIndices = [1..maxPopulationSize]
   maxPopulationSize :: Int
   maxPopulationSize = last . sort $ mapSomeFieldDirectCDF cdfSize x . fSelect <$> rdFields @a @p

-- * Auxiliaries
--
nChunksEachOf :: Int -> Int -> Text -> [Text]
nChunksEachOf chunks each center =
  T.chunksOf each (T.center (each * chunks) ' ' center)
