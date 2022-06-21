{-# LANGUAGE TypeInType #-}
module Cardano.Unlog.Render (module Cardano.Unlog.Render) where

import Prelude                  (head, id, last, tail, show)
import Cardano.Prelude          hiding (head, show)

import Data.Aeson               (ToJSON)
import Data.Aeson.Text          (encodeToLazyText)
import Data.List                (dropWhileEnd)
import Data.Text                qualified as T
import Data.Text.Lazy           qualified as LT
import Data.Time.Clock          (NominalDiffTime)

import Data.CDF

import Cardano.Analysis.Ground
import Cardano.Analysis.Run
import Cardano.Unlog.Org
import Cardano.Util


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
  , fDesc    :: Text
  }

mapField :: a p -> (forall v. Divisible v => CDF p v -> b) -> Field DSelect p a -> b
mapField x cdfProj Field{..} =
  case fSelect of
    DInt    (cdfProj . ($x) ->r) -> r
    DWord64 (cdfProj . ($x) ->r) -> r
    DFloat  (cdfProj . ($x) ->r) -> r
    DDeltaT (cdfProj . ($x) ->r) -> r

renderFieldCentiles :: a p -> (forall v. Divisible v => CDF p v -> [[v]]) -> Field DSelect p a -> [[Text]]
renderFieldCentiles x cdfProj Field{..} =
  case fSelect of
    DInt    (cdfProj . ($x) ->ds) -> ds <&> fmap (p.printf "%d")
    DWord64 (cdfProj . ($x) ->ds) -> ds <&> fmap (p.printf "%d")
    DFloat  (cdfProj . ($x) ->ds) -> ds <&> fmap (p.printf "%F")
    DDeltaT (cdfProj . ($x) ->ds) -> ds <&> fmap (T.justifyRight fWidth ' '.T.dropWhileEnd (== 's').p.show)
 where p = T.pack

renderFieldCentilesWidth :: a p -> (forall v. Divisible v => CDF p v -> [[v]]) -> Field DSelect p a -> [[Text]]
renderFieldCentilesWidth x cdfProj Field{..} =
  case fSelect of
    DInt    (cdfProj . ($x) ->ds) -> ds <&> fmap (p.printf "%*d" fWidth)
    DWord64 (cdfProj . ($x) ->ds) -> ds <&> fmap (p.printf "%*d" fWidth)
    DFloat  (cdfProj . ($x) ->ds) -> ds <&> fmap (T.take fWidth . p.printf "%*F" fWidth)
    DDeltaT (cdfProj . ($x) ->ds) -> ds <&> fmap (T.take fWidth . T.justifyRight fWidth ' '.T.dropWhileEnd (== 's').p.show)
 where p = T.pack

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

mapRenderCDF :: forall p a. (RenderCDFs a p, KnownCDF p) => (Field DSelect p a -> Bool) -> Maybe [Centile] -> (forall c. Divisible c => p c -> [c]) -> a p -> [[Text]]
mapRenderCDF fieldSelr centiSelr fSampleProps x =
  fields                                    -- list of fields
  <&> renderFieldCentiles x cdfSamplesProps -- for each field, list of per-sample lists of properties
  & transpose                               -- for each sample, list of per-field lists of properties
  & fmap (mapHead $ take 1)                 -- drop all extra properties of the centile field
  & fmap (fmap $ T.intercalate " ")
 where
   -- Pick relevant fields:
   fields :: [Field DSelect p a]
   fields = centiField : filter fieldSelr rdFields

   -- Pick relevant centiles:
   subset :: CDF p b -> CDF p b
   subset = maybe id subsetCDF centiSelr

   -- Get relevant values: for each selected sample, a list of properties (avg, min, max, avg-/+stddev)
   cdfSamplesProps :: Divisible c => CDF p c -> [[c]]
   cdfSamplesProps = fmap (fSampleProps . snd) . cdfSamples . subset

   -- The leftmost index "CDF":  just list the centiles
   centiField :: Field DSelect p a
   centiField = Field 6 0 "centi" "" "centi" (DFloat $ const centiCDF) "Centile"
   centiCDF   = mapSomeFieldDirectCDF (cdfCentilesCDF . subset) x (fSelect $ head rdFields)

data family RenderMode

data instance RenderMode
  = AsJSON
  | AsGnuplot
  | AsOrg
  | AsReport
  | AsPretty
  deriving (Show, Bounded, Enum)

modeFilename :: TextOutputFile -> Text -> RenderMode -> TextOutputFile
modeFilename orig@(TextOutputFile f) name = \case
  AsJSON    -> orig
  AsGnuplot -> printf f name & TextOutputFile
  AsOrg     -> orig
  AsReport  -> orig
  AsPretty  -> orig

renderCDF :: forall a p. (RenderCDFs a p, KnownCDF p, ToJSON (a p)) => Anchor -> (Field DSelect p a -> Bool) -> Maybe [Centile] -> RenderMode -> a p -> [(Text, [Text])]

renderCDF _anchor _fieldSelr _centileSelr AsJSON x = (:[]) . ("",) . (:[]) . LT.toStrict $
  encodeToLazyText x

renderCDF anchor fieldSelr _centileSelr AsGnuplot x =
  filter fieldSelr rdFields <&>
  \Field{fId=cdfField} ->
    (,) cdfField $
    "# " <> renderAnchor anchor :
    (mapRenderCDF ((== cdfField) . fId) Nothing (unliftCDFValExtra cdfIx) x
     & fmap (T.intercalate " "))

renderCDF a@Anchor{..} fieldSelr centileSelr AsOrg x =
  (:[]) . ("",) . render $
  Props
  { oProps = [ ("TITLE",    renderAnchorRuns a )
             , ("SUBTITLE", renderAnchorFiltersAndDomains a)
             , ("DATE",     renderAnchorDate a)
             , ("VERSION",  renderProgramAndVersion aVersion)
             ]
  , oConstants = []
  , oBody = (:[]) $
    Table
    { tColHeaders     = fields <&> fId
    , tExtended       = True
    , tApexHeader     = Just "centile"
    , tColumns        = fields <&> fmap (T.intercalate ":") . renderFieldCentilesWidth x cdfSamplesProps
    , tRowHeaders     = percSpecs <&> T.take 6 . T.pack . printf "%.4f" . unCentile
    , tSummaryHeaders = ["avg", "samples"]
    , tSummaryValues  = [ fields <&>
                          \f@Field{..} -> mapField x (T.take (fWidth + 1) . T.pack . printf "%f" . cdfAverage) f
                        , fields <&>
                          \f@Field{}   -> mapField x (T.pack . printf "%d" . cdfSize) f
                        ] & transpose
    , tFormula = []
    }
  }
 where
   cdfSamplesProps :: Divisible c => CDF p c -> [[c]]
   cdfSamplesProps = fmap (pure . unliftCDFVal cdfIx . snd) . cdfSamples . restrictCDF

   fields :: [Field DSelect p a]
   fields = nsamplesField : filter fieldSelr rdFields

   restrictCDF :: forall c. CDF p c -> CDF p c
   restrictCDF = maybe id subsetCDF centileSelr

   percSpecs :: [Centile]
   percSpecs = maybe (mapSomeFieldDirectCDF centilesCDF x (fSelect . head $ rdFields @a @p))
                     id
                     centileSelr

   nsamplesField :: Field DSelect p a
   nsamplesField = Field 6 0 "Nsamp" "" "Nsamp" (DInt $ const nsamplesDistrib) "Samples upto centile"
   nsamplesDistrib = cdf percSpecs populationIndices
   populationIndices :: [Int]
   populationIndices = [1..maxPopulationSize]
   maxPopulationSize :: Int
   maxPopulationSize = last . sort $ mapSomeFieldDirectCDF cdfSize x . fSelect <$> rdFields @a @p

renderCDF a@Anchor{..} fieldSelr _centileSelr AsReport x =
  (:[]) . ("",) . render $
  Props
  { oProps = [ ("TITLE",    renderAnchorRuns a )
             , ("SUBTITLE", renderAnchorFiltersAndDomains a)
             , ("DATE",     renderAnchorDate a)
             , ("VERSION",  renderProgramAndVersion aVersion)
             ]
  , oConstants = []
  , oBody = (:[]) $
    Table
    { tColHeaders     = ["average", "min", "max", "stddev", "size"]
    , tExtended       = True
    , tApexHeader     = Just "metric"
    , tColumns        = transpose $
                        fields <&>
                        fmap (T.take 6 . T.pack . printf "%f")
                        . mapField x
                        \CDF{..} ->
                          [ cdfAverage
                          , toDouble $ fst cdfRange
                          , toDouble $ snd cdfRange
                          , cdfStddev
                          , fromIntegral cdfSize
                          ]
    , tRowHeaders     = fields <&> fDesc
    , tSummaryHeaders = []
    , tSummaryValues  = []
    , tFormula = []
    }
  }
 where
   fields :: [Field DSelect p a]
   fields = filter fieldSelr rdFields

renderCDF a fieldSelr centiSelr AsPretty x =
  (:[]) . ("",) $
  renderAnchor a
  :  catMaybes [head1, head2]
  <> pLines
  <> sizeAvg
  -- CSV mode used to be:
  -- (T.intercalate "," $ fId <$> fields)
  -- :  pLines
 where
   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields then Nothing
           else Just (renderLineHead1 (uncurry T.take . ((+1) . fWidth &&& fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields then Nothing
           else Just (renderLineHead2 (uncurry T.take . ((+1) . fWidth &&& fHead2)))
   renderLineHead1 = mconcat . renderLine' (const 0) ((+ 1) . fWidth)
   renderLineHead2 = mconcat . renderLine' fLeftPad  ((+ 1) . fWidth)

   percSpecs :: [Centile]
   percSpecs = maybe (mapSomeFieldDirectCDF centilesCDF x (fSelect . head $ rdFields @a @p))
                     id
                     centiSelr

   restrictCDF :: forall c. CDF p c -> CDF p c
   restrictCDF = maybe id subsetCDF centiSelr

   pLines :: [Text]
   pLines = fields
            <&>
            fmap (T.intercalate " ") .
            renderFieldCentilesWidth x cdfSamplesProps
            & transpose
            & fmap (T.intercalate " ")
            -- fLine <$> [0..(nCentis - 1)]

   fields :: [Field DSelect p a]
   fields = percField : nsamplesField : filter fieldSelr rdFields

   cdfSamplesProps :: Divisible c => CDF p c -> [[c]]
   cdfSamplesProps = fmap (pure . unliftCDFVal cdfIx . snd) . cdfSamples . restrictCDF

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

   renderLine' ::
     (Field DSelect p a -> Int) -> (Field DSelect p a -> Int) -> (Field DSelect p a -> Text) -> [Text]
   renderLine' lefPad width rend = renderField lefPad width rend <$> fields
   renderField :: forall f. (f -> Int) -> (f -> Int) -> (f -> Text) -> f -> Text
   renderField _lefPad width rend f =
     --T.replicate (lefPad f) " " <>
     T.center (width f) ' ' (rend f)

   percField :: Field DSelect p a
   percField = Field 6 0 "%tile" "" "%tile" (DFloat $ const percsDistrib) "Centile"
   percsDistrib = mapSomeFieldDirectCDF (cdfCentilesCDF . restrictCDF) x (fSelect $ head rdFields)

   nsamplesField :: Field DSelect p a
   nsamplesField = Field 6 0 "Nsamp" "" "Nsamp" (DInt $ const nsamplesDistrib) "Samples upto centile"
   nsamplesDistrib = cdf percSpecs populationIndices
   populationIndices :: [Int]
   populationIndices = [1..maxPopulationSize]
   maxPopulationSize :: Int
   maxPopulationSize = last . sort $ mapSomeFieldDirectCDF cdfSize x . fSelect <$> rdFields @a @p
