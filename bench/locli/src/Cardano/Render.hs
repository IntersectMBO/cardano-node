{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Render (module Cardano.Render) where

import Prelude                  (head, id, show)
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
import Cardano.Org
import Cardano.Util


class RenderCDFs a p where
  rdFields :: [Field DSelect p a]

class RenderTimeline a where
  data RTComments a :: Type
  rtFields     :: Run -> [Field ISelect I a]
  rtCommentary :: a -> RTComments a -> [Text]
  rtCommentary _ _ = []

-- | Encapsulate all information necessary to render a column (projection) of
--   a certain projectible (a kind of analysis results):
--     - first parameter encapsulates the projection descriptor
--     - second parameter sets the arity (I vs. CDF I)
--     - third parameter is the projectible indexed by arity
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

renderCentiles :: Int -> [Centile] -> [Text]
renderCentiles wi = fmap (T.take wi . T.pack . printf "%f" . unCentile)

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
    DInt    (cdfProj . ($x) ->ds) -> ds <&> fmap (T.pack.printf "%*d" fWidth)
    DWord64 (cdfProj . ($x) ->ds) -> ds <&> fmap (T.pack.printf "%*d" fWidth)
    DFloat  (cdfProj . ($x) ->ds) -> ds <&> fmap (renderFloatStr fWidth.printf "%*F" fWidth)
    DDeltaT (cdfProj . ($x) ->ds) -> ds <&> fmap (renderFloatStr fWidth.dropWhileEnd (== 's').show)

renderFloatStr :: Int -> String -> Text
renderFloatStr w = T.justifyRight w ' '. T.take w . T.pack . stripLeadingZero
 where
   stripLeadingZero = \case
     '0':xs@('.':_) -> xs
     xs -> xs

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

mapSomeFieldCDF :: forall p c a. (forall b. Divisible b => CDF p b -> c) -> a p -> DSelect p a -> c
mapSomeFieldCDF f a = \case
  DInt    s -> f (s a)
  DWord64 s -> f (s a)
  DFloat  s -> f (s a)
  DDeltaT s -> f (s a)

renderTimeline :: forall (a :: Type). RenderTimeline a => Run -> (Field ISelect I a -> Bool) -> [RTComments a] -> [a] -> [Text]
renderTimeline run flt comments xs =
  concatMap (uncurry fLine) $ zip xs [(0 :: Int)..]
 where
   fLine :: a -> Int -> [Text]
   fLine l i =
     -- 0. a periodic header
     (if i `mod` 33 == 0 then catMaybes [head1, head2] else [])
     -- 1. actual timeline entry
     <> (entry l
     -- 2. per-entry commentary, if any
         : concat (fmap (rtCommentary l) comments))

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

mapRenderCDF :: forall p a. RenderCDFs a p
             => (Field DSelect p a -> Bool) -> Maybe [Centile]
             -> (forall c. Divisible c => p c -> [c])
             -> a p
             -> [[Text]]
mapRenderCDF fieldSelr centiSelr fSampleProps x =
  fields                                    -- list of fields
  <&> renderFieldCentiles x cdfSamplesProps -- for each field, list of per-sample lists of properties
  & ([renderCentiles 6 centiles] :)
  & transpose                               -- for each sample, list of per-field lists of properties
  & fmap (fmap $ T.intercalate " ")
 where
   -- Pick relevant fields:
   fields :: [Field DSelect p a]
   fields = filter fieldSelr rdFields

   -- Pick relevant centiles:
   subsetCenti :: CDF p b -> CDF p b
   subsetCenti = maybe id subsetCDF centiSelr

   centiles :: [Centile]
   centiles = mapSomeFieldCDF
              (centilesCDF . subsetCenti)
              x
              (fSelect $ head rdFields)

   -- Get relevant values: for each selected sample, a list of properties (avg, min, max, avg-/+stddev)
   cdfSamplesProps :: Divisible c => CDF p c -> [[c]]
   cdfSamplesProps = fmap (fSampleProps . snd) . cdfSamples . subsetCenti

data RenderFormat
  = AsJSON
  | AsGnuplot
  | AsOrg
  | AsReport
  | AsPretty
  deriving (Show, Bounded, Enum)

-- | When rendering a CDF-of-CDFs _and_ subsetting the data, how to subset:
data CDF2Aspect
  = OfOverallDataset   -- ^ Overall dataset statistical summary.
  | OfInterCDF         -- ^ Inter-sample (i.e. inter-CDF) stats.
  deriving (Show, Bounded, Enum)

modeFilename :: TextOutputFile -> Text -> RenderFormat -> TextOutputFile
modeFilename orig@(TextOutputFile f) name = \case
  AsJSON    -> orig
  AsGnuplot -> printf f name & TextOutputFile
  AsOrg     -> orig
  AsReport  -> orig
  AsPretty  -> orig

renderAnalysisCDFs :: forall a p. (RenderCDFs a p, KnownCDF p, ToJSON (a p)) => Anchor -> (Field DSelect p a -> Bool) -> CDF2Aspect -> Maybe [Centile] -> RenderFormat -> a p -> [(Text, [Text])]

renderAnalysisCDFs _anchor _fieldSelr _c2a _centileSelr AsJSON x = (:[]) . ("",) . (:[]) . LT.toStrict $
  encodeToLazyText x

renderAnalysisCDFs anchor fieldSelr _c2a _centileSelr AsGnuplot x =
  filter fieldSelr rdFields <&>
  \Field{fId=cdfField} ->
    (,) cdfField $
    "# " <> renderAnchor anchor :
    (mapRenderCDF ((== cdfField) . fId) Nothing (unliftCDFValExtra cdfIx) x
     & fmap (T.intercalate " "))

renderAnalysisCDFs a@Anchor{..} fieldSelr _c2a centileSelr AsOrg x =
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
                          \f@Field{..} -> mapField x (T.take (fWidth + 1) . T.pack . printf "%f" . cdfAverageVal) f
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
   fields = filter fieldSelr rdFields

   restrictCDF :: forall c. CDF p c -> CDF p c
   restrictCDF = maybe id subsetCDF centileSelr

   percSpecs :: [Centile]
   percSpecs = maybe (mapSomeFieldCDF centilesCDF x (fSelect . head $ rdFields @a @p))
                     id
                     centileSelr

renderAnalysisCDFs a@Anchor{..} fieldSelr aspect _centileSelr AsReport x =
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
    { tColHeaders     = fst (hdrsProjs @Integer) -- This is crazy.
    , tExtended       = True
    , tApexHeader     = Just "metric"
    , tColumns        = transpose $
                        fields <&>
                        fmap (T.take 6 . T.pack . printf "%f")
                        . mapField x (snd hdrsProjs)
    , tRowHeaders     = fields <&> fDesc
    , tSummaryHeaders = []
    , tSummaryValues  = []
    , tFormula = []
    }
  }
 where
   fields :: [Field DSelect p a]
   fields = filter fieldSelr rdFields

   hdrsProjs :: forall v. (Divisible v) => ([Text], CDF p v -> [Double])
   hdrsProjs = aspectColHeadersAndProjections aspect

   aspectColHeadersAndProjections :: forall v. (Divisible v)
                                  => CDF2Aspect -> ([Text], CDF p v -> [Double])
   aspectColHeadersAndProjections = \case
     OfOverallDataset ->
       (,)
       ["average", "CoV", "min", "max", "stddev", "size"]
       \c@CDF{..} ->
         let avg = cdfAverageVal c & toDouble in
         [ avg
         , cdfStddev / avg
         , fromRational . toRational $ fst cdfRange
         , fromRational . toRational $ snd cdfRange
         , cdfStddev
         , fromIntegral cdfSize
         ]
     OfInterCDF ->
       (,)
       ["average", "CoV", "min", "max", "stddev", "size"]
       (cdfArity
         (error "Cannot do inter-CDF statistics on plain CDFs")
         (\CDF{cdfAverage} ->
             let avg = cdfAverageVal cdfAverage & toDouble in
             [ avg
             , cdfStddev cdfAverage / avg
             , toDouble . fst $ cdfRange cdfAverage
             , toDouble . snd $ cdfRange cdfAverage
             , cdfStddev cdfAverage
             , fromIntegral $ cdfSize cdfAverage
             ]))

renderAnalysisCDFs a fieldSelr _c2a centiSelr AsPretty x =
  (:[]) . ("",) $
  renderAnchor a
  :  catMaybes [head1, head2]
  <> pLines
  <> sizeAvg
 where
   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields then Nothing
           else Just (renderLineHead1 (uncurry T.take . ((+1) . fWidth &&& fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields then Nothing
           else Just (renderLineHead2 (uncurry T.take . ((+1) . fWidth &&& fHead2)))
   renderLineHead1 = mconcat . ("       ":) . renderLine' (const 0) ((+ 1) . fWidth)
   renderLineHead2 = mconcat . (" %tile ":) . renderLine' fLeftPad  ((+ 1) . fWidth)

   pLines :: [Text]
   pLines = fields
            <&>
            fmap (T.intercalate " ") .
            renderFieldCentilesWidth x cdfSamplesProps
            & ((T.justifyLeft 6 ' ' <$> renderCentiles 6 centiles) :)
            & transpose
            & fmap (T.intercalate " ")

   fields :: [Field DSelect p a]
   fields = filter fieldSelr rdFields

   centiles :: [Centile]
   centiles = mapSomeFieldCDF
              centilesCDF
              x
              (fSelect $ head rdFields)

   cdfSamplesProps :: Divisible c => CDF p c -> [[c]]
   cdfSamplesProps = fmap (pure . unliftCDFVal cdfIx . snd)
                     . cdfSamples
                     . maybe id subsetCDF centiSelr

   sizeAvg :: [Text]
   sizeAvg = fmap (T.intercalate " ")
     [ (T.center 6 ' ' "avg" :) $
       (\f -> flip (renderField fLeftPad fWidth) f $ const $
                mapSomeFieldCDF
                  (fit (fWidth f) .T.pack . printf "%F" . cdfAverageVal)  x (fSelect f))
       <$> fields
     , (T.center 6 ' ' "size" :) $
       (\f -> flip (renderField fLeftPad fWidth) f $ const $
                mapSomeFieldCDF
                  (fit (fWidth f) . T.pack . show . cdfSize)    x (fSelect f))
       <$> fields
     ]

   fit :: Int -> Text -> Text
   fit w t = if T.length t > w &&
                -- Drop all non-floats, and floats with significant digit overflowing:
                maybe True (> w) (T.findIndex (== '.') t)
             then "..."
             else T.take w t

   renderLine' ::
     (Field DSelect p a -> Int) -> (Field DSelect p a -> Int) -> (Field DSelect p a -> Text) -> [Text]
   renderLine' lefPad width rend = renderField lefPad width rend <$> fields
   renderField :: forall f. (f -> Int) -> (f -> Int) -> (f -> Text) -> f -> Text
   renderField _lefPad width rend f =
     --T.replicate (lefPad f) " " <>
     T.center (width f) ' ' (rend f)

   -- populationIndices :: [Int]
   -- populationIndices = [1..maxPopulationSize]
   -- maxPopulationSize :: Int
   -- maxPopulationSize = last . sort $ mapSomeFieldCDF cdfSize x . fSelect <$> rdFields @a @p
