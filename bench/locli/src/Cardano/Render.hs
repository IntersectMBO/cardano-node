{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

{- HLINT ignore "Use concatMap" -}
{- HLINT ignore "Use fromMaybe" -}

module Cardano.Render (module Cardano.Render) where

import Prelude                          (id, show)
import Cardano.Prelude                  hiding (head, show)

import Data.Aeson.Text                  (encodeToLazyText)
import Data.List                        (dropWhileEnd)
import Data.Text                        qualified as T
import Data.Text.Lazy                   qualified as LT
import Options.Applicative              qualified as Opt

import Data.CDF

import Cardano.Org
import Cardano.Util
import Cardano.Analysis.API


-- | Explain the poor human a little bit of what was going on:
data Anchor
  = Anchor
  { aRuns    :: [Text]
  , aFilters :: ([FilterName], [ChainFilter])
  , aSlots   :: Maybe (DataDomain SlotNo)
  , aBlocks  :: Maybe (DataDomain BlockNo)
  , aVersion :: LocliVersion
  , aWhen    :: UTCTime
  }

runAnchor :: Run -> UTCTime -> ([FilterName], [ChainFilter]) -> Maybe (DataDomain SlotNo) -> Maybe (DataDomain BlockNo) -> Anchor
runAnchor Run{..} = tagsAnchor [tag metadata]

tagsAnchor :: [Text] -> UTCTime -> ([FilterName], [ChainFilter]) -> Maybe (DataDomain SlotNo) -> Maybe (DataDomain BlockNo) -> Anchor
tagsAnchor aRuns aWhen aFilters aSlots aBlocks =
  Anchor { aVersion = getLocliVersion, .. }

renderAnchor :: Anchor -> Text
renderAnchor a = mconcat
  [ "runs: ", renderAnchorRuns a, ", "
  , renderAnchorNoRuns a
  ]

renderAnchorRuns :: Anchor -> Text
renderAnchorRuns Anchor{..} = mconcat
  [ T.intercalate ", " aRuns ]

renderAnchorFiltersAndDomains :: Anchor -> Text
renderAnchorFiltersAndDomains a@Anchor{..} = mconcat
  [ "filters: ", case fst aFilters of
                   [] -> "unfiltered"
                   xs -> T.intercalate ", " (unFilterName <$> xs)
  , renderAnchorDomains a]

renderAnchorDomains :: Anchor -> Text
renderAnchorDomains Anchor{..} = mconcat $
  maybe [] ((:[]) . renderDomain "slot"  (showText . unSlotNo)) aSlots
  <>
  maybe [] ((:[]) . renderDomain "block" (showText . unBlockNo)) aBlocks
 where renderDomain :: Text -> (a -> Text) -> DataDomain a -> Text
       renderDomain ty r DataDomain{..} = mconcat
         [ ", ", ty
         , " range: raw(", renderIntv r ddRaw, ", "
         ,                 showText ddRawCount, " total)"
         ,   " filtered(", maybe "none"
                           (renderIntv r) ddFiltered, ", "
         ,                 showText ddFilteredCount, " total), "
         , "filtered ",   T.take 4 . showText $ ((/) @Double `on` fromIntegral)
                                                 ddFilteredCount  ddRawCount
         ]

renderAnchorNoRuns :: Anchor -> Text
renderAnchorNoRuns a@Anchor{..} = mconcat
  [ renderAnchorFiltersAndDomains a
  , ", ", renderProgramAndVersion aVersion
  , ", analysed at ", renderAnchorDate a
  ]

-- Rounds time to seconds.
renderAnchorDate :: Anchor -> Text
renderAnchorDate = showText . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral @Int . round . utcTimeToPOSIXSeconds . aWhen

justifyHead, justifyData, justifyCentile, justifyProp :: Int -> Text -> Text
justifyHead    w = T.center        w ' '
justifyData    w = T.justifyLeft   w ' '
justifyCentile w = T.justifyLeft   w ' '
justifyProp    w = T.center        w ' '

renderCentiles :: Int -> [Centile] -> [Text]
renderCentiles wi = fmap (T.take wi . T.pack . printf "%f" . unCentile)

renderScalar  :: a -> Field ISelect I a -> Text
renderScalar v Field{..} =
  let wi = width fWidth
      packWi  = T.pack.take wi
      showDt  = packWi.dropWhileEnd (== 's').show
      showInt = T.pack.printf "%d"
      showW64 = T.pack.printf "%d"
  in case fSelect of
    IInt     (($ v)->x) ->                 showInt x
    IWord64M (($ v)->x) -> smaybe "---"    showW64 x
    IWord64  (($ v)->x) ->                 showW64 x
    IFloat   (($ v)->x) ->      packWi $ printf "%F" x
    IDeltaTM (($ v)->x) -> smaybe "---"     showDt x
    IDeltaT  (($ v)->x) ->                  showDt x
    IDate    (($ v)->x) -> packWi $ take 10 $ show x
    ITime    (($ v)->x) -> packWi $ take  8 $ drop 11 $ show x
    IText    (($ v)->x) -> T.take wi . T.dropWhileEnd (== 's') $ x

renderFieldCentiles :: a p -> (forall v. Divisible v => CDF p v -> [[v]]) -> Field DSelect p a -> [[Text]]
renderFieldCentiles x cdfProj Field{..} =
  case fSelect of
    DInt    (cdfProj . ($ x) ->ds) -> ds <&> fmap (p.printf "%d")
    DWord64 (cdfProj . ($ x) ->ds) -> ds <&> fmap (p.printf "%d")
    DFloat  (cdfProj . ($ x) ->ds) -> ds <&> fmap (p.printf "%F")
    DDeltaT (cdfProj . ($ x) ->ds) -> ds <&> fmap (justifyData (width fWidth).T.dropWhileEnd (== 's').p.show)
 where p = T.pack

renderFieldCentilesWidth :: a p -> (forall v. Divisible v => CDF p v -> [[v]]) -> Field DSelect p a -> [[Text]]
renderFieldCentilesWidth x cdfProj Field{..} =
  case fSelect of
    DInt    (cdfProj . ($ x) ->ds) -> ds <&> fmap (T.center (width fWidth) ' '.T.pack.printf "%d")
    DWord64 (cdfProj . ($ x) ->ds) -> ds <&> fmap (T.center (width fWidth) ' '.T.pack.printf "%d")
    DFloat  (cdfProj . ($ x) ->ds) -> ds <&> fmap (renderFloatStr fWidth.printf "%*F" (width fWidth + 1))
    DDeltaT (cdfProj . ($ x) ->ds) -> ds <&> fmap (renderFloatStr fWidth.dropWhileEnd (== 's').show)

renderFloatStr :: Width -> String -> Text
renderFloatStr w = justifyData w'. T.take w' . T.pack . stripLeadingZero
 where
   w' = width w
   stripLeadingZero = \case
     '0':xs@('.':_) -> xs
     xs -> xs

renderSummary :: forall f a. (a ~ Summary f, TimelineFields a, ToJSON a)
  => RenderFormat -> Anchor -> (Field ISelect I a -> Bool) -> a -> [Text]
renderSummary AsJSON    _ _ x = (:[]) . LT.toStrict $ encodeToLazyText x
renderSummary AsGnuplot _ _ _ = error "renderSummary: output not supported:  gnuplot"
renderSummary AsPretty  _ _ _ = error "renderSummary: output not supported:  pretty"
renderSummary _ a fieldSelr summ =
  render $
  Props
  { oProps = [ ("TITLE",    renderAnchorRuns a )
             , ("SUBTITLE", renderAnchorFiltersAndDomains a)
             , ("DATE",     renderAnchorDate a)
             , ("VERSION",  renderProgramAndVersion (aVersion a))
             ]
  , oConstants = []
  , oBody = (:[]) $
    Table
    { tColHeaders     = ["Value"]
    , tExtended       = True
    , tApexHeader     = Just "Parameter"
    , tColumns        = --transpose $
                        [fields' <&> renderScalar summ]
    -- , tColumns        = [kvs <&> snd]
    , tRowHeaders     = fields' <&> fShortDesc
    , tSummaryHeaders = []
    , tSummaryValues  = []
    , tFormula = []
    , tConstants = []
    }
  }
 where
   fields' :: [Field ISelect I a]
   fields' = filter fieldSelr timelineFields

renderTimeline :: forall (a :: Type). TimelineFields a => (Field ISelect I a -> Bool) -> [TimelineComments a] -> [a] -> [Text]
renderTimeline flt comments xs =
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
   entry = renderLineDist . renderScalar

   fields' :: [Field ISelect I a]
   fields' = filter flt timelineFields

   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields' then Nothing
           else Just (renderLineHead (uncurry T.take . ((+1).width.fWidth&&&fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields' then Nothing
           else Just (renderLineHead (uncurry T.take . ((+1).width.fWidth&&&fHead2)))

   -- Different strategies: fields are forcefully separated,
   --                       whereas heads can use the extra space
   renderLineHead = mconcat . renderLine' justifyHead (toEnum.(+ 1).width.fWidth)
   renderLineDist :: (Field ISelect I a -> Text) -> Text
   renderLineDist = T.intercalate " " . renderLine' justifyData fWidth

   renderLine' :: (Int -> Text -> Text) -> (Field ISelect I a -> Width) -> (Field ISelect I a -> Text) -> [Text]
   renderLine' jfn wfn rfn = fields'
                             <&> \f -> jfn (width $ wfn f) (rfn f)

mapRenderCDF :: forall p a. CDFFields a p
             => (Field DSelect p a -> Bool) -> Maybe [Centile]
             -> (forall c. Divisible c => p c -> [c])
             -> a p
             -> [[Text]]
mapRenderCDF fieldSelr centiSelr fSampleProps x =
  fields'                                   -- list of fields
  <&> renderFieldCentiles x cdfSamplesProps -- for each field, list of per-sample lists of properties
  & (transpose [renderCentiles 6 centiles] :)
  & transpose                               -- for each sample, list of per-field lists of properties
  & fmap (fmap $ T.intercalate " ")
 where
   -- Pick relevant fields:
   fields' :: [Field DSelect p a]
   fields' = filter fieldSelr cdfFields

   -- Pick relevant centiles:
   subsetCenti :: CDF p b -> CDF p b
   subsetCenti = maybe id subsetCDF centiSelr

   centiles :: [Centile]
   centiles = mapSomeFieldCDF
              (centilesCDF . subsetCenti)
              x
              (fSelect $ head cdfFields)

   -- Get relevant values: for each selected sample, a list of properties (avg, min, max, avg-/+stddev)
   cdfSamplesProps :: Divisible c => CDF p c -> [[c]]
   cdfSamplesProps = fmap (fSampleProps . snd) . cdfSamples . subsetCenti

data RenderFormat
  = AsJSON
  | AsGnuplot
  | AsOrg
  | AsReport
  | AsPretty
  deriving (Eq, Show, Bounded, Enum)

-- | When rendering a CDF-of-CDFs _and_ subsetting the data, how to subset:
data CDF2Aspect
  = OfOverallDataset   -- ^ Overall dataset statistical summary.
  | OfInterCDF         -- ^ Inter-sample (i.e. inter-CDF) stats.
  deriving (Show, Bounded, Enum)

parseCDF2Aspect :: Opt.Parser CDF2Aspect
parseCDF2Aspect =
  [ Opt.flag' OfOverallDataset  (Opt.long "overall"    <> Opt.help "Overall dataset statistical summary")
  , Opt.flag' OfInterCDF        (Opt.long "inter-cdf"  <> Opt.help "Inter-sample (i.e. inter-CDF) stats")
  ] & \case
        (x:xs) -> foldl (<|>) x xs
        [] -> error "Crazy world, begone. 1"

modeFilename :: TextOutputFile -> Text -> RenderFormat -> TextOutputFile
modeFilename orig@(TextOutputFile f) name = \case
  AsJSON    -> orig
  AsGnuplot -> printf f name & TextOutputFile
  AsOrg     -> orig
  AsReport  -> orig
  AsPretty  -> orig

renderAnalysisCDFs :: forall a p. (CDFFields a p, KnownCDF p, ToJSON (a p)) => Anchor -> (Field DSelect p a -> Bool) -> CDF2Aspect -> Maybe [Centile] -> RenderFormat -> a p -> [(Text, [Text])]

renderAnalysisCDFs _anchor _fieldSelr _c2a _centileSelr AsJSON x = (:[]) . ("",) . (:[]) . LT.toStrict $
  encodeToLazyText x

renderAnalysisCDFs anchor fieldSelr _c2a _centileSelr AsGnuplot x =
  filter fieldSelr cdfFields <&>
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
    { tColHeaders     = fields' <&> fId
    , tExtended       = True
    , tApexHeader     = Just "centile"
    , tColumns        = fields' <&> fmap (T.intercalate ":") . renderFieldCentilesWidth x cdfSamplesProps
    , tRowHeaders     = percSpecs <&> T.take 6 . T.pack . printf "%.4f" . unCentile
    , tSummaryHeaders = ["avg", "samples"]
    , tSummaryValues  = [ fields' <&>
                          \f@Field{..} -> mapField x (T.take (width fWidth + 1) . T.pack . printf "%f" . cdfAverageVal) f
                        , fields' <&>
                          \f@Field{}   -> mapField x (T.pack . printf "%d" . cdfSize) f
                        ] & transpose
    , tFormula = []
    , tConstants = []
    }
  }
 where
   cdfSamplesProps :: Divisible c => CDF p c -> [[c]]
   cdfSamplesProps = fmap (pure . unliftCDFVal cdfIx . snd) . cdfSamples . restrictCDF

   fields' :: [Field DSelect p a]
   fields' = filterFields fieldSelr

   restrictCDF :: forall c. CDF p c -> CDF p c
   restrictCDF = maybe id subsetCDF centileSelr

   percSpecs :: [Centile]
   percSpecs = maybe (mapSomeFieldCDF centilesCDF x (fSelect . head $ cdfFields @a @p))
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
                        fields' <&>
                        fmap (T.take 6 . T.pack . printf "%f")
                        . mapFieldWithKey x (snd hdrsProjs)
    , tRowHeaders     = fields' <&> fShortDesc
    , tSummaryHeaders = []
    , tSummaryValues  = []
    , tFormula = []
    , tConstants = [("nSamples",
                     fields' <&> mapField x (T.pack . show . cdfSize) & head)]
    }
  }
 where
   fields' :: [Field DSelect p a]
   fields' = filter fieldSelr cdfFields

   hdrsProjs :: forall v. (Divisible v) => ([Text], Field DSelect p a -> CDF p v -> [Double])
   hdrsProjs = aspectColHeadersAndProjections aspect

   aspectColHeadersAndProjections :: forall v. (Divisible v)
                                  => CDF2Aspect -> ([Text], Field DSelect p a -> CDF p v -> [Double])
   aspectColHeadersAndProjections = \case
     OfOverallDataset ->
       (,)
       ["average", "CoV", "min", "max", "stddev", "range", "precision", "size"]
       \Field{..} c@CDF{cdfRange=Interval cdfMin cdfMax, ..} ->
         let avg = cdfAverageVal c & toDouble in
         [ avg
         , cdfStddev / avg
         , fromRational . toRational $ cdfMin
         , fromRational . toRational $ cdfMax
         , cdfStddev
         , fromRational . toRational $ cdfMax - cdfMin
         , fromIntegral $ fromEnum fPrecision
         , fromIntegral cdfSize
         ]
     OfInterCDF ->
       (,)
       ["average", "CoV", "min", "max", "stddev", "range", "precision", "size"]
       (\Field{..} ->
        cdfArity
         (error "Cannot do inter-CDF statistics on plain CDFs")
         (\CDF{cdfAverage=cdfAvg@CDF{cdfRange=Interval minAvg maxAvg,..}} ->
             let avg = cdfAverageVal cdfAvg & toDouble in
             [ avg
             , cdfStddev / avg
             , toDouble minAvg
             , toDouble maxAvg
             , cdfStddev
             , toDouble $ maxAvg - minAvg
             , fromIntegral $ fromEnum fPrecision
             , fromIntegral cdfSize
             ]))

renderAnalysisCDFs a fieldSelr _c2a centiSelr AsPretty x =
  (:[]) . ("",) $
  renderAnchor a
  :  catMaybes [head1, head2]
  <> pLines
  <> sizeAvg
 where
   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields' then Nothing
           else Just (renderLineHead1 (uncurry T.take . ((+1) . width . fWidth &&& fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields' then Nothing
           else Just (renderLineHead2 (uncurry T.take . ((+1) . width . fWidth &&& fHead2)))
   renderLineHead1 = mconcat . ("      ":) . renderLine' justifyHead (toEnum . (+ 1) . width . fWidth)
   renderLineHead2 = mconcat . (" %tile":) . renderLine' justifyHead (toEnum . (+ 1) . width . fWidth)

   pLines :: [Text]
   pLines = fields'
            <&>
            -- fmap (T.intercalate " ") .
            fmap T.concat .
            renderFieldCentilesWidth x cdfSamplesProps
            & ((justifyCentile 6 <$> renderCentiles 6 centiles) :)
            & transpose
            & fmap (T.intercalate " ")

   fields' :: [Field DSelect p a]
   fields' = filter fieldSelr cdfFields

   centiles :: [Centile]
   centiles = mapSomeFieldCDF
              centilesCDF
              x
              (fSelect $ head cdfFields)

   cdfSamplesProps :: Divisible c => CDF p c -> [[c]]
   cdfSamplesProps = fmap (pure . unliftCDFVal cdfIx . snd)
                     . cdfSamples
                     . maybe id subsetCDF centiSelr

   sizeAvg :: [Text]
   sizeAvg = fmap (T.intercalate " ")
     [ (justifyCentile 6 "avg" :) $
       (\f -> flip (renderField justifyData fWidth) f $ const $
                mapSomeFieldCDF
                  (fit (fWidth f) .T.pack . printf "%F" . cdfAverageVal)  x (fSelect f))
       <$> fields'
     , (justifyProp 6 "size" :) $
       (\f -> flip (renderField justifyHead fWidth) f $ const $
                mapSomeFieldCDF
                  (fit (fWidth f) . T.pack . show . cdfSize)    x (fSelect f))
       <$> fields'
     ]

   fit :: Width -> Text -> Text
   fit (width -> w) t =
     if T.length t > w &&
        -- Drop all non-floats, and floats with significant digit overflowing:
        maybe True (> w) (T.findIndex (== '.') t)
     then "..."
     else T.take w t

   renderLine' :: (Int -> Text -> Text) -> (Field DSelect p a -> Width) -> (Field DSelect p a -> Text) -> [Text]
   renderLine' jfn wfn rfn  = renderField jfn wfn rfn <$> fields'

   renderField :: forall f. (Int -> Text -> Text) -> (f -> Width) -> (f -> Text) -> f -> Text
   renderField jfn wfn rend f = jfn (width $ wfn f) (rend f)

   -- populationIndices :: [Int]
   -- populationIndices = [1..maxPopulationSize]
   -- maxPopulationSize :: Int
   -- maxPopulationSize = last . sort $ mapSomeFieldCDF cdfSize x . fSelect <$> cdfFields @a @p
