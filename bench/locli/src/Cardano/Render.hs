{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

{- HLINT ignore "Use concatMap" -}
{- HLINT ignore "Use fromMaybe" -}

module Cardano.Render (module Cardano.Render) where

import Prelude                          (id, show)
import Cardano.Prelude                  hiding (head, show)

import Data.Aeson.Text                  (encodeToLazyText)
import Data.List                        (dropWhileEnd)
import Data.Map.Strict                  qualified as Map
import Data.Text                        qualified as T
import Data.Text.Lazy                   qualified as LT
import Options.Applicative              qualified as Opt

import Data.CDF

import Cardano.Org
import Cardano.Util
import Cardano.Analysis.API

data RenderConfig
  = RenderConfig
  { rcFormat          :: RenderFormat
  , rcDateVerMetadata :: Bool
  , rcRunMetadata     :: Bool
  }
  deriving Show

mkRenderConfigDef :: RenderFormat -> RenderConfig
mkRenderConfigDef rcFormat =
  RenderConfig
  { rcDateVerMetadata = True
  , rcRunMetadata     = True
  , ..
  }

data RenderFormat
  = AsJSON
  | AsGnuplot
  | AsOrg
  | AsReport
  | AsPretty
  deriving (Eq, Show, Bounded, Enum)

-- | Explain the poor human a little bit of what was going on:
data Anchor
  = Anchor
  { aRuns    :: [Text]
  , aFilters :: ([FilterName], [ChainFilter])
  , aSlots   :: Maybe (DataDomain I SlotNo)
  , aBlocks  :: Maybe (DataDomain I BlockNo)
  , aVersion :: LocliVersion
  , aWhen    :: UTCTime
  }

tagsAnchor :: [Text] -> UTCTime -> ([FilterName], [ChainFilter]) -> Maybe (DataDomain I SlotNo) -> Maybe (DataDomain I BlockNo) -> Anchor
tagsAnchor aRuns aWhen aFilters aSlots aBlocks =
  Anchor { aVersion = getLocliVersion, .. }

renderAnchor :: RenderConfig -> Anchor -> Text
renderAnchor RenderConfig{..} a = mconcat $
  (if rcRunMetadata  then ["runs: ", renderAnchorRuns a, ", "] else [])
  <>
  [renderAnchorFiltersAndDomains a, ", "]
  <>
  [ renderAnchorDateVer a | rcDateVerMetadata ]

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
 where renderDomain :: Text -> (a -> Text) -> DataDomain I a -> Text
       renderDomain ty r DataDomain{..} = mconcat
         [ ", ", ty
         , " range: raw(", renderIntv r (fmap unI ddRaw), ", "
         ,                 showText ddRawCount, " total)"
         ,   " filtered(", maybe "none"
                           (renderIntv r . fmap unI) ddFiltered, ", "
         ,                 showText ddFilteredCount, " total), "
         , "filtered ",   T.take 4 . showText $ ((/) @Double `on` (fromIntegral.unI))
                                                 ddFilteredCount  ddRawCount
         ]

renderAnchorDateVer :: Anchor -> Text
renderAnchorDateVer a@Anchor{..} = mconcat
  [ renderProgramAndVersion aVersion
  , ", analysed at ", renderAnchorDate a
  ]

-- Rounds time to seconds.
renderAnchorDate :: Anchor -> Text
renderAnchorDate = showText . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral @Int . round . utcTimeToPOSIXSeconds . aWhen

renderAnchorOrgProperties :: RenderConfig -> Anchor -> [(Text, Text)]
renderAnchorOrgProperties RenderConfig{rcDateVerMetadata, rcRunMetadata} a =
  [ ("TITLE",    renderAnchorRuns a )                  | rcRunMetadata ]
  <>
  [ ("SUBTITLE", renderAnchorFiltersAndDomains a) ]
  <>
  [ ("DATE",     renderAnchorDate a)                   | rcDateVerMetadata ]
  <>
  [ ("VERSION",  renderProgramAndVersion (aVersion a)) | rcDateVerMetadata ]

--
--
justifyHead, justifyData, justifyCentile, justifyProp :: Int -> Text -> Text
justifyHead    w = T.justifyLeft   w ' '
justifyData    w = T.justifyLeft   w ' '
justifyCentile w = T.justifyLeft   w ' '
justifyProp    w = T.center        w ' '

renderCentiles :: Int -> [Centile] -> [Text]
renderCentiles wi = fmap (T.take wi . T.pack . printf "%f" . unCentile)

renderScalarLim  :: Maybe Int -> a -> Field ISelect I a -> Text
renderScalarLim wLim v Field{..} =
  let wi = unWidth fWidth <|> wLim
           & fromMaybe (error "renderScalar:  request to render a width-free-field, without a supplied width limit.")
      packWi  = T.pack.take wi
      showDt  = handleStrOverflowFloat wi.renderDiffTime
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
    DInt    (cdfProj . ($ x) ->ds) -> ds <&> fmap (formatInt fWidth)
    DWord64 (cdfProj . ($ x) ->ds) -> ds <&> fmap (formatInt fWidth)
    DFloat  (cdfProj . ($ x) ->ds) -> ds <&> fmap (formatDouble fWidth)
    DDeltaT (cdfProj . ($ x) ->ds) -> ds <&> fmap (formatDiffTime fWidth)

renderSummary :: forall f a. (a ~ Summary f, TimelineFields a, ToJSON a)
  => RenderConfig -> Anchor -> (Field ISelect I a -> Bool) -> a -> [Text]
renderSummary RenderConfig{rcFormat=AsJSON} _ _ x = (:[]) . LT.toStrict $ encodeToLazyText x
renderSummary rc@RenderConfig{rcFormat=AsReport} a fieldSelr summ =
  render $
  Props
  { oProps = renderAnchorOrgProperties rc a
  , oConstants = []
  , oBody = (:[]) $
    Table
    { tColHeaders     = ["Value"]
    , tExtended       = True
    , tApexHeader     = Just "Parameter"
    , tColumns        = --transpose $
                        [fields' <&> renderScalarLim (Just 32) summ]
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
renderSummary rc  _ _ _ =
  error $ "renderSummary: RenderConfig not supported:  " <> show rc

renderProfilingData ::
  RenderConfig -> Anchor -> (ProfileEntry (CDF I) -> Bool) -> ProfilingData (CDF I) -> [Text]
renderProfilingData rc a flt pd =
  render $
  Props
  { oProps = renderAnchorOrgProperties rc a
  , oConstants = []
  , oBody = (:[]) $
    Table
    { tColHeaders     = ["time, %", "range", "alloc, %", "source location"]
    , tExtended       = True
    , tApexHeader     = Just "Parameter"
    , tColumns        = [ fieldsTime   <&> renderScalarLim (Just 6) pd
                        , fieldsRange  <&> renderScalarLim (Just 6) pd
                        , fieldsAlloc  <&> renderScalarLim (Just 6) pd
                        , fieldsSrcLoc <&> renderScalarLim (Just 60) pd
                        ]
    , tRowHeaders     = pes <&> peFunc
    , tSummaryHeaders = []
    , tSummaryValues  = []
    , tFormula        = []
    , tConstants      = []
    }
  }
 where
   pes :: [ProfileEntry (CDF I)]
   pes = filter flt (pdMap pd & Map.elems)
         & sortBy (compare `on` Down . unI . cdfAverage . peTime)
   fieldsTime   = pes <&> mkFi W3  (IFloat . ((unI . cdfAverage . peTime) .))
   fieldsRange  = pes <&> mkFi W3  (IFloat . ((uncurry (flip (-)) . (low &&& high) .  cdfRange . peTime) .))
   fieldsAlloc  = pes <&> mkFi W3  (IFloat . ((unI . cdfAverage . peAlloc) .))
   fieldsSrcLoc = pes <&> mkFi Wno (IText  . (peSrcLoc .))

   mkFi :: f ~ CDF I
        => Width
        -> ((ProfilingData f -> ProfileEntry f) -> ISelect I (ProfilingData f))
        -> ProfileEntry f
        -> Field ISelect I (ProfilingData f)
   mkFi w f pe = Field
     { fId    = peFunc pe
     , fHead1 = ""
     , fHead2 = ""
     , fWidth = w
     , fUnit  = Pct
     , fPrecision = P3
     , fScale = Lin
     , fRange = Free
     , fSelect =
       f
       $ fromMaybe (error $ "Function '" <> T.unpack (peFunc pe) <> "' missing from ProfileData.")
       . Map.lookup (peFunc pe)
       . pdMap
     , fShortDesc = peFunc pe
     , fDescription = peSrcLoc pe
     }

renderTimelineWithClass :: forall (a :: Type). TimelineFields a => (Field ISelect I a -> Bool) -> RenderConfig -> Anchor -> [TimelineComments a] -> [a] -> [Text]
renderTimelineWithClass flt = renderTimeline (filter flt timelineFields) rtCommentary

renderTimeline :: forall a b. [Field ISelect I a] -> (a -> b -> [Text]) -> RenderConfig -> Anchor -> [b] -> [a] -> [Text]
renderTimeline fields' commentFn rc a comments xs =
  ("# " <> renderAnchor rc a)
  : concatMap (uncurry fLine) (zip xs [(0 :: Int)..])
 where
   fLine :: a -> Int -> [Text]
   fLine l i =
     -- 0. a periodic header
     (if i `mod` 33 == 0 then catMaybes [head1, head2] else [])
     -- 1. actual timeline entry
     <> (entry l
     -- 2. per-entry commentary, if any
         : concat (fmap (commentFn l) comments))

   entry :: a -> Text
   entry = renderLineDist . renderScalarLim Nothing

   head1, head2 :: Maybe Text
   head1 =
     if all ((== 0) . T.length . fHead1) fields'
     then Nothing
     else Just $ renderLineHead
          (uncurry (takeAlign T.justifyLeft) . ((+1).unsafeUnWidth "renderTimeline head1".fWidth&&&fHead1))
   head2 =
     if all ((== 0) . T.length . fHead2) fields'
     then Nothing
     else Just $ renderLineHead
          (uncurry (takeAlign T.justifyLeft) . ((+1).unsafeUnWidth "renderTimeline head2".fWidth&&&fHead2))
   takeAlign f n = f n ' ' . T.take n

   -- Different strategies: fields are forcefully separated,
   --                       whereas heads can use the extra space
   renderLineHead = mconcat . renderLine' justifyHead fWidth
   -- renderLineHead = mconcat . renderLine' justifyHead (toEnum.(+ 1).unsafeUnWidth "renderTimeline".fWidth)
   renderLineDist :: (Field ISelect I a -> Text) -> Text
   renderLineDist = T.intercalate " " . renderLine' justifyData fWidth

   renderLine' :: (Int -> Text -> Text) -> (Field ISelect I a -> Width) -> (Field ISelect I a -> Text) -> [Text]
   renderLine' jfn wfn rfn = fields'
                             <&> \f -> jfn (unsafeUnWidth "renderTimeline" $ wfn f) (rfn f)

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

renderAnalysisCDFs :: forall a p. (CDFFields a p, KnownCDF p, ToJSON (a p)) => Anchor -> (Field DSelect p a -> Bool) -> CDF2Aspect -> Maybe [Centile] -> RenderConfig -> a p -> [(Text, [Text])]

renderAnalysisCDFs _anchor _fieldSelr _c2a _centileSelr RenderConfig{rcFormat=AsJSON} x = (:[]) . ("",) . (:[]) . LT.toStrict $
  encodeToLazyText x

renderAnalysisCDFs anchor fieldSelr _c2a _centileSelr rc@RenderConfig{rcFormat=AsGnuplot} x =
  filter fieldSelr cdfFields <&>
  \Field{fId=cdfField} ->
    (,) cdfField $
    "# " <> renderAnchor rc anchor :
    (mapRenderCDF ((== cdfField) . fId) Nothing (unliftCDFValExtra cdfIx) x
     & fmap (T.intercalate " "))

renderAnalysisCDFs a fieldSelr _c2a centileSelr rc@RenderConfig{rcFormat=AsOrg} x =
  (:[]) . ("",) . render $
  Props
  { oProps = renderAnchorOrgProperties rc a
  , oConstants = []
  , oBody = (:[]) $
    Table
    { tColHeaders     = fields' <&> fId
    , tExtended       = True
    , tApexHeader     = Just "centile"
    , tColumns        = fields' <&> fmap (T.intercalate ":") . renderFieldCentiles x cdfSamplesProps
    , tRowHeaders     = percSpecs <&> (T.take 6 . T.pack . printf "%.4f") . unCentile
    , tSummaryHeaders = ["avg", "samples"]
    , tSummaryValues  = [ fields' <&>
                          \f@Field{..} ->
                            mapField x (formatDouble fWidth . cdfAverageVal) f
                        , fields' <&>
                          \f@Field{..} ->
                            mapField x (formatInt    fWidth . cdfSize) f
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

renderAnalysisCDFs a fieldSelr aspect _centileSelr rc@RenderConfig{rcFormat=AsReport} x =
  (:[]) . ("",) . render $
  Props
  { oProps = renderAnchorOrgProperties rc a
  , oConstants = []
  , oBody = (:[]) $
    Table
    { tColHeaders     = fst (hdrsProjs @Integer) -- This is crazy.
    , tExtended       = True
    , tApexHeader     = Just "metric"
    , tColumns        = transpose $
                        fields' <&>
                        fmap (formatDouble W6)
                        . mapFieldWithKey x (snd hdrsProjs)
    , tRowHeaders     = fields' <&> (\Field{..} ->
                                       fShortDesc <> ", " <> renderUnit fUnit)
    , tSummaryHeaders = []
    , tSummaryValues  = []
    , tFormula = []
    , tConstants = [("nSamples",
                     fields' <&> mapField x (formatInt W4 . cdfSize) & head)]
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

renderAnalysisCDFs a fieldSelr _c2a centiSelr rc@RenderConfig{rcFormat=AsPretty} x =
  (:[]) . ("",) $
  renderAnchor rc a
  :  catMaybes [head1, head2]
  <> pLines
  <> sizeAvg
 where
   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields' then Nothing
           else Just (renderLineHead1 (uncurry T.take . ((+1) . unsafeUnWidth "renderAnalysisCDFs/AsPretty" . fWidth &&& fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields' then Nothing
           else Just (renderLineHead2 (uncurry T.take . ((+1) . unsafeUnWidth "renderAnalysisCDFs/AsPretty" . fWidth &&& fHead2)))
   renderLineHead1 = mconcat . ("      ":) . renderLine' justifyHead (toEnum . (+ 1) . unsafeUnWidth "renderAnalysisCDFs/AsPretty" . fWidth)
   renderLineHead2 = mconcat . (" %tile":) . renderLine' justifyHead (toEnum . (+ 1) . unsafeUnWidth "renderAnalysisCDFs/AsPretty" . fWidth)

   pLines :: [Text]
   pLines = fields'
            <&>
            -- fmap (T.intercalate " ") .
            fmap T.concat .
            renderFieldCentiles x cdfSamplesProps
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
       (\f -> mapSomeFieldCDF (formatDouble (fWidth f) . cdfAverageVal) x (fSelect f))
       -- (\f -> flip (renderField justifyData fWidth) f $ const $
       --          mapSomeFieldCDF
       --            (fit f .T.pack . printf "%F" . cdfAverageVal)  x (fSelect f))
       <$> fields'
     , (justifyProp 6 "size" :) $
       (\f -> mapSomeFieldCDF (formatInt (fWidth f) . cdfSize) x (fSelect f))
       <$> fields'
     ]

   renderLine' :: (Int -> Text -> Text) -> (Field DSelect p a -> Width) -> (Field DSelect p a -> Text) -> [Text]
   renderLine' jfn wfn rfn  = renderField jfn wfn rfn <$> fields'

   renderField :: forall f. (Int -> Text -> Text) -> (f -> Width) -> (f -> Text) -> f -> Text
   renderField jfn wfn rend f = jfn (unsafeUnWidth "renderAnalysisCDFs/AsPretty" $ wfn f) (rend f)

   -- populationIndices :: [Int]
   -- populationIndices = [1..maxPopulationSize]
   -- maxPopulationSize :: Int
   -- maxPopulationSize = last . sort $ mapSomeFieldCDF cdfSize x . fSelect <$> cdfFields @a @p

--
-- Rendering mini-lib
--
-- Terms & conditions:
--
-- - "format" means "fit width"
--
formatInt :: Integral a => Width -> a -> Text
formatInt = centerLim renderInt (handleStrOverflowTrimMark $ const True)
 where
   renderInt :: Integral a => a -> Text
   renderInt x = T.pack $ printf "%d" (fromIntegral x :: Integer)

formatDouble :: Width -> Double -> Text
formatDouble = centerLim renderDouble handleStrOverflowFloat
 where
   renderDouble :: Double -> Text
   renderDouble x = T.pack $ printf "%F" x

formatDiffTime :: Width -> NominalDiffTime -> Text
formatDiffTime = centerLim renderDiffTime handleStrOverflowFloat

renderDiffTime :: NominalDiffTime -> Text
renderDiffTime = T.pack . dropWhileEnd (== 's').show

handleStrOverflowFloat :: Int -> Text -> Text
handleStrOverflowFloat w =
  handleStrOverflowTrimMark decideCutMark w . stripTrailingDot . stripLeadingZero
 where
   decideCutMark :: Text -> Bool
   decideCutMark = isNothing . T.findIndex (== '.')
   stripLeadingZero x@(T.isPrefixOf "0." -> True) = T.drop 1 x
   stripLeadingZero x = x
   stripTrailingDot x@(T.isSuffixOf "."  -> True) = T.dropEnd 1 x
   stripTrailingDot x = x

handleStrOverflowTrim :: Int -> Text -> Text
handleStrOverflowTrim = T.take

handleStrOverflowTrimMark :: (Text -> Bool) -> Int -> Text -> Text
handleStrOverflowTrimMark f w s =
  let len = length s
  in if len <= w then s
     else if f s
          then T.take (w - 1) s <> ">"
          else T.take  w      s

centerLim :: (a -> Text) -> (Int -> Text -> Text) -> Width -> a -> Text
centerLim rend handleOverflow wLim x =
  mapWidth
    (error "Wno is not a valid width limit: centerLim")
    (\w -> T.center w ' ' $ handleOverflow w (rend x))
    wLim
