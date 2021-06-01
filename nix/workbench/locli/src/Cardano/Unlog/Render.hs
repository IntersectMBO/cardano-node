{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.Unlog.Render (module Cardano.Unlog.Render) where

import           Prelude (head, show)
import           Cardano.Prelude hiding (head, show)

import           Data.List (dropWhileEnd)
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime)
import           Text.Printf (printf)

import           Data.Distribution


class Show a => RenderDistributions a where
  rdFields :: [Field a]

class Show a => RenderTimeline a where
  rtFields :: [Field a]

-- | Incapsulate all information necessary to render a column.
data Field a
  = Field
  { fWidth   :: Int
  , fLeftPad :: Int
  , fHead1   :: Text
  , fHead2   :: Text
  , fSelect  :: DSelect a
  }

data DSelect a
  = DInt    (a -> Distribution Float Int)
  | DWord64 (a -> Distribution Float Word64)
  | DFloat  (a -> Distribution Float Float)
  | DDeltaT (a -> Distribution Float NominalDiffTime)

mapSomeFieldDistribution :: (forall b. Distribution Float b -> c) -> a -> DSelect a -> c
mapSomeFieldDistribution f a = \case
  DInt    s -> f (s a)
  DWord64 s -> f (s a)
  DFloat  s -> f (s a)
  DDeltaT s -> f (s a)

renderDistributions :: forall a. RenderDistributions a => a -> [Text]
renderDistributions x = (catMaybes [head1, head2]) <> pLines
  where
    pLines :: [Text]
    pLines = fLine <$> [0..(nPercs - 1)]

    fLine :: Int -> Text
    fLine pctIx = renderLineDist $
     \Field{..} ->
       let w = show fWidth
           getCapPerc :: forall b c. Distribution b c -> c
           getCapPerc d = dPercIx d pctIx
       in T.pack $ case fSelect of
         DInt    (($x)->d) -> printf ('%':(w++"d")) (getCapPerc d)
         DWord64 (($x)->d) -> printf ('%':(w++"d")) (getCapPerc d)
         DFloat  (($x)->d) -> printf ('%':(w++"f")) (getCapPerc d)
         DDeltaT (($x)->d) -> printf ('%':(w++"s"))
                              (take fWidth . dropWhileEnd (== 's')
                               . show $ getCapPerc d)

    head1, head2 :: Maybe Text
    head1 = if all ((== 0) . T.length . fHead1) fields then Nothing
            else Just (renderLineHead1 fHead1)
    head2 = if all ((== 0) . T.length . fHead2) fields then Nothing
            else Just (renderLineHead2 fHead2)

    renderLineHead1 = mconcat . renderLine' (const 0) ((+ 1) . fWidth)
    renderLineHead2 = mconcat . renderLine' fLeftPad  ((+ 1) . fWidth)
    renderLineDist = T.intercalate " " . renderLine' fLeftPad fWidth

    renderLine' ::
      (Field a -> Int) -> (Field a -> Int) -> (Field a -> Text) -> [Text]
    renderLine' lpfn wfn rfn = flip fmap fields $
      \f ->
        (T.replicate (lpfn f) " ") <> T.center (wfn f) ' ' (rfn f)

    fields :: [Field a]
    fields = percField : rdFields
    percField :: Field a
    percField = Field 6 0 "" "%tile" (DFloat $ const percsDistrib)
    nPercs = length $ dPercentiles percsDistrib
    percsDistrib = mapSomeFieldDistribution
                     distribPercsAsDistrib x (fSelect $ head rdFields)

-- * Auxiliaries
--
distribPercsAsDistrib :: Distribution Float b -> Distribution Float Float
distribPercsAsDistrib Distribution{..} = Distribution 1 0.5 $
  (\p -> p {pctSample = psFrac (pctSpec p)}) <$> dPercentiles

nChunksEachOf :: Int -> Int -> Text -> [Text]
nChunksEachOf chunks each center =
  T.chunksOf each (T.center (each * chunks) ' ' center)
