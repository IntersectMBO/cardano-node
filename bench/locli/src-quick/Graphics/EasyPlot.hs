{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | A simple wrapper to the gnuplot command line utility.
--
-- Typically you will invoke a plot like so:
--
-- > plot X11 $ Data2D [Title "Sample Data"] [] [(1, 2), (2, 4), ...]
--
-- To plot a function, use the following:
--
-- > plot X11 $ Function2D [Title "Sine and Cosine"] [] (\x -> sin x * cos x)
--
-- There is also a shortcut available - the following plots the sine function:
--
-- > plot X11 sin
--
-- Output can go into a file, too (See 'TerminalType'):
--
-- > plot (PNG "plot.png") (sin . cos)
--
-- Haskell functions are plotted via a set of tuples obtained form the function.
-- If you want to make use of gnuplots mighty function plotting functions you can
-- pass a 'Gnuplot2D' or 'Gnuplot3D' object to plot.
--
-- > plot X11 $ Gnuplot2D [Color Blue] [] "2**cos(x)"
--
-- For 3D-Plots there is a shortcut available by directly passing a String:
--
-- > plot X11 "x*y"
--
-- Multiple graphs can be shown simply by passing a list of these:
--
-- > plot X11 [ Data2D [Title "Graph 1", Color Red] [] [(x, x ** 3) | x <- [-4,-3.9..4]]
-- >          , Function2D [Title "Function 2", Color Blue] [] (\x -> negate $ x ** 2) ]
--
-- For 3D Graphs it is useful to be able to interact with the graph (See 'plot'' and 'GnuplotOption'):
--
-- > plot' [Interactive] (mkTerminal X11) $ Gnuplot3D [Color Magenta] [] "x ** 2 + y ** 3"
--
-- If you want to inspect the command and datafiles that are used to plot your graph,
-- turn on debugging - intermediate files will be kept on disk:
--
-- > plot' [Debug] (mkTerminal X11) $ Gnuplot3D [Color Magenta] [] "x ** 4 + y ** 3"
-- > > set term x11 persist; splot x ** 4 + y ** 3 lc rgb "magenta"
module Graphics.EasyPlot (

    -- * Plotting
    Plot (plot, plot'),

    -- * Graphs for 2D and 3D plots
    Graph2D (..), Graph3D (..),

    -- * Configuration and other options
    Terminal(..), TerminalType (..),
    defaultOsTerminal, mkTerminal,
    Color (..), Style (..), Style2D (..),
    Option (..), Option2D (..), Option3D (..),
    GnuplotOption (..)

    ) where

import           Control.Monad
import           Data.Bool (bool)
import           Data.List (intercalate, nubBy, sortBy)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word8)
import           System.Directory (removeFile)
import           System.Exit (ExitCode (ExitSuccess))
import           System.Process (rawSystem)
import           Text.Printf

-- | TerminalType determines where the output of gnuplot should go.
data TerminalType =
    Aqua        -- ^ Output on Mac OS X (Aqua Terminal).
  | Windows     -- ^ Output for MS Windows.
  | X11         -- ^ Output to the X Window System.
  | Dumb        -- ^ Output to text-based user interface.
  | Qt          -- ^ Output to a Qt window (should be cross-platform)

  | PS FilePath -- ^ Output into a Postscript file.
  | EPS FilePath -- ^ Output into an EPS file.
  | PNG FilePath -- ^ Output as Portable Network Graphic into file.
  | PDF FilePath -- ^ Output as Portable Document Format into a file.
  | SVG FilePath -- ^ Output as Scalable Vector Graphic into a file.
  | GIF FilePath -- ^ Output as Graphics Interchange Format into a file.
  | JPEG FilePath -- ^ Output into a JPEG file.
  | Latex FilePath -- ^ Output as LaTeX.
  | PNGCairo FilePath -- ^ Output as Portable Network Graphic into file, using Cairo as rendering backend.
    deriving (Eq, Show)

data Terminal = Terminal
  { tType :: TerminalType      -- ^ the terminal type.
  , tName :: String            -- ^ the terminal name; represents caption for window-based terminal types, output filename for file based ones.
  , tSize :: Maybe (Int, Int)  -- ^ the terminal size, if other than the default is desired: pixels for pixel-based terminals and formats, inches for PDF, PS, EPS
  }
  deriving (Eq, Show)
  -- TODO: Use fractional size type, for non-pixel based resolution specs?

defaultOsTerminal :: TerminalType
#if defined(linux_HOST_OS)
defaultOsTerminal = X11
#elif defined(mingw32_HOST_OS)
defaultOsTerminal = Windows
#elif defined(darwin_HOST_OS)
defaultOsTerminal = Aqua
#else
defaultOsTerminal = Dumb
#endif

-- this is a little redundant, but keeps backwards compatibility
mkTerminal :: TerminalType -> Terminal
mkTerminal tt = Terminal tt defaultName Nothing
 where
  defaultName = case tt of
    PS f          -> f
    EPS f         -> f
    PNG f         -> f
    PDF f         -> f
    SVG f         -> f
    GIF f         -> f
    JPEG f        -> f
    Latex f       -> f
    PNGCairo f    -> f
    _             -> ""

-- | The Style of a graph.
data Style = Lines  -- ^ points in the plot are interconnected by lines.
           | Points -- ^ data points are little cross symbols.
           | Dots   -- ^ data points are real dots (approx the size of a pixel).
           | Impulses
           | Linespoints
           | Steps

-- | The Color of a graph.
data Color = Red | Blue | Green | Yellow | Orange | Magenta | Cyan
           | DarkRed | DarkBlue | DarkGreen | DarkYellow | DarkOrange | DarkMagenta | DarkCyan
           | LightRed | LightBlue | LightGreen | LightMagenta
           | Violet | White | Brown | Grey | DarkGrey | Black
           | RGB Word8 Word8 Word8 -- ^ a custom color

data Style2D = Boxerrorbars | Boxes | Boxyerrorbars
             | Filledcurves | Financebars | Fsteps | Histeps | Histograms
             | Steps2D | Xerrorbars | Xyerrorbars | Yerrorbars | Xerrorlines
             | Xyerrorlines | Yerrorlines

-- | Options on how to render a graph.
data Option = Style Style   -- ^ The style for a graph.
            | Title String  -- ^ The title for a graph in a plot (or a filename like @plot1.dat@).
            | Color Color   -- ^ The line-color for the graph (or if it consist of 'Dots' or 'Points' the color of these)
            | Width Int     -- ^ The line width for a graph.

-- | Options which are exclusively available for 2D plots.
data Option2D x y = Range x x -- ^ Plots the function for the specified x range
                  | For [x]   -- ^ Plots the function only for the given x values
                  | Step x    -- ^ Uses the given step-size for plotting along the x-axis

-- | Options which are exclusively available for 3D plots.
data Option3D x y z = RangeX x x -- ^ Plots the function for the specified x range
                    | RangeY y y -- ^ Plots the function for the specified y range
                    | ForX [x]   -- ^ Plots the function only for the given x values
                    | ForY [y]   -- ^ Plots the function only for the given y values
                    | StepX x    -- ^ Uses the given step-size for plotting along the x-axis
                    | StepY y    -- ^ Uses the given step-size for plotting along the y-axis

-- | A two dimensional set of data to plot.
data Graph2D x y =
      Function2D   [Option] [Option2D x y] (x -> y)
      -- ^ plots a Haskell function @x -> y@

    | Data2D       [Option] [Option2D x y] [(x, y)]
      -- ^ plots a set of tuples.

    | Gnuplot2D    [Option] [Option2D x y] String
      -- ^ plots a custom function passed to Gnuplot (like @x**2 + 10@)

    | File2D       [Option] [Option2D x y] FilePath (Maybe (Int, Int))
      -- ^ plots data read from a file, optionally giving indices of which columns to plot as x and y

-- | A three dimensional set of data to plot.
data Graph3D x y z =
      Function3D   [Option] [Option3D x y z] (x -> y -> z)
      -- ^ plots a Haskell function @x -> y -> z@

    | Data3D       [Option] [Option3D x y z] [(x, y, z)]
      -- ^ plots a set of triples.

    | Gnuplot3D    [Option] [Option3D x y z] String
      -- ^ plots a custom function passed to Gnuplot (like @x*y@)

    | File3D       [Option] [Option3D x y z] FilePath (Maybe (Int, Int, Int))
      -- ^ plots data read from a file, optionally giving indices of which columns to plot as x, y and z

-- | Options which can be used with 'plot''
data GnuplotOption = Interactive -- ^ keeps gnuplot open, so that you can interact with the plot (only usefull with 'X11')
                   | Debug       -- ^ keeps intermediate files used to invoke gnuplot, such as the script '_plot.p' and the datafiles '_plot*.dat'.
    deriving Eq

-- | Provides the plot function for different kinds of graphs (2D and 3D)
class Plot a where

    -- | Do a plot to the terminal (i.e. a window will open and your plot can be seen)
    plot :: TerminalType -- ^ The terminal to be used for output.
            -> a         -- ^ The graph to plot. A 'Graph2D' or 'Graph3D' or a list of these.
            -> IO Bool   -- ^ Whether the plot was successful or not.

    plot = plot' [] . mkTerminal

    plot' :: [GnuplotOption]
            -> Terminal
            -> a
            -> IO Bool

-- | 'plot' can be used to plot a single 'Graph2D'.
instance (Fractional x, Enum x, Show x, Num y, Show y) => Plot (Graph2D x y) where
    plot' options term graph = plot' options term [graph]

-- | 'plot' can be used to plot a list of 'Graph2D'.
instance (Fractional x, Enum x, Show x, Num y, Show y) => Plot [Graph2D x y] where
    plot' options term graphs = exec options term "plot" options' datasources
        where   (options', datasources) = unzip $ map prepare graphs
                prepare (Gnuplot2D  opt _opt2d g) = (opts $ sanitize opt, DataExpression g)
                prepare (Data2D     opt _opt2d d) = (opts $ sanitize opt, DataRendered $ toString d)
                prepare (Function2D opt opt2d f) = (opt', DataRendered plotData)
                    where   (opt', plotData) = render2D opt opt2d f
                prepare (File2D     opt _opt2d f ixs) = (opts $ sanitize opt, DataFile f ixs')
                    where   ixs' = maybe [] (\(ix1, ix2) -> [ix1, ix2]) ixs

-- | 'plot' can be used to plot a single 'Graph3D'.
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot (Graph3D x y z) where
    plot' options term graph = plot' options term [graph]

-- | 'plot' can be used to plot a list of 'Graph3D'
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot [Graph3D x y z] where
    plot' options term graphs = exec options term "splot" options' datasources
        where   (options', datasources) = unzip $ map prepare graphs
                prepare (Gnuplot3D  opt _opt3d g) = (opts $ sanitize opt, DataExpression g)
                prepare (Data3D     opt _opt3d d) = (opts $ sanitize opt, DataRendered $ toString d)
                prepare (Function3D opt opt3d f) = (opt', DataRendered plotData)
                    where   (opt', plotData) = render3D opt opt3d f
                prepare (File3D     opt _opt3d f ixs) = (opts $ sanitize opt, DataFile f ixs')
                    where   ixs' = maybe [] (\(ix1, ix2, ix3) -> [ix1, ix2, ix3]) ixs

-- | A 2D function can be plotted directly using 'plot'
instance {-# OVERLAPPABLE #-} (Fractional x, Enum x, Show x, Num y, Show y) => Plot (x -> y) where
    plot' options term f = plot' options term $ Function2D [] [] f

-- | A list of 2D functions can be plotted directly using 'plot'
instance {-# OVERLAPPABLE #-} (Fractional x, Enum x, Show x, Num y, Show y) => Plot [x -> y] where
    plot' options term fs = plot' options term $ map (Function2D [] []) fs

-- | A 3D function can be plotted directly using 'plot'
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot (x -> y -> z) where
    plot' options term f = plot' options term $ Function3D [] [] f

-- | A list of 3D functions can be plotted directly using 'plot'
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot [x -> y -> z] where
    plot' options term fs = plot' options term $ map (Function3D [] []) fs

-- | A list of tuples can be plotted directly using 'plot'
instance (Fractional x, Enum x, Num x, Show x, Num y, Show y) => Plot [(x, y)] where
    plot' options term d = plot' options term $ Data2D [] [] d

-- | A list of triples can be plotted directly using 'plot'
instance (Fractional x, Enum x, Show x, Fractional y, Enum y, Show y, Num z, Show z) => Plot [(x, y, z)] where
    plot' options term d = plot' options term $ Data3D [] [] d

-- | plot accepts a custom string which is then to be interpreted by gnu plot.
--   The function will be interpreted as 'Gnuplot3D'.
instance Plot String where
    plot' options term g = plot' @(Graph3D Double Double Int) options term $ Gnuplot3D [] [] g

-- | plots mutliple 3D functions using gnuplots native function parser
--   and renderer. The String will be interpreted as 'Gnuplot3D'.
instance Plot [String] where
    plot' options term g = plot' @[Graph3D Double Double Int] options term $ map (Gnuplot3D [] []) g

-- | INTERNAL: Prepares 2D plots of haskell functions.
render2D :: (Num a, Show a, Fractional x, Show x, Enum x) =>
                [Option] -> [Option2D x y] -> (x -> a) -> ([Char], String)
render2D opt opt2d f = (opts $ sanitize (opt ++ [Style Lines]), plot2D f)
    where   plot2D g = toString [(x, g x) | x <- fromMaybe [x1,sx..x2] $ for opt2d]

            (x1, x2) = range opt2d
            sx       = x1 + step opt2d

-- | INTERNAL: Prepares 3D plots of haskell functions.
render3D :: (Num a, Show a, Fractional x, Fractional y, Enum y,
              Enum x, Show x, Show y) =>
                [Option] -> [Option3D x y z] -> (x -> y -> a) -> ([Char], String)
render3D opt opt3d f = (opts $ sanitize opt, plot3D f)
    where   plot3D g = toString [(x, y, g x y) | x <- xs, y <- ys]

            xs = fromMaybe [x1,sx..x2] $ forX opt3d
            ys = fromMaybe [y1,sy..y2] $ forY opt3d

            ((x1, x2), (y1, y2)) = (rangeX opt3d, rangeY opt3d)
            (sx, sy) = (x1 + stepX opt3d, y1 + stepY opt3d)

for :: [Option2D x y] -> Maybe [x]
for []             = Nothing
for ((For xs) : _) = Just xs
for (_ : xs)       = for xs

range :: Num x => [Option2D x y] -> (x, x)
range []                  = (-5, 5)
range ((Range x1 x2) : _) = (x1, x2)
range (_ : xs)            = range xs

step :: Fractional x => [Option2D x y] -> x
step []             = 0.05
step ((Step x) : _) = x
step (_ : xs)       = step xs

forX :: [Option3D x y z] -> Maybe [x]
forX []              = Nothing
forX ((ForX xs) : _) = Just xs
forX (_ : xs)        = forX xs

forY :: [Option3D x y z] -> Maybe [y]
forY []              = Nothing
forY ((ForY ys) : _) = Just ys
forY (_ : ys)        = forY ys

rangeX :: Num x => [Option3D x y z] -> (x, x)
rangeX []                   = (-5, 5)
rangeX ((RangeX x1 x2) : _) = (x1, x2)
rangeX (_ : xs)             = rangeX xs

rangeY :: Num y => [Option3D x y z] -> (y, y)
rangeY []                   = (-5, 5)
rangeY ((RangeY y1 y2) : _) = (y1, y2)
rangeY (_ : ys)             = rangeY ys

stepX :: Fractional x => [Option3D x y z] -> x
stepX []              = 0.1
stepX ((StepX x) : _) = x
stepX (_ : xs)        = stepX xs

stepY :: Fractional y => [Option3D x y z] -> y
stepY []              = 0.1
stepY ((StepY y) : _) = y
stepY (_ : ys)        = stepY ys

-- | INTERNAL: Sanitizes options given via Graph-Objects
sanitize :: [Option] -> [Option]
sanitize = sortBy ord . nubBy dup
    where   ord a b
                | dup a b   = EQ
                | otherwise = ord' a b
            ord' (Style _) (Title _) = LT
            ord' (Style _) (Color _) = LT
            ord' (Color _) (Title _) = GT
            ord' a b
                | ord' b a == LT = GT
                | otherwise      = LT
            dup (Title _) (Title _) = True
            dup (Style _) (Style _) = True
            dup (Color _) (Color _) = True
            dup _ _                 = False

-- | INTERNAL: Translates options into gnuplot commands
opts :: GnuplotIdiom a => [a] -> [Char]
opts []     = ""
opts [x]    = toString x
opts (x:xs) = toString x ++ " " ++ opts xs

-- | INTERNAL: Guarded values
infix 7 |?
(|?) :: Monoid m => m -> Bool -> m
(|?) = bool mempty
{-# INLINE (|?) #-}

data DataSource =
    DataRendered String        -- ^ data rendered by a GnuplotIdiom instance, e.g. "2 0 3\n1 1 2"
  | DataExpression String      -- ^ expression passed verbatim to gnuplot, such as "sin(x) + cos(y)"
  | DataFile FilePath [Int]    -- ^ datafile and a (possibly empty) list of indices for a 'using x:y' modifier

writeDataSource :: (DataSource, FilePath) -> IO DataSource
writeDataSource = \case
  (DataRendered s, f) -> writeFile f s >> pure (DataRendered f)
  (s, _)              -> pure s

asPlotCmd :: DataSource -> String
asPlotCmd = \case
  DataRendered f   -> quoted f
  DataExpression e -> e
  DataFile f ixs   -> quoted f ++ (" using " ++ intercalate ":" (map show ixs)) |? (not . null) ixs

-- | INTERNAL: Invokes gnuplot.
exec :: [GnuplotOption] -> Terminal -> String -> [String] -> [DataSource] -> IO Bool
exec options term@Terminal{..} plotfunc plotops datasets = do
  -- in case of DataRendered, this replaces the data with the name of its intermediate file, for later removal
  datasources <- mapM writeDataSource $ zip datasets fileNames

  let
    plotcmds = [ asPlotCmd source ++ " " ++ popts | (source, popts) <- zip datasources plotops ]
    plotstmt = foldl1  (\x y -> x ++ ", " ++ y) plotcmds
    plotcmd  = foldl1  (\x y -> x ++ "; " ++ y)
                    (preamble ++ [plotfunc ++ " " ++ plotstmt])

    args = cliArgs ++ ["-e", plotcmd] ++ ["-" | Interactive `elem` options]

  when (Debug `elem` options) $
    writeFile "_plot.p" plotcmd

  exitCode <- rawSystem "gnuplot" args

  unless (Debug `elem` options) $
    mapM_ removeFile [ f | DataRendered f <- datasources ]

  return $ exitCode == ExitSuccess
  where
    preamble  = [ toString term ]
    fileNames = [ "_plot" ++ show ix ++ ".dat" | ix <- [1 :: Int ..] ]
    -- X11 terminal only requires size as a CLI parameter
    cliArgs
      | tType == X11, Just (x, y) <- tSize  = [ "-geometry", show x ++ "x" ++ show y ]
      | tType == Windows                    = [ "-persist" ]
      | otherwise                           = []

-- | INTERNAL: Provides 'toString' for translating Haskell values into gnuplot commands
--   (ordinary strings)
class GnuplotIdiom a where
    toString :: a -> String

instance (Num x, Show x, Num y, Show y) => GnuplotIdiom (x, y) where
    toString (x, y) = space $ shows x $ space $ show y

instance (Num x, Show x, Num y, Show y, Num z, Show z) => GnuplotIdiom (x, y, z) where
    toString (x, y, z) = space $ shows x $ space $ shows y $ space $ show z

quoted, space :: String -> String
quoted s = "\"" ++ s ++ "\""
space x  = ' ' : x

instance GnuplotIdiom Style where
    toString x = case x of
        Lines       -> "with lines"
        Points      -> "with points"
        Dots        -> "with dots"
        Impulses    -> "with impulses"
        Linespoints -> "with linespoints"
        Steps       -> "with steps"


instance GnuplotIdiom Option where
    toString x = case x of
        Title t -> "title \"" ++ t ++ "\""
        Style s -> toString s
        Color c -> "lc rgb \"" ++ toString c ++ "\""
        Width w -> "lw " ++ show w

instance GnuplotIdiom x => GnuplotIdiom [x] where
    toString = unlines . map toString

instance GnuplotIdiom Terminal where
  toString Terminal{..} = case tType of
    PNG{}       -> "set term png" ++ size ',' ++ output
    PDF{}       -> "set term pdf enhanced" ++ size ',' ++ output
    SVG{}       -> "set term svg dynamic" ++ size ',' ++ output
    GIF{}       -> "set term gif" ++ size ',' ++ output
    JPEG{}      -> "set term jpeg" ++ size ',' ++ output
    Latex{}     -> "set term latex" ++ output
    EPS{}       -> "set term postscript eps" ++ size ',' ++ output
    PS{}        -> "set term postscript" ++ size ',' ++ output
    Aqua        -> "set term aqua" ++ title ++ size ' '
    Windows     -> "set term windows" ++ title ++ size ','
    X11         -> "set term x11 persist" ++ title
    Qt          -> "set term qt persist" ++ title ++ size ','
    PNGCairo{}  -> "set term pngcairo" ++ size ',' ++ output
    Dumb        -> "set term dumb feed" ++ size ','
    where
      title  = " title " ++ quoted tName |? (not . null) tName
      output = "; set output " ++ quoted tName
      -- different terminal types have different ways of specifying size
      size c   = maybe "" (\(x, y) -> concat [" size ", show x, [c], show y]) tSize

instance GnuplotIdiom Color where
  toString = \case
    Red          -> "red"
    Blue         -> "blue"
    Green        -> "green"
    Yellow       -> "yellow"
    Orange       -> "orange"
    Magenta      -> "magenta"
    Cyan         -> "cyan"
    DarkRed      -> "dark-red"
    DarkBlue     -> "dark-blue"
    DarkGreen    -> "dark-green"
    DarkYellow   -> "dark-yellow"
    DarkOrange   -> "dark-orange"
    DarkMagenta  -> "dark-magenta"
    DarkCyan     -> "dark-cyan"
    LightRed     -> "light-red"
    LightBlue    -> "light-blue"
    LightGreen   -> "light-green"
    LightMagenta -> "light-magenta"
    Violet       -> "violet"
    Grey         -> "grey"
    White        -> "white"
    Brown        -> "brown"
    DarkGrey     -> "dark-grey"
    Black        -> "black"
    RGB r g b    -> printf "#%02X%02X%02X" r g b
