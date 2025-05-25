module  Types
        ( MazeIx
        , GenerationBias(..)
        , AppState(..)
        , RenderCommand(..)

        , initialAppState
        , getQuadWH
        , emptyMaze

        , modifyAppState
        , modifyAppStateAndPrint

        , module ReExport
        ) where

import           Graphics.Rendering.OpenGL (GLfloat, GLint)

import           Control.Concurrent.MVar   as ReExport
import           Control.Monad             as ReExport
import           Data.Bool                 as ReExport


-- a maze is defined by a list of maze indices [MazeIx].
-- these refer to all maze fields that are walkable.
type MazeIx = (Int, Int)


-- different visual patterns for the maze
data GenerationBias =
    NoBias | Vertical | Horizontal | CheckerBoard  | DiagonalSplit
    deriving (Show, Enum)

-- passed to the renderer callback
data RenderCommand =
      RenderFrame (GLfloat, GLfloat) (Maybe [MazeIx]) [MazeIx]
    | RendererQuit
    | RendererIdle

-- application state
data AppState = AppState
    { asMaze        :: [MazeIx]                                         -- ^ the current maze
    , asDims        :: (Int, Int)                                       -- ^ maze dimensions
    , asQuadWH      :: (GLfloat, GLfloat)                               -- ^ quad dimensions on screen
    , asScreenWH    :: (GLint, GLint)                                   -- ^ current screen resolution
    , asNeedBuild   :: Bool                                             -- ^ need to build a new maze?
    , asNeedSolve   :: Bool                                             -- ^ need to find a solution?
    , asShowBuild   :: Bool                                             -- ^ animate the generation process?
    , asBuildBias   :: GenerationBias                                   -- ^ bias will influence the maze pattern
    , asAnimating   :: Bool                                             -- ^ some animation is in progress
    , asSolution    :: Maybe [MazeIx]                                   -- ^ if a solution has been found, remember it
    , asRenderFrame :: RenderCommand -> IO ()                           -- ^ send a render command to GLUT
    }

instance Show AppState where
    show as = "AppState -- animate: " ++ show (asShowBuild as) ++ "; bias: " ++ show (asBuildBias as)

modifyAppState, modifyAppStateAndPrint :: MVar AppState -> (AppState -> AppState) -> IO ()
modifyAppState mv f =
    let f' a = pure (f a)
    in modifyMVar_ mv f'
modifyAppStateAndPrint mv f =
    let f' a = pure (f a, f a)
    in modifyMVar mv f' >>= print

-- project a maze coordinate (empty cell) onto its corresponding index in the Maze data structure
mazeProject :: Int -> Int
mazeProject = (+1) . (*2)

-- calculate quad size in pixels of a maze cell, depending on screen size and maze dimensions
getQuadWH :: (GLint, GLint) -> (Int, Int) -> (GLfloat, GLfloat)
getQuadWH (w, h) (mazeW, mazeH) =
    ( fromIntegral w / fromIntegral (mazeProject mazeW)
    , fromIntegral h / fromIntegral (mazeProject mazeH)
    )

emptyMaze :: (Int, Int) -> [MazeIx]
emptyMaze (mazeW, mazeH) =
    [ (mazeProject x, mazeProject y)
        | x <- [0 .. mazeW-1]
        , y <- [0 .. mazeH-1]
    ]

initialAppState :: (GLint, GLint) -> (Int, Int) -> AppState
initialAppState screenDims mazeDims =
    AppState
        { asMaze        = emptyMaze mazeDims
        , asDims        = mazeDims
        , asQuadWH      = getQuadWH screenDims mazeDims
        , asScreenWH    = screenDims
        , asNeedBuild   = False
        , asNeedSolve   = False
        , asShowBuild   = False
        , asBuildBias   = NoBias
        , asAnimating   = False
        , asSolution    = Nothing
        , asRenderFrame = \_ -> pure ()
        }
