{-# LANGUAGE RecordWildCards #-}

module  GL where

import           Types

import           Graphics.Rendering.OpenGL as Gl
import           Graphics.UI.GLUT          as Glut

import           Control.Exception

import           Data.Char                 (ord)


colorPath, colorSolution :: Color3 GLfloat
colorWall :: Color4 GLfloat

colorPath     = Color3 0.8 0.8 0.8          -- maze path: white-ish light gray
colorSolution = Color3 0.6 0.1 0.1          -- solution path: reddish
colorWall     = Color4 0.1 0.1 0.1 1.0      -- maze walls: dark grey


-- produces a singleframe, rendering the maze with given quad sizes.
-- a list of soution cells ndicating the way through the maze is optional.
-- NB: always converts and draws everything -- completely unoptimized
showMaze :: (GLfloat, GLfloat) -> Maybe [MazeIx] -> [MazeIx] -> IO ()
showMaze (w, h) solutionCells maze = do
    Gl.clear [ColorBuffer]
    Gl.color colorPath
    Gl.unsafeRenderPrimitive Quads $
        mapM_ drawQuad maze
    forM_ solutionCells $ \rs -> do
        Gl.color colorSolution
        Gl.unsafeRenderPrimitive Quads $
            mapM_ drawQuad rs
    Glut.swapBuffers

  where
    glVertex2f x y = Gl.vertex $ Vertex2 x y

    -- draws a quad (counter-clockwise)
    drawQuad (blX, blY) = do
        let
            x = fromIntegral blX * w
            y = fromIntegral blY * h
        glVertex2f x y
        glVertex2f (x+w) y
        glVertex2f (x+w) (y+h)
        glVertex2f x (y+h)


glutInputCallback :: MVar AppState -> Glut.KeyboardMouseCallback
glutInputCallback appState key Down _ _ = do
    AppState{asDims = (w, h), ..} <- readMVar appState
    let
        newMazeDims mazeDims' =
             modifyMVar_ appState $ \st -> return st
                { asDims        = mazeDims'
                , asQuadWH      = getQuadWH asScreenWH mazeDims'
                , asNeedBuild   = True
                , asShowBuild   = False
                }

        cycleBias f =
            try (evaluate $ f asBuildBias) >>= either
                (\SomeException{} -> return ())
                (\bias' ->  modifyAppStateAndPrint appState $ \st -> st
                    { asNeedBuild   = True
                    , asShowBuild   = False
                    , asBuildBias   = bias'
                    })

    -- disregard keyboard input during animation
    unless asAnimating $ case key of
        Char c
            | ord c == 27   -> terminateMainLoop appState
            | c == '+'      -> cycleBias succ
            | c == '-'      -> cycleBias pred
            | c == ' '      ->
                 modifyAppState appState $ \st -> st {asNeedBuild = True}
            | ord c == 13   ->
                 modifyAppState appState $ \st -> st {asNeedSolve = True}

        SpecialKey sk
            | sk == KeyLeft     && w > 8    -> newMazeDims (w-1, h)
            | sk == KeyRight    && w < 256  -> newMazeDims (w+1, h)
            | sk == KeyUp       && h < 256  -> newMazeDims (w, h+1)
            | sk == KeyDown     && h > 8    -> newMazeDims (w, h-1)
            | sk == KeyF1 ->
                 modifyAppStateAndPrint appState $ \st -> st {asShowBuild = not asShowBuild}

        _   -> return ()

glutInputCallback _ _ _ _ _ =
    return ()


glutReshapeCallback :: MVar AppState -> Glut.ReshapeCallback
glutReshapeCallback appState (Size w h) = do
    glSetup2D (w, h)
    dims <- asDims `fmap` readMVar appState
    modifyMVar_ appState $ \st -> return st
        { asQuadWH      = getQuadWH (w, h) dims
        , asScreenWH    = (w, h)
        }
    Glut.postRedisplay Nothing

glutDisplayCallback :: MVar AppState -> Glut.DisplayCallback
glutDisplayCallback appState = do
    AppState{..} <- readMVar appState
    -- the callback should only display the static maze, immediately; animation is handled by generateMaze / solveMaze themselves
    unless asAnimating $
        showMaze asQuadWH asSolution asMaze


-- start/terminateMainLoop work around missing function
-- 'Glut.leaveMainLoop' when not using freeGLUT
terminateMainLoop :: MVar AppState -> IO ()
terminateMainLoop appState = do
    -- obsolete any frame that may still have an open render request
    -- to guard against GLUT calls erroring without a window
    as <- readMVar appState
    asRenderFrame as RendererQuit
    Gl.get Glut.currentWindow >>= mapM_ Glut.destroyWindow

startMainLoop :: MVar AppState -> IO ()
startMainLoop appState = do
    renderArgs <- newMVar RendererIdle
    let
        -- 20ms delay -> max 50fps
        frameDelayLoop = Glut.addTimerCallback 20 renderCallback

        -- if there's an unprocessed render request, it will be dropped in favor of the incoming one
        renderCommand = void . swapMVar renderArgs

        -- by using a periodic GLUT timer callback, we ensure that
        -- 1. it executes in the same thread as the GLUT mainloop
        -- 2. it doesn't block the mainloop
        -- 3. there may be GLUT implementations where this is unnecessary, but better safe than sorry
        renderCallback :: Glut.TimerCallback
        renderCallback = 
            swapMVar renderArgs RendererIdle >>= \case
                RendererIdle        -> frameDelayLoop
                RenderFrame a b c   -> showMaze a b c >> frameDelayLoop
                RendererQuit        -> pure ()

    modifyAppState appState $ \st -> st {asRenderFrame = renderCommand}
    frameDelayLoop
    Glut.mainLoop

-- set up OpenGL for a 2D scene
glSetup2D :: (GLsizei, GLsizei) -> IO ()
glSetup2D (w, h) = do
    Gl.viewport             $= (Position 0 0, Size w h)
    Gl.matrixMode           $= Projection
    Gl.loadIdentity
    Gl.ortho                0.0 (fromIntegral w) 0.0 (fromIntegral h) 0.0 1.0
    Gl.matrixMode           $= Modelview 0
    Gl.loadIdentity

-- OpenGL initialization
initializeGL :: (GLint, GLint) -> String -> IO ()
initializeGL screenDims windowName = do
    _ <- Glut.getArgsAndInitialize
    Glut.initialDisplayMode $= [DoubleBuffered, RGBMode]
    Glut.initialWindowSize  $= uncurry Size screenDims
    _ <- Glut.createWindow  windowName
    glSetup2D               screenDims
    Gl.clearColor           $= colorWall
