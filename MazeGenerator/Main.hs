{-# LANGUAGE RecordWildCards #-}

module Main where

import           GL
import           MazeGenerator
import           Types

import           Control.Concurrent
import           Graphics.Rendering.OpenGL (GLint, ($=))
import qualified Graphics.UI.GLUT          as Glut
import           System.Environment        (getArgs)


mazeDims   :: (Int, Int)
screenDims :: (GLint, GLint)

mazeDims    = (56, 48)            -- empty cells in a maze
screenDims  = (800, 600)          -- initial window dimensions


main :: IO ()
main = do
    putStrLn "Maze Generator (c) by M.G.Meier 2014-15, 2025\n"

    initializeGL screenDims "MazeGenerator"

    appState <- newMVar emptyAppState

    getArgs >>= \case
        ["--speedtest"] ->
            let howMany = 1000
            in do
                putStrLn ("generating " ++ show howMany ++ " mazes")
                Glut.displayCallback    $= glutDisplayCallback appState
                delayed 50              $ replicateM_ howMany (generateMaze appState) >> terminateMainLoop appState
                startMainLoop appState

        _ -> do
            showKeyBindings
            print emptyAppState
            Glut.displayCallback        $= glutDisplayCallback appState
            Glut.reshapeCallback        $= Just (glutReshapeCallback appState)
            Glut.keyboardMouseCallback  $= Just (glutInputCallback appState)
            delayed 50                  $ generateMaze appState
            watchAndDispatch appState
            startMainLoop appState

  where
    emptyAppState = initialAppState screenDims mazeDims

    -- this is used to schedule an action ahead of entering the GLUT mainloop
    -- !! there is no return from the mainloop !!
    delayed ms action = void $ forkIO $ do
      threadDelay $ ms * 1000
      action

    showKeyBindings = putStrLn
        "Key Bindings:\n\
        \  (Space)              - create new maze\n\
        \  (Enter)              - solve maze (animated)\n\
        \  (Arrow left/right)   - adjust maze width\n\
        \  (Arrow up/down)      - adjust maze height\n\
        \  +, -                 - change maze pattern via generation bias\n\
        \  F1                   - toggle step-by-step animated maze creation\n\
        \  Esc                  - quit"

  -- periodically polls the app state to check if a maze has to be generated or solved, triggering the appropriate action
    watchAndDispatch :: MVar AppState -> IO ()
    watchAndDispatch appState = void $ forkIO $
        forever $ do
            AppState{..} <- readMVar appState
            if
                | asAnimating -> pure ()                -- an anmation is running, skip all checks
                | asNeedBuild -> generateMaze appState
                | asNeedSolve -> solveMaze appState
                | otherwise   -> pure ()

            -- wait 50ms before polling again    
            threadDelay $ 50 * 1000
