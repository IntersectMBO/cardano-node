{-# LANGUAGE RecordWildCards #-}

module MazeGenerator where

import           RandomGeneration
import           Types

import           Prelude            hiding (rem)

import           Control.Concurrent
import qualified Data.Set           as S


{-
  Source:
    http://en.wikipedia.org/wiki/Maze_generation#Recursive_backtracker

    The depth-first search algorithm of maze generation is frequently
    implemented using backtracking:

    Make the initial cell the current cell and mark it as visited
    While there are unvisited cells
        If the current cell has any unvisited neighbours                -- see (Note: 1)
            Choose randomly one of the unvisited neighbours
            Push the current cell to the stack
            Remove the wall between the current cell and the chosen cell
            Make the chosen cell the current cell and mark it as visited
        Else if stack is not empty                                      -- see (Note: 2)
            Pop a cell from the stack
            Make it the current cell
        Else                                                            -- see (Note: 3)
            Pick a random unvisited cell, make it the current cell and
            mark it as visited
-}

-- tail-recursive implementation of Depth-first search algorithm
-- * buildSnapshot must announce a visual update of the generation process
-- * randomFunc must make a random pick which is GenerationBias-aware
depthFirstSearch
    :: ([MazeIx] -> IO ())
    -> (MazeIx -> [MazeIx] -> IO MazeIx)
    -> (MazeIx -> [MazeIx])
    -> [MazeIx]
    -> Int
    -> IO [MazeIx]
depthFirstSearch buildSnapshot randomFunc neighboursAround maze_ =
    depthFirstSearch' maze_ S.empty (0, 0) []                           -- (0, 0) with empty stack results in a random starting point
  where
    emptyCells = S.fromList maze_

    depthFirstSearch' maze _ _ _ 0 = return maze                        -- no remaining unvisited cells? done!
    depthFirstSearch' maze visit current@(cx, cy) stack rem =
        let
            unvisitedNeighbs =
                [ix | ix <- neighboursAround current, not (S.member ix visit)]

            (visit', rem')                                              -- adjust remaining unvisited cell count and mark current cell as visited if necessary
                | S.member current visit   = (visit, rem)
                | otherwise                = (S.insert current visit, rem-1)

        in if null unvisitedNeighbs
            then case stack of
                [] -> do
                    next <- randomElement $
                        S.toList (S.difference emptyCells visit)        -- all unvisited cells is the set difference between empty and visited cells
                    depthFirstSearch' maze visit' next stack rem'       -- Note: 3
                c:cs ->
                    depthFirstSearch' maze visit' c cs rem'             -- Note: 2

            else do
                next@(nx, ny) <- randomFunc current unvisitedNeighbs
                let
                    tearDown    = ((cx+nx) `div` 2, (cy+ny) `div` 2)
                    maze'       = tearDown:maze
                buildSnapshot maze'                                          -- things have changed; give an update to whom it may concern
                depthFirstSearch' maze' visit' next (current:stack) rem'     -- Note: 1


-- (possibly animated) generation of a new maze conforming to the
-- parameters held in the application state.
generateMaze :: MVar AppState -> IO ()
generateMaze appState = do
    AppState {..} <- readMVar appState
    let
        -- the neighboring empty maze cells
        neighboursAround (x, y) =
            filter clipping [(x-2, y), (x+2, y), (x, y-2), (x, y+2)]
          where
            clipping (i, j) =
                i > 0 && j > 0 && i < 2 * fst asDims && j < 2 * snd asDims

        -- visualizes the generation process; increase delay to slow down the animation
        buildSnapshot xs = do
            asRenderFrame $ RenderFrame asQuadWH Nothing xs
            threadDelay 2000

    modifyAppState appState $ \st -> st {asNeedBuild = False, asAnimating = True, asSolution = Nothing}
    maze <- depthFirstSearch
                (if asShowBuild then buildSnapshot else const (pure ()))
                (randomBias asBuildBias asDims)
                neighboursAround
                (emptyMaze asDims)
                (uncurry (*) asDims)
    modifyAppState appState $ \st -> st {asMaze = maze, asAnimating = False}
    asRenderFrame $ RenderFrame asQuadWH Nothing maze


-- animates the algorithm that solves the current maze on display.
-- the solver keeps track of all paths it is walking
-- simultaneously as a list of paths. it tries to extend
-- each path with adjacent unused maze indices. a path which can't be
-- extended further has either hit the exit, or it's removed from the list.
-- NB. this algorithm can't solve mazes with circular paths.
solveMaze :: MVar AppState -> IO ()
solveMaze appState = do
    AppState{..} <- readMVar appState
    let
        enter   = (0, 1)                                                   -- punch a hole in the wall bottom left...
        exit    = let (right, upper) = maximum asMaze in (right+1, upper)  -- ...and top right
        maze    = exit : asMaze

        doesExit     = dropWhile ((/= exit) . head)                     -- look for the first path to hit the exit
        moves (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

        solveRecursive free sols
            | S.null free || null sols = pure Nothing                   -- conditions on which a maze is unsolvable
            | otherwise = do
                let
                    sols' = concat [ map (:sol) ms
                        | sol@(s:_) <- sols
                        , let ms = filter (`S.member` free) $ moves s
                        , (not . null) ms
                        ]
                    free' = foldr (S.delete . head) free sols'          -- remove recent path extensions from the Set
                    reds  = (S.toList . S.fromList . concat) sols'      -- dedupe for drawing only; completely unoptimized

                asRenderFrame $ RenderFrame asQuadWH (Just reds) maze
                threadDelay 7500                                        -- increase delay to slow down the animation

                case doesExit sols' of
                    x:_ -> pure $ Just x
                    _   -> solveRecursive free' sols'

    modifyAppState appState $ \st -> st {asNeedSolve = False, asAnimating = True, asSolution = Nothing}
    sol <- solveRecursive (S.fromList maze) [[enter]]
    modifyAppState appState $ \st -> st {asSolution = sol, asAnimating = False}
    asRenderFrame $ RenderFrame asQuadWH sol maze
