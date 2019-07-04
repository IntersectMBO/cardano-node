{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module LiveView (
      runNodeLiveView
    ) where

import           Control.Monad (void)

import           Terminal.Game

runNodeLiveView :: Int -> IO ()
runNodeLiveView l = void $
    runGame (MyState (10, 10) Stop False 1)
            (logicFun l)
            drawFun
            gsQuit
            13

boundaries :: (Coords, Coords)
boundaries =
    ( ( 1 :: Row,  1 :: Column)
    , (25 :: Row, 95 :: Column)
    )

data MyState = MyState
    { gsCoord      :: Coords
    , gsMove       :: Move
    , gsQuit       :: Bool
    , gsMemoryPerc :: Int
    } deriving (Show, Eq)

data Move = N | S | E | W | Stop
          deriving (Show, Eq)

logicFun
    :: Int
    -> MyState
    -> Event
    -> MyState
logicFun _ gs (KeyPress 'q') = gs { gsQuit = True }
logicFun _ gs (KeyPress 'i') = gs { gsMemoryPerc = let cur = gsMemoryPerc gs in cur + 1 }
logicFun _ gs Tick           = gs { gsCoord = (1, 1) }
logicFun _ gs (KeyPress _c)  = gs { gsMove = error "TODO" }

header :: Plane
header = blankPlane (85 :: Width) (1 :: Height)
    & (1 :: Row,  1 :: Column) % textBox "CARDANO SL"
                                 (10 :: Width) (1 :: Height)
                               # bold
    & (1 :: Row, 17 :: Column) % textBox "Release: Shelley"
                                 (17 :: Width) (1 :: Height)
    & (1 :: Row, 76 :: Column) % textBox "Node: 00"
                                 (9 :: Width) (1 :: Height)

mempoolStats :: Plane
mempoolStats = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % textBox "Memory pool"
                                 (25 :: Width) (1 :: Height)
                               # bold
    & (1 :: Row, 19 :: Column) % textBox "142 / 71%"
                                 (10 :: Width) (1 :: Height)
    & (3 :: Row,  3 :: Column) % textBox "[||||||||||||||      ]"
                                 (25 :: Width) (1 :: Height)

cpuStats :: Plane
cpuStats = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % textBox "CPU usage"
                                 (25 :: Width) (1 :: Height)
                               # bold
    & (1 :: Row, 19 :: Column) % textBox "8%"
                                 (10 :: Width) (1 :: Height)
    & (3 :: Row,  3 :: Column) % textBox "[|||                 ]"
                                 (25 :: Width) (1 :: Height)

memoryStats :: Int -> Plane
memoryStats memoryPerc = blankPlane (27 :: Width) (3 :: Height)
    & (1 :: Row,  1 :: Column) % textBox "Memory usage"
                                 (25 :: Width) (1 :: Height)
                               # bold
    & (1 :: Row, 19 :: Column) % textBox (show memoryPerc <> " GB")
                                 (20 :: Width) (1 :: Height)
    & (3 :: Row,  3 :: Column) % textBox "[|||||||             ]"
                                 (25 :: Width) (1 :: Height)

systemStats :: Int -> Plane
systemStats memoryPerc = blankPlane (30 :: Width) (17 :: Height)
    & ( 1 :: Row, 1 :: Column) % mempoolStats
    & ( 7 :: Row, 1 :: Column) % cpuStats
    & (13 :: Row, 1 :: Column) % memoryStats memoryPerc

nodeInfoLabels :: Plane
nodeInfoLabels = blankPlane (20 :: Width) (18 :: Height)
    & ( 1 :: Row, 1 :: Column) % textBox "version:"
                                 (10 :: Width) (1 :: Height)
    & ( 2 :: Row, 1 :: Column) % textBox "commit:"
                                 (10 :: Width) (1 :: Height)
    & ( 4 :: Row, 1 :: Column) % textBox "uptime:"
                                 (10 :: Width) (1 :: Height)
    & ( 6 :: Row, 1 :: Column) % textBox "block height:"
                                 (15 :: Width) (1 :: Height)
    & ( 7 :: Row, 1 :: Column) % textBox "minted:"
                                 (15 :: Width) (1 :: Height)
    & ( 9 :: Row, 1 :: Column) % textBox "transactions:"
                                 (15 :: Width) (1 :: Height)
    & (11 :: Row, 1 :: Column) % textBox "peers connected:"
                                 (17 :: Width) (1 :: Height)
    & (13 :: Row, 1 :: Column) % textBox "max network delay:"
                                 (19 :: Width) (1 :: Height)

nodeInfoValues :: Plane
nodeInfoValues = blankPlane (15 :: Width) (18 :: Height)
    & ( 1 :: Row,  1 :: Column) % textBox "3.1.5"
                                  (10 :: Width) (1 :: Height)
                                # bold
    & ( 2 :: Row,  1 :: Column) % textBox "bc39f7124"
                                  (10 :: Width) (1 :: Height)
                                # bold
    & ( 4 :: Row,  1 :: Column) % textBox "00:01:32"
                                  (10 :: Width) (1 :: Height)
                                # bold
    & ( 6 :: Row,  1 :: Column) % textBox "12432"
                                  (10 :: Width) (1 :: Height)
                                # bold
    & ( 7 :: Row,  1 :: Column) % textBox "4281"
                                  (10 :: Width) (1 :: Height)
                                # bold
    & ( 9 :: Row,  1 :: Column) % textBox "174283"
                                  (10 :: Width) (1 :: Height)
                                # bold
    & (11 :: Row,  1 :: Column) % textBox "2"
                                  (10 :: Width) (1 :: Height)
                                # bold
    & (13 :: Row,  1 :: Column) % textBox "7 ms"
                                  (10 :: Width) (1 :: Height)
                                # bold

nodeInfo :: Plane
nodeInfo = blankPlane (40 :: Width) (18 :: Height)
    & (1 :: Row,  1 :: Column) % nodeInfoLabels
    & (1 :: Row, 22 :: Column) % nodeInfoValues

drawFun :: MyState -> Plane
drawFun (MyState (r, c) _ _ memoryPerc) = blankPlane mw mh
    & (1 :: Row,   1 :: Column) % box '*' mw     mh     -- border
    & (2 :: Row,   2 :: Column) % box ' ' (mw-2) (mh-2) -- space inside of border
    & (3 :: Row,   7 :: Column) % header
    & (7 :: Row,   9 :: Column) % systemStats memoryPerc
    & (7 :: Row,  55 :: Column) % nodeInfo
    & (r, c)   % cell '@' # invert
  where
    mh :: Height
    mw :: Width
    (mh, mw) = snd boundaries
