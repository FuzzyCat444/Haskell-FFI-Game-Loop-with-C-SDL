module HaskellApp where

import System.Random
import Engine

-- Change window title here
windowTitle :: String
windowTitle = "C/SDL Haskell App" 

-- Change window size here
windowSize :: (Int, Int)
windowSize = (800, 600) 

-- Add image resources here
resources :: [String]
resources = ["hello.bmp"] 

-- Represents current game state/world
data GameState = GameState Double StdGen

-- Initialize the game state with random number generator
initGameState :: StdGen -> GameState
initGameState rng = GameState 0.0 rng

-- Handle mouse and keyboard events
handleEvent :: GameState -> Event -> GameState
handleEvent (GameState angle rng) (MouseWheel scr _) = GameState (angle + scr * 5) rng
handleEvent gs _ = gs

-- Update game state (60 fps)
update :: GameState -> GameState
update gs = gs 

-- Do optional IO (60 fps)
doIO :: GameState -> IO ()
doIO gs = return ()

-- Console-log information from the game state here (60 fps)
-- Prefer this over doIO for logging
logs :: GameState -> [String]
logs (GameState angle rng) 
    = [ 
          show $ take 3 $ (randomRs (0.0, 1.0) rng :: [Double])
      ]

-- Render game state to list of sprites (no fps limit)
render :: GameState -> [Sprite]
render (GameState angle rng) 
    = [
          Sprite "hello.bmp" NR NR (Point 400 300) angle
      ] 

shouldQuit :: GameState -> Bool
shouldQuit gs = let (GameState a _) = gs in a > 45

onQuit :: GameState -> IO ()
onQuit gs = return ()
