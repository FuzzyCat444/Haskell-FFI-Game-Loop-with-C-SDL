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
imageFiles :: [String]
imageFiles = []

-- Add sound resources here
soundFiles :: [String]
soundFiles = []

-- Add music resources here
musicFiles :: [String]
musicFiles = []

-- Represents current game state/world
data GameState = GameState

-- Initialize the game state 
-- (Optionally with random number generator)
initGameState :: StdGen -> GameState
initGameState rng = GameState

-- Handle mouse and keyboard events
handleEvent :: GameState -> Event -> GameState
handleEvent gs _ = gs

-- Update game state (60 fps)
update :: GameState -> GameState
update gs = gs

-- Sounds to play (60 fps)
sounds :: GameState -> [Sound]
sounds gs = []

-- Music to play (60 fps)
music :: GameState -> Music
music gs = ResumeMusic

-- Do optional IO (60 fps)
doIO :: GameState -> IO ()
doIO gs = return ()

-- Console-log information from the game state here (60 fps)
-- Prefer this over doIO for logging
logs :: GameState -> [String]
logs gs = []

-- Render game state to list of sprites (no fps limit)
render :: GameState -> [Sprite]
render gs = []

shouldQuit :: GameState -> Bool
shouldQuit gs = False

onQuit :: GameState -> IO ()
onQuit gs = return ()




-- Misc functions (seldom changed)

-- Maximum sprites that can be drawn on the screen
-- More sprites take more memory
maxSprites :: Int
maxSprites = 10000

-- Maximum sounds that can be played at once
maxSounds :: Int
maxSounds = 100
