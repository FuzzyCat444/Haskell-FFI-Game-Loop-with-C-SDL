{-# LANGUAGE ForeignFunctionInterface #-}

module HaskellApp where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Char
import Data.Ratio
import qualified Data.HashMap.Lazy as Map

-- VVVVVVVVVVVVVVVVV Your Application Code Here VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV

windowTitle :: String
windowTitle = "C/SDL Haskell App" -- Change window title here

windowSize :: (Int, Int)
windowSize = (800, 600) -- Change window size here

resources :: [String]
resources = ["hello.bmp", "goodbye.bmp"] -- Add image resources here

data GameState = AngleState Double -- Represents current game state/world

initGameState :: GameState
initGameState = AngleState 0 -- Initialize the game state

update :: GameState -> GameState
update (AngleState a) = AngleState (a + 0.1) -- Update game state (60 fps)

render :: GameState -> [Sprite]
render (AngleState a) = [
        Sprite (img "goodbye.bmp") NR NR (Point 400 300) a
    ] -- Render game state to list of sprites

-- ^^^^^^^^^^^^^^^^^ Your Application Code Here ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

hs_windowTitleCallback :: CString -> IO ()
hs_windowTitleCallback titleArr = 
    writeStringToArray titleArr windowTitle
foreign export ccall hs_windowTitleCallback :: CString -> IO ()


hs_windowSizeCallback :: Ptr CInt -> IO ()
hs_windowSizeCallback sizeArr = 
    writeListToArray sizeArr 
    (let (w, h) = windowSize in map fromIntegral [w, h])
foreign export ccall hs_windowSizeCallback :: Ptr CInt -> IO ()

resourceMap :: Map.HashMap String Int
resourceMap = Map.fromList $ zip resources [0..length resources - 1]
img :: String -> Int -- Use this function to map image names to sprite id's
img name =
    case id of Nothing -> 0
               Just x  -> x
    where id = Map.lookup name resourceMap
    
hs_initGameStateCallback :: IO (StablePtr GameState)
hs_initGameStateCallback = newStablePtr initGameState
foreign export ccall hs_initGameStateCallback :: IO (StablePtr GameState)

hs_cleanupCallback :: StablePtr GameState -> IO ()
hs_cleanupCallback gsPtr = freeStablePtr gsPtr
foreign export ccall hs_cleanupCallback :: StablePtr GameState -> IO ()

hs_updateGameStateCallback :: StablePtr GameState -> IO (StablePtr GameState)
hs_updateGameStateCallback gsPtr = do
    gameState <- deRefStablePtr gsPtr
    freeStablePtr gsPtr
    let nextState = update gameState
    newPtr <- newStablePtr nextState
    return newPtr
foreign export ccall hs_updateGameStateCallback :: 
    StablePtr GameState -> IO (StablePtr GameState)

hs_renderGameStateCallback :: StablePtr GameState -> Ptr CInt -> IO ()
hs_renderGameStateCallback gsPtr spritesArr = do 
    gameState <- deRefStablePtr gsPtr
    rs <- renderedSprites (render gameState) spritesArr
    return rs
foreign export ccall hs_renderGameStateCallback 
    :: StablePtr GameState -> Ptr CInt -> IO ()

hs_imageResourcesCallback :: IO (Ptr CString)
hs_imageResourcesCallback = do
    cStrs <- cStrings $ show (length resources) : resources
    arr <- newArray cStrs
    return arr
    where cStrings [] = return []
          cStrings (x:xs) = do
              cStr <- newCString x
              cStrs <- cStrings xs
              return (cStr : cStrs)
foreign export ccall hs_imageResourcesCallback :: IO (Ptr CString)

hs_maxSpritesCallback :: CInt
hs_maxSpritesCallback = 1000 -- Change max number of sprites on screen here
foreign export ccall hs_maxSpritesCallback :: CInt

-- Event system
data Event = KeyDown Key | KeyUp Key 
           | MouseDown Button | MouseUp Button
           | MouseMove (Double, Double)
           | MouseWheel Double 
           
data Button = LeftMouseButton | RightMouseButton

data Key = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
         | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT 
         | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
         
         | Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9

-- Utility data types and functions to render game state
data Rect = 
    Rect Int Int Int Int |
    NR -- Null rect (use full region of sprite/screen)
    
rectList :: Rect -> [Int]
rectList NR = [0, 0, 0, 0]
rectList (Rect x y w h) = [x, y, w, h]

data Point =
    Point Int Int |
    NP -- Null point (use origin)
    
pointList :: Point -> [Int]
pointList NP = [0, 0]
pointList (Point x y) = [x, y]

data Sprite = 
    Sprite {
        id :: Int,
        srcRect :: Rect,
        dstRect :: Rect,
        origin :: Point,
        angle :: Double
    }
    
serializeSpriteList :: [Sprite] -> [CInt]
serializeSpriteList list = 
    map fromIntegral (length list : doSerializeList list)
    where doSerializeList [] = []
          doSerializeList (x:xs) = serializeSprite x ++ doSerializeList xs
          serializeSprite (Sprite id srcRect dstRect origin angle) = 
              [id] ++ rectList srcRect ++ rectList dstRect 
              ++ pointList origin 
              ++ (map fromIntegral [numerator angRatio, denominator angRatio])
              where angRatio = approxRational angle 0.0000001
              
renderedSprites :: [Sprite] -> Ptr CInt -> IO ()
renderedSprites sprites spritesArr = 
    writeListToArray spritesArr $ serializeSpriteList sprites

-- Utility functions for writing array data to C/SDL application
writeListToArray :: Storable a => Ptr a -> [a] -> IO ()
writeListToArray arrPtr xs = do
    iterateList arrPtr xs 0
    where iterateList arrPtr [] elem = return ()
          iterateList arrPtr (x:xs) elem = do
              pokeElemOff arrPtr elem x
              iterateList arrPtr xs (elem + 1)
              
writeStringToArray :: CString -> String -> IO ()
writeStringToArray arrPtr str = 
    writeListToArray arrPtr $ map (fromIntegral . ord) (str ++ "\0")
