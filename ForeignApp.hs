{-# LANGUAGE ForeignFunctionInterface #-}

module ForeignApp where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Char
import Data.Ratio
import qualified Data.HashMap.Lazy as Map
import System.Random

import HaskellApp
import Engine

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
img :: String -> Int
img name =
    case id of Nothing -> 0
               Just x  -> x
    where id = Map.lookup name resourceMap
    
hs_initGameStateCallback :: IO (StablePtr GameState)
hs_initGameStateCallback = do
    rng <- getStdGen
    gsPtr <- newStablePtr (initGameState rng)
    return gsPtr
foreign export ccall hs_initGameStateCallback :: IO (StablePtr GameState)

hs_cleanupCallback :: StablePtr GameState -> IO ()
hs_cleanupCallback gsPtr = do
    gameState <- deRefStablePtr gsPtr
    onQuit gameState
    freeStablePtr gsPtr
    return ()
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
    
hs_doIOCallback :: StablePtr GameState -> IO ()
hs_doIOCallback gsPtr = do
    gameState <- deRefStablePtr gsPtr
    doIO gameState
    return ()
foreign export ccall hs_doIOCallback :: StablePtr GameState -> IO ()

hs_renderGameStateCallback :: StablePtr GameState -> Ptr CInt -> IO ()
hs_renderGameStateCallback gsPtr spritesArr = do 
    gameState <- deRefStablePtr gsPtr
    rs <- renderedSprites (render gameState) spritesArr
    return rs
foreign export ccall hs_renderGameStateCallback 
    :: StablePtr GameState -> Ptr CInt -> IO ()
    
hs_writeLogsCallback :: StablePtr GameState -> IO ()
hs_writeLogsCallback gsPtr = do
    gameState <- deRefStablePtr gsPtr
    let strings = logs gameState
    mapM_ putStrLn strings
    return ()
foreign export ccall hs_writeLogsCallback :: StablePtr GameState -> IO ()

hs_shouldQuitCallback :: StablePtr GameState -> IO Bool
hs_shouldQuitCallback gsPtr = do
    gameState <- deRefStablePtr gsPtr
    let quit = shouldQuit gameState
    return quit
foreign export ccall hs_shouldQuitCallback :: StablePtr GameState -> IO Bool

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

hs_eventCallback :: StablePtr GameState -> CInt -> CInt -> CInt -> CDouble -> 
    CDouble -> CDouble -> CDouble -> CDouble -> IO (StablePtr GameState)
hs_eventCallback gsPtr evType key button x y dx dy scroll = do
    let { 
        event = constructEvent e ky but mx my mdx mdy mscr
        where e = fromIntegral evType :: Int
              ky = fromIntegral key :: Int
              but = fromIntegral but :: Int
              mx = realToFrac x
              my = realToFrac y
              mdx = realToFrac dx
              mdy = realToFrac dy
              mscr = realToFrac scroll
    }
    gameState <- deRefStablePtr gsPtr
    freeStablePtr gsPtr
    let nextState = handleEvent gameState event
    newPtr <- newStablePtr nextState
    return newPtr
foreign export ccall hs_eventCallback :: StablePtr GameState -> CInt -> CInt -> 
    CInt -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> 
    IO (StablePtr GameState)
    
serializeSpriteList :: [Sprite] -> [CInt]
serializeSpriteList list = 
    map fromIntegral (length list : doSerializeList list)
    where doSerializeList [] = []
          doSerializeList (x:xs) = serializeSprite x ++ doSerializeList xs
          serializeSprite (Sprite id srcRect dstRect origin angle) = 
              [img id] ++ rectList srcRect ++ rectList dstRect 
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
