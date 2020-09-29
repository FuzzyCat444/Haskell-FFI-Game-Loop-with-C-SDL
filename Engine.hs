module Engine where

-- Event system
data Event = KeyDown Key | KeyUp Key 
           | MouseDown Button (Double, Double) -- (x, y)
           | MouseUp Button (Double, Double) -- (x, y)
           | MouseMove (Double, Double) (Double, Double) -- (x, y) (dx, dy)
           | MouseWheel Double (Double, Double) -- scroll, (x, y)
           
data Button = LeftMouseButton | MiddleMouseButton | RightMouseButton

data Key = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
         | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT 
         | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ 
         
         | KeyComma | KeyPeriod | KeyForwardSlash | KeyBackSlash | KeyMinus
         | KeyEquals | KeyLBracket | KeyRBracket | KeySemicolon | KeyQuote
         | KeyBackQuote
         
         | KeyFN | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5 | KeyF6 | KeyF7
         | KeyF8 | KeyF9 | KeyF10 | KeyF11 | KeyF12
         
         | Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
         
         | KeySpace | KeyTab | KeyLShift | KeyRShift | KeyLCtrl | KeyRCtrl 
         | KeyLAlt | KeyRAlt | KeyDel | KeyIns | KeyEsc | KeyLeftArrow 
         | KeyRightArrow | KeyUpArrow | KeyDownArrow | KeyEnter | KeyBackspace
         
keyFromId :: Int -> Key
keyFromId 0 = KeyA
keyFromId 1 = KeyB
keyFromId 2 = KeyC
keyFromId 3 = KeyD
keyFromId 4 = KeyE
keyFromId 5 = KeyF
keyFromId 6 = KeyG
keyFromId 7 = KeyH
keyFromId 8 = KeyI
keyFromId 9 = KeyJ
keyFromId 10 = KeyK
keyFromId 11 = KeyL
keyFromId 12 = KeyM
keyFromId 13 = KeyN
keyFromId 14 = KeyO
keyFromId 15 = KeyP
keyFromId 16 = KeyQ
keyFromId 17 = KeyR
keyFromId 18 = KeyS
keyFromId 19 = KeyT
keyFromId 20 = KeyU
keyFromId 21 = KeyV
keyFromId 22 = KeyW
keyFromId 23 = KeyX
keyFromId 24 = KeyY
keyFromId 25 = KeyZ
keyFromId 26 = KeyComma
keyFromId 27 = KeyPeriod
keyFromId 28 = KeyForwardSlash
keyFromId 29 = KeyBackSlash
keyFromId 30 = KeyMinus
keyFromId 31 = KeyEquals
keyFromId 32 = KeyLBracket
keyFromId 33 = KeyRBracket
keyFromId 34 = KeySemicolon
keyFromId 35 = KeyQuote
keyFromId 36 = KeyBackQuote
keyFromId 37 = KeyF1
keyFromId 38 = KeyF2
keyFromId 39 = KeyF3
keyFromId 40 = KeyF4
keyFromId 41 = KeyF5
keyFromId 42 = KeyF6
keyFromId 43 = KeyF7
keyFromId 44 = KeyF8
keyFromId 45 = KeyF9
keyFromId 46 = KeyF10
keyFromId 47 = KeyF11
keyFromId 48 = KeyF12
keyFromId 49 = Key0
keyFromId 50 = Key1
keyFromId 51 = Key2
keyFromId 52 = Key3
keyFromId 53 = Key4
keyFromId 54 = Key5
keyFromId 55 = Key6
keyFromId 56 = Key7
keyFromId 57 = Key8
keyFromId 58 = Key9
keyFromId 59 = KeySpace
keyFromId 60 = KeyTab
keyFromId 61 = KeyLShift
keyFromId 62 = KeyRShift
keyFromId 63 = KeyLCtrl
keyFromId 64 = KeyRCtrl
keyFromId 65 = KeyLAlt
keyFromId 66 = KeyRAlt
keyFromId 67 = KeyDel
keyFromId 68 = KeyIns
keyFromId 69 = KeyEsc
keyFromId 70 = KeyLeftArrow
keyFromId 71 = KeyRightArrow
keyFromId 72 = KeyUpArrow
keyFromId 73 = KeyDownArrow
keyFromId 74 = KeyEnter
keyFromId 75 = KeyBackspace

mouseButtonFromId :: Int -> Button
mouseButtonFromId 0 = LeftMouseButton
mouseButtonFromId 1 = MiddleMouseButton
mouseButtonFromId 2 = RightMouseButton
         
constructEvent :: Int -> Int -> Int -> Double -> Double -> 
    Double -> Double -> Double -> Event
constructEvent 0 ky _ _ _ _ _ _ = KeyDown (keyFromId ky)
constructEvent 1 ky _ _ _ _ _ _ = KeyUp (keyFromId ky)
constructEvent 2 _ but mx my _ _ _ = MouseDown theButton (mx, my)
    where theButton = mouseButtonFromId but
constructEvent 3 _ but mx my _ _ _ = MouseUp theButton (mx, my)
    where theButton = mouseButtonFromId but
constructEvent 4 _ _ mx my mdx mdy _ = MouseMove (mx, my) (mdx, mdy)
constructEvent 5 _ _ mx my _ _ mscr = MouseWheel mscr (mx, my)
constructEvent _ _ _ _ _ _ _ _ = MouseWheel 0.0 (0.0, 0.0)
--constructEvent e ky but mx my mdx mdy mscr = KeyDown KeyA

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
        id :: String,
        srcRect :: Rect,
        dstRect :: Rect,
        origin :: Point,
        angle :: Double
    }
