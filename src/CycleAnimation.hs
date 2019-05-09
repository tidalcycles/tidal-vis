module CycleAnimation where


import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Colour.SRGB
import GHC.Int (Int16)
import Graphics.UI.SDL
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render
import Graphics.UI.SDL.TTF.Types
import Sound.Tidal.Context hiding (Event)
import Sound.Tidal.Tempo
import Sound.Tidal.Utils

import qualified GHC.Word
import qualified Graphics.UI.SDL.Framerate as FR
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.TTF.General as TTFG
import qualified Sound.OSC.FD as FD
import qualified Sound.Tidal.Pattern as Pat

import Common


data Scene = Scene
  { mouseXY :: (Float, Float)
  , cursor  :: (Float, Float)
  }

data AppConfig = AppConfig
  { acScreen  :: Surface
  , acFont    :: Font
  , acTempo   :: MVar Tempo
  , acFps     :: FR.FPSManager
  , acPattern :: MVar Pat.ControlPattern
  }

type AppState = StateT Scene IO

type AppEnv = ReaderT AppConfig AppState

run' :: MVar ControlPattern -> IO ()
run' mp = withInit [InitEverything] $ do
  result <- TTFG.init
  if not result
    then putStrLn "Failed to init ttf"
    else do
      enableUnicode True
      env <- initEnv mp
      --ws <- wordMenu (font env) things
      let scene = Scene (0,0) (0.5,0.5)
      runLoop env scene

runLoop :: AppConfig -> Scene -> IO ()
runLoop = evalStateT . runReaderT looping

-- | Animate pattern looply.
-- | Choose form of pattern within 'loop'.
looping :: AppEnv ()
looping = do
    quit' <- whileEvents action
    screen <- acScreen `liftM` ask
    tempoM <- acTempo `liftM` ask
    fps <- acFps `liftM` ask
    mp <- acPattern `liftM` ask
    liftIO $ do
        pat <- readMVar mp
        appendFile "pat" $ show pat ++ "\n\n"
        tempo <- readMVar tempoM
        beat <- beatNow tempo
        bgColor <- (mapRGB . surfaceGetPixelFormat) screen 0x00 0x00 0x00
        clipRect <- Just `liftM` getClipRect screen
        void $ fillRect screen clipRect bgColor

        -- | Use one of
        --
        -- | (1) Cicle form of moving patterns
        -- drawPatC (100, fi screenHeight / 2) (dirtToColour pat) screen beat

        -- | (2) Rectangular form of moving patterns
        drawPatR (0, fi screenHeight) (dirtToColour pat) screen beat

        Graphics.UI.SDL.flip screen
        FR.delay fps
    unless quit' looping
  where
    action e = do
      scene <- get
      scene' <- handleEvent scene e
      put scene'

initEnv :: MVar ControlPattern -> IO AppConfig
initEnv mp = do
    time' <- FD.time
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    font' <- openFont "futura.ttf" 22
    setCaption "Cycle" []
    tempoMV' <- remoteLocal defaultConfig time'
    fps <- FR.new
    FR.init fps
    return $ AppConfig screen font' tempoMV' fps mp

-- Draw one cycle pattern.
drawArc
  :: Surface
  -> ColourD
  -> (Double, Double) -- Middle`s coord
  -> (Double, Double) -- Torus`s internal and external radiuses.
  -> Double -- (pi*2) * fromRational (s - (toRational $ beat / 8))
  -> Double -- ((pi*2) * fromRational (e-s))
  -> Double -- pace
  -> IO ()
drawArc screen c (x,y) (r,r') t o pace
    | o <= 0 = return ()
    | otherwise = do
        let pix = colourToPixel c
        void $ SDLP.filledPolygon screen coords pix
        drawArc screen c (x,y) (r,r') t (o - pace) pace
        return ()
  where
    a = max t (t + o - pace) -- start width
    b = t + o -- end width
    coords :: [(Int16, Int16)]
    coords = map (\(x',y') -> (floor $ x + x', floor $ y + y'))
        [ (r * cos a, r * sin a) -- 1
        , (r' * cos a, r' * sin a) -- 2
        , (r' * cos b, r' * sin b) -- 3
        , (r * cos b, r * sin b) -- 4
        ]

-- Draw cycle patterns continiously.
drawPatC
    :: (Double, Double)
    -> Pat.Pattern ColourD
    -> Surface
    -> Double
    -> IO ()
drawPatC (r,r') pat screen beat = mapM_ drawEvents $ event (pos beat) pat
  where
    drawEvents :: ((Rational, Rational), [ColourD]) -> IO ()
    drawEvents ((b,e), cs) =
        mapM_ (\(index', color) -> drawEvent (b,e) color index' (length cs))
            (enumerate $ reverse cs)

    drawEvent :: (Rational, Rational) -> ColourD -> Int -> Int -> IO ()
    drawEvent (b, e) color index' len = do
        let thickness = (1 / fromIntegral len) * (r' - r)
        let thickIndex = r + thickness * fromIntegral index'

        drawArc screen color middle (thickIndex, thickIndex + thickness)
            ((pi*2) * fromRational (b - pos beat)) ((pi*2) * fromRational (e - b)) (pi/16)

-- Draw one rectangle pattern
drawRect :: Surface
  -> ColourD
  -> (Double, Double) -- thickIndex, thickIndex + thickness
  -> Double  -- ((pi*2) * fromRational (start - pos))
  -> Double  -- ((pi*2) * fromRational (end - start))
  -> Double  -- pace (pi/16)
  -> IO ()
drawRect screen c (thickStart,thickEnd) t o pace
    | o <= 0 = return ()
    | otherwise = do
        let pix = colourToPixel c
        void $ SDLP.filledPolygon screen coords pix
        drawRect screen c (thickStart, thickEnd) t (o - pace) pace
        return ()
  where
    a = max t (t + o - pace) --
    b = t + o

    coords = map (\(x',y') -> (floor x', floor y'))
        [ (b, thickStart) -- 1
        , (b, thickEnd) -- 2
        , (a, thickEnd) -- 3
        , (a, thickStart) -- 4
        ]

-- Draw rectangle patterns continiously
drawPatR :: (Double, Double) -> Pat.Pattern ColourD -> Surface -> Double -> IO ()
drawPatR (x1,x2) p screen beat = mapM_ drawEvents $ event (pos beat) p
  where
    drawEvents :: ((Rational, Rational), [ColourD]) -> IO ()
    drawEvents ((b, e), cs) =
      mapM_ (\(index', c) -> drawEvent (b, e) c index' (length cs)) (enumerate $ reverse cs)

    drawEvent :: (Rational, Rational) -> ColourD -> Int -> Int -> IO ()
    drawEvent (b, e) color index' len = do
        let thickness = (1 / fromIntegral len) * (x2 - x1)
        let thickIndex = thickness * fromIntegral index'
        let width = fi screenWidth
        drawRect screen color (thickIndex, thickIndex + thickness)
            (width * fromRational (b - pos beat)) (width * fromRational (e - b)) 1

event :: Rational -> Pat.Pattern ColourD -> [((Rational, Rational), [ColourD])]
event position pat = map (\(Pat.Event _ Arc{..} events) ->
    ((max start position, min stop (position + 1)), events))
        $ queryArc (segmentator pat) (Arc position (position + 1))

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents action = do
    ev <- liftIO pollEvent
    case ev of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            action ev
            whileEvents action

textSize :: String -> Font -> IO (Float,Float)
textSize text font' =
  do message <- renderTextSolid font' text (Color 0 0 0)
     return (fromScreen (surfaceGetWidth message, surfaceGetHeight message))

colourToPixel :: Colour Double -> Pixel
colourToPixel c = rgbColor (floor $ r*255) (floor $ g*255) (floor $ b *255)
  where (RGB r g b) = toSRGB c

colourToPixelS :: Surface -> Colour Double -> IO Pixel
colourToPixelS surface c =
    (mapRGB . surfaceGetPixelFormat) surface (floor $ r*255) (floor $ g*255) (floor $ b*255)
  where (RGB r g b) = toSRGB c

rgbColor :: GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8 -> Pixel
rgbColor r g b = Pixel
  ( shiftL (fi r) 24
  .|. shiftL (fi g) 16
  .|. shiftL (fi b) 8
  .|. fi (255 :: Integer)
  )

pixel :: Surface -> (GHC.Word.Word8,GHC.Word.Word8,GHC.Word.Word8) -> IO Pixel
pixel face (r,g,b) = mapRGB (surfaceGetPixelFormat face) r g b

screenWidth :: Int
screenWidth = 500

screenHeight :: Int
screenHeight = 400

screenBpp :: Int
screenBpp = 32

-- A middle of window.
middle :: (Double, Double)
middle = (fromIntegral $ screenWidth `div` 2, fromIntegral $ screenHeight `div` 2)

fromScreen :: (Int, Int) -> (Float, Float)
fromScreen (x, y) =
  ( fromIntegral x / fromIntegral screenWidth
  , fromIntegral y / fromIntegral screenHeight
  )

pos :: Double -> Rational
pos beat = toRational $ beat / 8

isInside :: Integral a => Rect -> a -> a -> Bool
isInside (Rect rx ry rw rh) x y =
    (x' > rx) && (x' < rx + rw) && (y' > ry) && (y' < ry + rh)
  where (x', y') = (fromIntegral x, fromIntegral y)

ctrlDown :: [Modifier] -> Bool
ctrlDown = any (`elem` [KeyModLeftCtrl, KeyModRightCtrl])

shiftDown :: [Modifier] -> Bool
shiftDown = any (`elem` [ KeyModLeftShift, KeyModRightShift, KeyModShift])

handleEvent :: Scene -> Event -> AppEnv Scene
handleEvent scene (KeyDown k) =
    handleKey scene (symKey k) (symUnicode k) (symModifiers k)
handleEvent scene _ = return scene

handleKey :: Scene -> SDLKey -> Char -> [Modifier] -> AppEnv Scene
handleKey scene SDLK_SPACE _ _ = return scene
handleKey scene _ _ _          = return scene

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst rect
  where rect = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }
