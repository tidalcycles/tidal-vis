module VisGradient
       ( renderGradientSVG
       , renderGradientPDF
       ) where

import Data.Colour.SRGB
import Sound.Tidal.Context
import Sound.Tidal.Utils

import qualified Graphics.Rendering.Cairo as C

import Common


-- | Constans
totalWidth :: Double
totalWidth = 1700

ratio :: Double
ratio = 3/40

levelHeight :: Double
levelHeight = totalWidth * ratio

v :: (FilePath -> Double -> Double -> (C.Surface -> IO ()) -> IO ())
  -> FilePath
  -> (Double, Double)
  -> [[Event ColourD]]
  -> IO ()
v sf fn (x,y) colorEvents = sf fn x y $ \surf ->
    C.renderWith surf $ do
        C.save
        -- C.scale x (y / (fromIntegral $ length colorEvents))
        C.setOperator C.OperatorOver
        -- C.setSourceRGB 0 0 0
        -- C.rectangle 0 0 1 1
        --C.fill
        mapM_ (renderLevel (length colorEvents)) $ enumerate colorEvents
        C.restore

renderLevel
  :: (Foldable t, Integral a)
  => p
  -> (a, t (Event ColourD))
  -> C.Render ()
renderLevel _ (num, level) = do
    C.save
    mapM_ drawEvent $ level
    C.restore
  where
    drawEvent e@(Event _ Arc{..} c) = do
        let (Arc sWhole eWhole) = wholeOrPart e
        let (RGB r g b) = toSRGB c
        let x = (fromRational start) * totalWidth
        let y = (fromIntegral num) * levelHeight
        let xWhole = (fromRational sWhole) * totalWidth
        -- let w = levelHeight
        let lineW = (fromRational (stop - start) * totalWidth)
        let wholeLineW = (fromRational (eWhole-sWhole) * totalWidth)
        -- let lineH = 2
        -- let lgap = 3
        -- let rgap = 3
        -- let border = 3
        -- let half = levelHeight / 2
        -- let quarter = levelHeight / 4
        -- C.setSourceRGBA 0.6 0.6 0.6 1
        -- C.rectangle x y lineW levelHeight
        C.withLinearPattern xWhole 0 (wholeLineW + xWhole) 0 $ \pat -> do
            -- C.patternAddColorStopRGB pat 0 0 0 0
            -- C.patternAddColorStopRGB pat 0.5 1 1 1
            C.save
            C.patternAddColorStopRGBA pat 0 r g b 1
            C.patternAddColorStopRGBA pat 1 r g b 0.5
            C.patternSetFilter pat C.FilterFast
            C.setSource pat
            -- C.setSourceRGBA r g b 1
            -- C.arc (x+half) (y+half) (w/2) 0 (2 * pi)
            C.rectangle x y lineW levelHeight
            C.fill
            C.restore
            -- C.stroke
            -- C.fill
            -- C.stroke

renderGradientSVG :: String -> Pattern ColourD -> IO ()
renderGradientSVG name pat = do
    v C.withSVGSurface (name ++ ".svg")
        (totalWidth, levelHeight * (fromIntegral $ length $ levels pat)) $ levels pat
    return ()

renderGradientPDF :: String -> Pattern ColourD -> IO ()
renderGradientPDF name pat = do
    v C.withPDFSurface (name ++ ".pdf")
        (totalWidth, levelHeight * (fromIntegral $ length $ levels pat)) $ levels pat
    return ()

