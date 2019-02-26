module VisCycle
       ( renderCyclePDF
       , renderCycleSVG
       ) where

import Data.Colour.SRGB
import Sound.Tidal.Context
import Sound.Tidal.Utils

import Common

import qualified Graphics.Rendering.Cairo as C


-- | Constants.
totalWidth :: Double
totalWidth = 500

border :: Double
border = 5

v :: (String -> Double -> Double -> (C.Surface -> IO ()) -> IO ())
  -> String             -- ^ filePath
  -> (Double, Double)   -- ^ size
  -> [[Event ColourD]]
  -> String             -- ^ label
  -> IO ()
v sf fn (x,y) colorEvents label =
    sf fn x y $ \surf -> C.renderWith surf $ do
        C.setAntialias C.AntialiasBest
        C.save
        C.translate border border
        C.scale (totalWidth-(border*2)) (totalWidth-(border*2))
        C.setOperator C.OperatorOver
        C.selectFontFace ("Inconsolata" :: String) C.FontSlantNormal C.FontWeightNormal
        C.setFontSize 0.2
        (C.TextExtents _ _ _ textH _ _) <- C.textExtents (label :: String)
        C.moveTo 0 textH
        C.textPath (label :: String)
        C.setSourceRGB 0 0 0
        C.fill
        -- C.setSourceRGB 0 0 0
        -- C.rectangle 0 0 1 1
        -- C.fill
        mapM_ (renderLevel (length colorEvents)) $ enumerate colorEvents
        C.restore

renderLevel :: Int -> (Int, [Event ColourD]) -> C.Render ()
renderLevel total (num, level) = do
    C.save
    mapM_ drawEvent level
    C.restore
  where
    drawEvent :: Event ColourD -> C.Render ()
    drawEvent (Event _ Arc{..} c) = do
        let (RGB r g b) = toSRGB c
        let levelHeight = (1 / fi (total+1))/2
        let h = levelHeight * fi (num + 1)
        let hPi = pi / 2
        let dPi = pi * 2
        C.save
        C.setSourceRGBA r g b 1
        C.arc 0.5 0.5 (h+levelHeight) (fromRational start * dPi - hPi) (fromRational stop * dPi - hPi)
        C.arcNegative 0.5 0.5 h  (fromRational stop * dPi - hPi) (fromRational start * dPi - hPi)
        C.fill
        C.setSourceRGBA 0.5 0.5 0.5 1
        C.setLineWidth 0.005
        C.arc 0.5 0.5 (h+levelHeight) (fromRational start * dPi - hPi) (fromRational stop * dPi - hPi)
        C.arcNegative 0.5 0.5 h  (fromRational stop * dPi - hPi) (fromRational start * dPi - hPi)
        C.stroke
        C.restore

-- | Render a cycle pattern to pdf file.
renderCyclePDF
  :: String -- ^ File name (and path)
  -> String -- ^ Background text
  -> Pattern ColourD
  -> IO ()
renderCyclePDF name label pat = do
    v C.withPDFSurface (name ++ ".pdf") (totalWidth, totalWidth) (levels pat) label
    return ()

    -- | Render a cycle pattern to pdf file.
renderCycleSVG
  :: String -- ^ File name (and path)
  -> String -- ^ Background text
  -> Pattern ColourD
  -> IO ()
renderCycleSVG name label pat = do
    v C.withSVGSurface (name ++ ".svg") (totalWidth, totalWidth) (levels pat) label
    return ()
