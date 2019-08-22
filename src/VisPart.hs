module VisPart
       ( renderPartSVG
       , renderPartPDF
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

v :: Show a => (FilePath -> Double -> Double -> (C.Surface -> IO ()) -> IO ())
  -> FilePath
  -> (Double, Double)
  -> [[Event a]]
  -> IO ()
v sf fn (x,y) es = sf fn x y $ \surf ->
    C.renderWith surf $ do
        C.save
        -- C.scale x (y / (fromIntegral $ length colorEvents))
        C.setOperator C.OperatorOver
        -- C.setSourceRGB 0 0 0
        -- C.rectangle 0 0 1 1
        --C.fill
        C.setAntialias C.AntialiasBest
        mapM_ (renderLevel (length es)) $ enumerate es
        C.restore

renderLevel
  :: (Foldable t, Integral a, Show b)
  => p
  -> (a, t (Event b))
  -> C.Render ()
renderLevel _ (num, level) = do
    C.save
    mapM_ drawEvent $ level
    C.restore
  where
    drawEvent e@(Event _ (Arc sPart ePart) v) = do
        let (Arc sWhole eWhole) = wholeOrPart e
        let (r, g, b) = (0,0,0)
        let px = (fromRational sPart) * totalWidth
        let wx = (fromRational sWhole) * totalWidth
        let y = (fromIntegral num) * levelHeight
        let pw = (fromRational (ePart - sPart) * totalWidth)
        let ww = (fromRational (eWhole - sWhole) * totalWidth)
        let gap = 12
        let lw = 2
            halfLw = lw /2
            halfGap = gap / 2

        C.withLinearPattern wx 0 (ww + wx) 0 $ \pat -> do
            C.save
            C.patternAddColorStopRGBA pat 0 0.8 0.8 0.8 1
            C.patternAddColorStopRGBA pat 1 0 0 0 0.5
            C.patternSetFilter pat C.FilterFast
            C.setSource pat
            let leftGap = if px == wx then halfGap else 0
                rightGap = if px+pw == wx+ww then halfGap else 0
            C.rectangle (px+leftGap) (y+halfGap) ((pw-(leftGap+rightGap))) (levelHeight-gap)
            C.fill
            C.restore
            C.save
            C.setSourceRGBA 0 0 0 1
            C.setLineWidth lw
            C.moveTo (px+leftGap) (y+halfGap)
            C.lineTo (px+pw-(rightGap)) (y+halfGap)
            C.moveTo (px+leftGap) (y+levelHeight-halfGap)
            C.lineTo (px+pw-(rightGap)) (y+levelHeight-halfGap)
            C.stroke
            if px == wx
              then do C.moveTo (px+halfGap) (y+levelHeight-halfGap)
                      C.lineTo (px+halfGap) (y+halfGap)
                      C.stroke
              else (do C.setDash [6,4] 6
                       C.moveTo (px) (y+halfGap)
                       C.lineTo (wx+halfGap) (y+halfGap)
                       C.lineTo (wx+halfGap) (y+levelHeight-halfGap)
                       C.lineTo (px) (y+levelHeight-halfGap)
                       C.stroke
                       C.setDash [] 0
                       return ()
                   )
            if (px+pw) == (wx+ww)
              then do C.moveTo (px+pw-halfGap) (y+levelHeight-halfGap)
                      C.lineTo (px+pw-halfGap) (y+halfGap)
                      C.stroke
                      return ()
              else (do C.setDash [6,4] 0
                       C.moveTo (px+pw) (y+halfGap)
                       C.lineTo (wx+ww-halfGap) (y+halfGap)
                       C.lineTo (wx+ww-halfGap) (y+levelHeight-halfGap)
                       C.lineTo (px+pw) (y+levelHeight-halfGap)
                       C.stroke
                       C.setDash [] 0
                       return ()
                   )
            C.restore
            C.selectFontFace ("Inconsolata" :: String) C.FontSlantNormal C.FontWeightNormal
            C.setFontSize 35
            (C.TextExtents _ _ textW textH _ _) <- C.textExtents (show v)
            C.moveTo (wx + 12) (y + textH + 16)
            C.textPath (show v)
            C.setSourceRGB 0 0 0
            C.fill
--        C.save
--        C.translate border border
--        C.scale (totalWidth-(border*2)) (totalWidth-(border*2))
--        C.setOperator C.OperatorOver
            -- C.fill
            -- C.stroke

renderPartSVG :: Show a => String -> Pattern a -> IO ()
renderPartSVG name pat = do
    v C.withSVGSurface (name ++ ".svg")
        (totalWidth, levelHeight * (fromIntegral $ length $ levelsWhole pat)) $ levelsWhole pat
    return ()

renderPartPDF :: Show a => String -> Pattern a -> IO ()
renderPartPDF name pat = do
    v C.withPDFSurface (name ++ ".pdf")
        (totalWidth, levelHeight * (fromIntegral $ length $ levelsWhole pat)) $ levelsWhole pat
    return ()

