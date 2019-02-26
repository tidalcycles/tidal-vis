module Vis
       ( magicallyMakeEverythingFaster
       , renderMatPDF
       , renderMatSVG
       , renderMatBundlePDF
       , renderMatBundleSVG
       , svgAsString
       , vPDF
       , vSVG
       ) where

import Data.Colour.SRGB
import Sound.Tidal.Context hiding (segment)

import Common

import qualified Graphics.Rendering.Cairo as C

-- | Render PDF.
vPDF
  :: FilePath -- ^ path/filename without extansion.
  -> (Double, Double) -- ^ Image size.
  -> Pattern ColourD -- ^ Pattern. See 'Examples.hs' for pattern examples.
  -> IO ()
vPDF = v C.withPDFSurface

vSVG :: FilePath -> (Double, Double) -> Pattern ColourD -> IO ()
vSVG = v C.withSVGSurface

-- | Render bundle of patterns to PDF.
renderMatBundlePDF :: FilePath -> [Pattern ColourD] -> IO ()
renderMatBundlePDF path xs = mapM_ (\(num, p)
    -> vPDF (concat [path, "patternP_", show num, ".pdf"]) (1600,400) p)
    $ zip [(0::Int)..] xs

-- | Render bundle of patterns to SVG.
renderMatBundleSVG :: FilePath -> [Pattern ColourD] -> IO ()
renderMatBundleSVG path xs = mapM_ (\(num, p)
    -> vSVG (concat [path, "patternS_", show num, ".svg"]) (1600,400) p)
    $ zip [(0::Int)..] xs

-- | First argument is order number for name.
renderMatPDF :: String -> Pattern ColourD -> IO ()
renderMatPDF name = vPDF (concat [name, ".pdf"]) (1600, 400)

-- | First argument is order number for name.
renderMatSVG :: String -> Pattern ColourD -> IO ()
renderMatSVG name = vSVG (concat [name, ".svg"]) (1600, 400)

-- | Show svg code of pattern.
svgAsString :: Pattern ColourD -> IO String
svgAsString pat = do
  renderMatSVG "/tmp/vis2-tmp" pat
  readFile "/tmp/vis2-tmp.svg"

magicallyMakeEverythingFaster :: Pattern a -> [Event a]
magicallyMakeEverythingFaster = splitArcs 16
  where
    splitArcs num p = concatMap
        (\i -> queryArc p $ Arc i $ i+(1/num)) [0, (1/num) .. (1-(1/num))]

-- | Constant.
ticks :: Ratio Integer
ticks = 1

v :: (FilePath -> Double -> Double -> (C.Surface -> IO ()) -> IO ())
  -> FilePath
  -> (Double, Double) -- ^ Image output size.
  -> Pattern ColourD
  -> IO ()
v sf fn (x,y) pat =
  sf fn x y $ \surf ->
    C.renderWith surf $ do
      C.save
      C.scale x y
      C.setOperator C.OperatorOver
      C.setSourceRGB 0 0 0
      C.rectangle 0 0 1 1
      C.fill
      mapM_ renderEvent (events pat)
      C.restore

-- | Convert time and color to rendered type.
renderEvent :: Event [ColourD] -> C.Render ()
renderEvent (Event _ Arc{..} value) = do
    C.save
    drawBlocks value 0
    C.restore
  where
    height = 1 / fromIntegral (length value)
    drawBlocks :: [ColourD] -> Integer -> C.Render ()
    drawBlocks [] _ = return ()
    drawBlocks (c:cs) num = do
        let (RGB r g b) = toSRGB c
        let x = fromRational start
        let y = fromIntegral num * height
        let w = fromRational (stop - start)
        let h = height
        C.setSourceRGBA r g b 1
        C.rectangle x y w h
        C.fill
        C.stroke
        drawBlocks cs (num + 1)

events :: Pattern ColourD -> [Event [ColourD]]
events pat = map
    ( \(Event whole Arc{..} value)
      -> Event whole (Arc ((start - tick) / speed') ((stop - tick) / speed')) value
    )
    $ queryArc (segmentator pat) (Arc tick (tick + speed'))
  where
    speed' :: Ratio Integer
    speed' = 1
    tick :: Ratio Integer
    tick = ticks / 2


