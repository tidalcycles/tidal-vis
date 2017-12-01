module Sound.Tidal.VisCycle where

import qualified Graphics.Rendering.Cairo as C 
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Control.Applicative
import Sound.Tidal.Parse
import Sound.Tidal.Pattern
import Sound.Tidal.Time
import Sound.Tidal.Utils
import Data.Ratio
import Data.Maybe
import System.Cmd
import Data.List
import Data.Ord         ( comparing )

totalWidth = 50 :: Double
border = 5
ratio = 1

arrangeEvents [] = []
arrangeEvents (e:es) = addEvent e (arrangeEvents es)
fits e es = null $ filter (id) $ map (\e' -> isJust $ subArc (snd' e) (snd' e')) es
addEvent e [] = [[e]]
addEvent e (level:levels) | fits e level = (e:level):levels
                          | otherwise = level:(addEvent e levels)

v sf fn (x,y) levels label =
      sf fn x y $ \surf -> do
        C.renderWith surf $ do
          C.setAntialias C.AntialiasBest
          C.save
          C.translate border border
          C.scale (totalWidth-(border*2)) (totalWidth-(border*2))
          C.setOperator C.OperatorOver
          C.selectFontFace "Inconsolata" C.FontSlantNormal C.FontWeightNormal
          C.setFontSize 0.2
          (C.TextExtents _ _ textW textH _ _) <- C.textExtents (label :: String)
          C.moveTo (0) (textH)
          C.textPath (label :: String)
          C.setSourceRGB 0 0 0
          C.fill
          -- C.setSourceRGB 0 0 0
          -- C.rectangle 0 0 1 1
          --C.fill
          mapM_ (renderLevel (length levels)) $ enumerate levels
          C.restore

renderLevel total (n, level) = do C.save
                                  mapM_ drawEvent $ level
                                  C.restore
      where drawEvent ((sWhole, eWhole), (s,e), c) = 
              do let (RGB r g b) = toSRGB c
                 C.save
                 C.setSourceRGBA r g b 1
                 C.arc 0.5 0.5 (h+levelHeight) ((fromRational s)*(pi*2)-(pi/2)) ((fromRational e)*(pi*2)-(pi/2))
                 C.arcNegative 0.5 0.5 h  ((fromRational e)*(pi*2)-(pi/2)) ((fromRational s)*(pi*2)-(pi/2))
                 C.fill
                 C.setSourceRGBA 0.5 0.5 0.5 1
                 C.setLineWidth 0.005
                 C.arc 0.5 0.5 (h+levelHeight) ((fromRational s)*(pi*2)-(pi/2)) ((fromRational e)*(pi*2)-(pi/2))
                 C.arcNegative 0.5 0.5 h  ((fromRational e)*(pi*2)-(pi/2)) ((fromRational s)*(pi*2)-(pi/2))
                 C.stroke
                 C.restore
                   where h = levelHeight * (fromIntegral (n + 1))
                         levelHeight = (1 / fromIntegral (total+1))/2
            vPDF = v C.withPDFSurface

visCycle :: [Char] -> String -> Pattern ColourD -> IO ()
visCycle name label pat =
  do v (C.withPDFSurface) (name ++ ".pdf") (totalWidth, totalWidth) levels label
     return ()
       where levels = arrangeEvents $ sortOn ((\x -> snd x - fst x) . snd') (arc pat (0,1))
             sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

visAsString pat = do visCycle "/tmp/vis2-tmp" "" pat
                     svg <- readFile "/tmp/vis2-tmp.svg"
                     return svg


magicallyMakeEverythingFaster = splitArcs 16
  where splitArcs n p = concatMap (\i -> arc p (i,i+(1/n))) [0, (1/n) .. (1-(1/n))]
