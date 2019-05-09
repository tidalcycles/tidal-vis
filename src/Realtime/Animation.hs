module Realtime.Animation
       ( movingPatterns
       ) where

import Control.Concurrent
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (<|))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Realtime.Types (ColorI)

import qualified Data.Sequence as S


window :: Display
window = InWindow "Nice Window" (500, 500) (20, 20)

background :: Color
background = greyN 0.1

movingPatterns :: MVar [ColorI] -> IO ()
movingPatterns tp = simulateIO window background 12
  (S.singleton [(200,100,200,250)])
  (pure . pictures . seqToPics)
  $ \_ _ seqColors -> do
    mColors <- tryTakeMVar tp
    let colsNew = fromMaybe [] mColors
    let headColors = seqColors `S.index` 0
    pure $ if headColors==colsNew || null colsNew then seqColors else addColorList colsNew seqColors
  where
    seqToPics :: Seq [ColorI] -> [Picture]
    seqToPics = S.foldMapWithIndex (\i c -> makeLine (length c) i c)

    makeLine :: Int -> Int -> [ColorI] -> [Picture]
    makeLine cLength i = map (\(n,col) -> rectLinesDown col n cLength i) . zip [0..]
    -- Keep circle list length equal to 'n'.
    refrain :: Int -> Seq [ColorI] -> Seq [ColorI]
    refrain n xs
      | S.length xs <= n = xs
      | otherwise        = S.take n xs
    -- Every round number spawn circle and add it to right end. Colorize new circle with new color.
    addColorList :: [ColorI] -> Seq [ColorI] -> Seq [ColorI]
    addColorList colors seqColors = colors <| refrain 10 seqColors

    rectLinesDown :: ColorI -> Float -> Int -> Int -> Picture
    rectLinesDown col n l i
        = translate (piece * n - 250 + piece / 2) (225 - 50 * fromIntegral i)
        $ color (makeColorFromIntTuple col)
        $ rectangleSolid piece 50
      where
        piece = 500 / fromIntegral l

makeColorFromIntTuple :: (Int, Int, Int, Int) -> Color
makeColorFromIntTuple (r,g,b,a) = makeColorI r g b a



