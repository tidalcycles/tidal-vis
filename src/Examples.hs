module Examples where

import Data.Colour
import Sound.Tidal.Context

import Common (dirtToColour)
import Vis
import VisCycle
import VisGradient


-- | Examples how to render still images to PDF or SVG formats.
--
-- | Here is renders of still images only.
main :: IO ()
main = do
  renderMatBundlePDF "./examples/" [a, b, c, d, e, f, g]
  return ()

-- | Make mat rectangle pattern
matRect :: IO ()
matRect = renderMatPDF "./examples/matRect" pip

-- | Make bundle of mat rectangle pattern
matBundleRect :: IO ()
matBundleRect = renderMatBundlePDF "./examples/" [foo, pip, pop, bar, buz]

-- | Make gradient rectangle pattern
gradientRect :: IO ()
gradientRect = renderGradientPDF "./examples/gradientRect" pip

-- | Make gradient rectangle pattern
matCycleWithBorders :: IO ()
matCycleWithBorders = renderCyclePDF "./examples/cycle" "background text" pip

repeater :: Pattern ColourD
repeater = dirtToColour
    $ juxBy 0.6 brak
    $ every 2 ((* speed (1 + sine)) . ply 4)
    $ stack
        [ s "bd:4 ~ ~ drum:3 ~ ~ drum:2 ~"
        , s "~ wind:1/2 hh:9"
        , s "subroc3d:9(2,7)"
        ]
    # speed 0.5
    # legato 1

-- | Prepared patterns.
foo :: Pattern ColourD
foo = dirtToColour $ striate 16 $ sound "[bd*3? dr2, ~ casio ~, [bd arpy]]" # n
  "2? 3 1 2"

pip :: Pattern ColourD
pip = dirtToColour $ fast 12 $ sound
  "[bd bd bd, <[sd sd] cp>, <arpy [arpy <[arpy arpy]> arpy arpy]>, odx]"

pop :: Pattern ColourD
pop = dirtToColour $ fast 12 $ loopAt 3 $ sound
  "[~ bd bd ~] ~ [bd ~ ~ [sd ~ ~ sd] ~ ~ sd]"

bar :: Pattern ColourD
bar = dirtToColour $ fast 12 $ sound "{~ ~ ~ ~, arpy bass2 drum notes can}"

buz :: Pattern ColourD
buz =
  dirtToColour $ fast 24 $ sound "arpy*4" # pan (range 0.25 0.75 sine) # gain
    (range 1.2 0.5 sine)

a :: Pattern ColourD
a = density 16 $ every 2 rev $ every 3 (superimpose (iter 4)) $ rev
  "[black blue darkblue, grey lightblue]"

b :: Pattern (Colour Double)
b = flip darken <$> "[black blue orange, red green]*16" <*> sine

c :: Pattern (Colour Double)
c =
  density 10
    $   flip darken
    <$> "[black blue, grey ~ navy, cornflowerblue blue]*2"
    <*> (slow 5 $ (*) <$> sine <*> (slow 2 tri))

d :: Pattern (Colour Double)
d =
  every 2 rev
    $ density 10
    $ (   blend'
      <$> "blue navy"
      <*> "orange [red, orange, purple]"
      <*> (slow 6 $ sine)
      )
  where blend' x y z = blend z x y

e :: Pattern (Colour Double)
e =
  density 32
    $   flip over
    <$> "[grey olive, black ~ brown, darkgrey]"
    <*> (   withOpacity
        <$> "[beige, lightblue white darkgreen, beige]"
        <*> ((*) <$> (slow 8 $ slow 4 sine) <*> (slow 3 $ sine))
        )

f :: Pattern ColourD
f =
  density 2
    $   flip darken
    <$> (density 8 $ "[black blue, grey ~ navy, cornflowerblue blue]*2")
    <*> sine

g :: Pattern ColourD
g = density 2 $ do
  let x = "[skyblue olive, grey ~ navy, cornflowerblue green]"
  coloura <- density 8 x
  colourb <- density 4 x
  slide'  <- slow 2 sine
  return $ blend slide' coloura colourb
