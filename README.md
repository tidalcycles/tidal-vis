# tidal-vis

Tidal is a domain specific language for live coding pattern. This package allows colour patterns to be rendered as PDF or SVG files. See _Examples.hs_ module for more help.

## Example

    density 16 $ every 2 rev $ every 3 (superimpose (iter 4)) $ rev "[black blue darkblue, grey lightblue]"

![0](https://i.imgur.com/MPbpH0n.jpg)

To run pattern [animation](https://youtu.be/cCmCSSb4vHs) (not good performance):

    cd ./tidal-vis/
    stack repl ./src/CycleAnimation.hs
    :set -XOverloadedStrings
    ah <- run
    swapMVar ah $ degradeBy 0.3 $ every 3 (fast 3) $  Params.s "[red, white, [purple orange green]]"

Look at _CycleAnimation.hs_ for more information.

## Tutorial

[`tidal-vis` installation tutorial](http://blog.kindohm.com/2016/09/02/tidal-vis) by [Kindohm](http://github.com/kindohm)

## Hackage

https://hackage.haskell.org/package/tidal-vis

## Misc

For more information: [tidalcycles](http://tidalcycles.org)
