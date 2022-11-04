# tidal-vis

Tidal is a domain specific language for live coding pattern. This package allows several things:

1. OSC messages sent to SC to be dynamicly rendered in realtime with at separate window.
[Demo of realtime visualisation.](https://youtu.be/bZS6WufE8FY)
2. Colour patterns to be rendered as PDF or SVG files. See _Examples.hs_ module for more help.
3. Colour patterns to be rendered to be rendered dynamicly in separate window. See _CycleAnimation.hs_ for more. [Demo.](https://youtu.be/cCmCSSb4vHs)

## (1) Realtime animation during livecoding

1. Comment out any existing lines in *BootTidal.hs* that begin with `tidal <- startTidal`.

2. Add the following lines to *BootTidal.hs*:

```haskell
 -- Target and shape for pattern visualizing.
 patternTarget = Target { oName = "Pattern handler", oAddress = "127.0.0.1", oPort = 5050, oBusPort = Nothing, oLatency = 0.02, oWindow = Nothing, oSchedule = Pre BundleStamp, oHandshake = False }
 patternShape = OSC "/trigger/something" $ Named {requiredArgs = []}

 -- Target for playing music via SuperCollider.
 musicTarget = superdirtTarget { oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120 }

 config = defaultConfig {cFrameTimespan = 1/20}

 -- Send pattern as OSC both to SuperCollider and to tidal-vis.
 tidal <- startStream config [(musicTarget, [superdirtShape]), (patternTarget, [patternShape])]

 -- Send pattern as OSC to SuperCollider only.
 -- tidal <- startTidal musicTarget config
```

3. Install **tidal-vis** and run:

```bash
 cabal update
 cabal install tidal-vis
 tidal-vis
```

4. Eval your **Tidal** code.

5. Profit.

## (2) Render SVG or PDF

For exanple, when pattern is

    density 16 $ every 2 rev $ every 3 (superimpose (iter 4)) $ rev "[black blue darkblue, grey lightblue]"

Output image is

![0](https://i.imgur.com/MPbpH0n.jpg)

## (3) Animate one pattern

To animate pattern (not good performance):

    cd ./tidal-vis/
    stack repl ./src/CycleAnimation.hs
    :set -XOverloadedStrings
    ah <- run
    swapMVar ah $ degradeBy 0.3 $ every 3 (fast 3) $  Params.s "[red, white, [purple orange green]]"

Look at _CycleAnimation.hs_ for more information. Look at `looping` function to change animation form.

## Tutorial

[`tidal-vis` installation tutorial](https://www.kindohm.com/posts/2016/2016-09-02-tidal-vis/) by [Kindohm](http://github.com/kindohm)

## Hackage

https://hackage.haskell.org/package/tidal-vis

## Misc

For more information: [tidalcycles](http://tidalcycles.org)
