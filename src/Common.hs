{-# LANGUAGE NamedFieldPuns #-}

module Common
       ( arrangeEvents
       , beatNow
       , dirtToColour
       , fi
       , levels
       , levelsWhole
       , remoteLocal
       , segmentator
       , toPattern
       ) where

import Control.Concurrent.MVar
import Data.Bits (shiftR, (.&.))
import Data.Colour.SRGB (sRGB)
import Data.Function (on)
import Data.Hashable (hash)
import Data.List (groupBy, nub, sortOn)
import Data.Maybe (isJust)
import Data.Time (diffUTCTime, getCurrentTime)
import Network.Socket (SockAddr (..), addrAddress, getAddrInfo)
import Sound.Tidal.Context

import qualified Sound.OSC.FD as OSC
import qualified Sound.Tidal.Tempo as Tempo


-- | Common functions.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fitsWhole :: Event b -> [Event b] -> Bool
fitsWhole event events = not $ any (\event' -> isJust $ subArc (wholeOrPart event) (wholeOrPart event')) events

addEventWhole :: Event b -> [[Event b]] -> [[Event b]]
addEventWhole e [] = [[e]]
addEventWhole e (level:ls)
    | isAnalog e = level:ls
    | fitsWhole e level = (e:level) : ls
    | otherwise = level : addEventWhole e ls

arrangeEventsWhole :: [Event b] -> [[Event b]]
arrangeEventsWhole = foldr addEventWhole []

levelsWhole :: Pattern a -> [[Event a]]
levelsWhole pat = arrangeEventsWhole $ sortOn' ((\Arc{..} -> stop - start) . part) (queryArc pat (Arc 0 1))

fits :: Event b -> [Event b] -> Bool
fits (Event _ part' _) events = not $ any (\Event{..} -> isJust $ subArc part' part) events

addEvent :: Event b -> [[Event b]] -> [[Event b]]
addEvent e [] = [[e]]
addEvent e (level:ls)
    | fits e level = (e:level) : ls
    | otherwise = level : addEvent e ls

arrangeEvents :: [Event b] -> [[Event b]]
arrangeEvents = foldr addEvent []

levels :: Pattern a -> [[Event a]]
levels pat = arrangeEvents $ sortOn' ((\Arc{..} -> stop - start) . part) (queryArc pat (Arc 0 1))

sortOn' :: Ord a => (b -> a) -> [b] -> [b]
sortOn' f = map snd . sortOn fst . map (\x -> let y = f x in y `seq` (y, x))

-- | Recover deprecated functions for 1.0.13
dirtToColour :: ControlPattern -> Pattern ColourD
dirtToColour = fmap (stringToColour . show)

stringToColour :: String -> ColourD
stringToColour str = sRGB (r/256) (g/256) (b/256)
  where
    i = hash str `mod` 16777216
    r = fromIntegral $ (i .&. 0xFF0000) `shiftR` 16
    g = fromIntegral $ (i .&. 0x00FF00) `shiftR` 8
    b = fromIntegral (i .&. 0x0000FF)

segmentator :: Pattern ColourD -> Pattern [ColourD]
segmentator p@Pattern{..} = Pattern $ \(State arc@Arc{..} _)
    -> filter (\(Event _ (Arc start' stop') _) -> start' < stop && stop' > start)
    $ groupByTime (segment' (queryArc p arc))

segment' :: [Event a] -> [Event a]
segment' es = foldr split es pts
  where pts = nub $ points es

split :: Time -> [Event a] -> [Event a]
split _ [] = []
split t (ev@(Event whole Arc{..} value):es)
    | t > start && t < stop =
      Event whole (Arc start t) value : Event whole (Arc t stop) value : split t es
    | otherwise = ev:split t es

points :: [Event a] -> [Time]
points []                       = []
points (Event _ Arc{..} _ : es) = start : stop : points es

groupByTime :: [Event a] -> [Event [a]]
groupByTime es = map merge $ groupBy ((==) `on` part) $ sortOn (stop . part) es
  where
    merge :: [EventF a b] -> EventF a [b]
    merge evs@(Event{whole, part} : _) = Event whole part $ map (\Event{value} -> value) evs
    merge _                            = error "groupByTime"

beatNow :: Tempo.Tempo -> IO Double
beatNow t = do
    now <- getCurrentTime
    at <- case OSC.iso_8601_to_utctime $ OSC.time_pp $ Tempo.atTime t of
        Nothing  -> pure now
        Just at' -> pure at'
    let delta = realToFrac $ diffUTCTime now at
    let beatDelta = Tempo.cps t * delta
    return $ Tempo.nudged t + beatDelta

remoteLocal :: Config -> OSC.Time -> IO (MVar Tempo.Tempo)
remoteLocal config time = do
  let tempoClientPort = cTempoClientPort config
      hostname = cTempoAddr config
      remotePort = cTempoPort config
  (remote_addr:_) <- getAddrInfo Nothing (Just hostname) Nothing
  local <- OSC.udpServer "127.0.0.1" tempoClientPort
  case addrAddress remote_addr of
    SockAddrInet _ a -> do
      let remote = SockAddrInet (fromIntegral remotePort) a
      newMVar $ Tempo.defaultTempo time local remote
    _ -> error "wrong Socket"

toPattern :: [Event ControlMap] -> ControlPattern
toPattern evs = Pattern $ const evs

