module Realtime.Types
       ( TidalPacket (..)
       , ColorI
       , defaultTidalPacket
       , packetToTidalPacket
       , parsePacket
       ) where

import Data.Bits (shiftR, (.&.))
import Data.Hashable (hash)
import Data.Maybe (fromMaybe)
import Sound.OSC


data TidalPacket = TidalPacket
    { tpTime  :: Double
    , tpCycle :: Float
    , tpDelta :: Float
    , tpColor :: ColorI
    } deriving (Eq, Show)

type ColorI = (Int, Int, Int, Int)

defaultTidalPacket :: TidalPacket
defaultTidalPacket = TidalPacket
    { tpTime = immediately
    , tpCycle = 1.0
    , tpDelta = 1.0
    , tpColor = (100, 200, 50, 250)
    }

parsePacket :: Packet -> Maybe (Int,Int,Int,Int)
parsePacket p = tupleI list
  where
    list = mapM datum_integral . messageDatum =<< packet_to_message p
    tupleI = \case
        Nothing -> Nothing
        Just list' -> case list' of
            (r:g:b:a:_) -> Just (r,g,b,a)
            _           -> Nothing

stringToColour :: String -> (Int,Int,Int,Int)
stringToColour str = (r, g, b, 250)
  where
    i = hash str `mod` 16777216
    r = (i .&. 0xFF0000) `shiftR` 16
    g = (i .&. 0x00FF00) `shiftR` 8
    b = i .&. 0x0000FF

deleteDatumValue :: String -> [Datum] -> [Datum]
deleteDatumValue d ds = go
  where
    go = case break (==d') ds of
        (f,x:_:xs) -> f ++ (x:xs)
        _          -> []
    d' = string d

roundFloats :: Datum -> Datum
roundFloats = \case
    Float d_float -> Float (fromInteger (round $ d_float * 10000) / 10000)
    x -> x

takeDatumValue :: String -> [Datum] -> Datum
takeDatumValue d ds = go
  where
    go = case break (== d') ds of
      (_,_:v:_) -> v
      _         -> string "No value for your datum"
    d' = string d

packetToTidalPacket :: Packet -> TidalPacket
packetToTidalPacket p = TidalPacket
    { tpTime = bundleTime bund
    , tpCycle = cycle'
    , tpDelta = delta'
    , tpColor = color'
    }
  where
    bund = packet_to_bundle p
    datums = concatMap messageDatum $ bundleMessages bund
    cycle' = takeFloat "cycle" datums
    delta' = takeFloat "delta" datums
    color' = stringToColour $ show $ deleteDatumValue "cycle" datums
    takeFloat str = fromMaybe 0 . datum_floating . roundFloats . takeDatumValue str
