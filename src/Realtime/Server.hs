module Realtime.Server
       ( animeCollectorServerU
       ) where

import Control.Concurrent
import Control.Concurrent.Async (race_)
import Control.Concurrent.Chan.Unagi.Bounded (InChan, OutChan)
import Control.Monad
import Sound.OSC

import qualified Control.Concurrent.Chan.Unagi.Bounded as U
import qualified Sound.OSC.FD as FD

import Realtime.Animation (movingPatterns)
import Realtime.Types (ColorI, TidalPacket (..), packetToTidalPacket)


-- Command to start the server in a repl for testing
-- do u <- t0; udp_close u; hoscServerTPU

animeCollectorServerU :: IO ()
animeCollectorServerU = do
    (inChan, outChan) <- U.newChan 100
    mvar <- newEmptyMVar
    race_ (hoscServerTPU inChan) $ race_ (collector outChan mvar) (movingPatterns mvar)

t0 :: IO UDP
t0 = udpServer "127.0.0.1" 5050

-- Listen to osc packets and write them to channel.
hoscServerTPU :: InChan TidalPacket -> IO ()
hoscServerTPU inChan = FD.withTransport t0 $ \udp -> forever $ do
    packet <- udp_recv_packet udp
    let tp = packetToTidalPacket packet
    U.writeChan inChan tp

-- Collect sync packets to list and put mvar for animation.
collector :: OutChan TidalPacket -> MVar [ColorI] -> IO ()
collector outChan mvColors = do
    buffer <- newEmptyMVar
    forever $ do
        c <- U.readChan outChan
        mtp <- tryTakeMVar buffer
        case mtp of
            Nothing -> putMVar buffer (tpTime c, [tpColor c])
            Just tp ->
                if fst tp == tpTime c
                then void $ putMVar buffer (toTuple c tp)
                else do
                    putMVar buffer (tpTime c, [tpColor c])
                    putMVar mvColors $ snd tp

-- Take time and color.
toTuple :: TidalPacket -> (Double, [ColorI]) -> (Double, [ColorI])
toTuple tp (f,tps) = (f, tpColor tp : tps)



