{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Project (topEntity, test) where

import Clash.Prelude
import Data.Function ((&))
import qualified Data.List as L

freq :: Unsigned 5
freq = 3

initState = True

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool ->
  Signal System Bool
topEntity = exposeClockResetEnable (freqDiv freq initState)

freqDiv initAcc initOut =
  moore
    ( \(acc, st) clk ->
        let acc' = (acc + 1) `mod` initAcc
            st' = if clk && acc == 0 then not st else st
         in (acc', st')
    )
    snd
    (initAcc, initOut)

test =
  print
    ( L.cycle [True, False]
        & simulate @System (freqDiv freq initState)
        & L.take 24
    )
