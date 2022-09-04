{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.BitOfByte (topEntity) where

import Clash.Prelude

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Unsigned 3, Unsigned 8) ->
  Signal System Bool
topEntity = exposeClockResetEnable bitOfByte

bitAt :: Unsigned 3 -> Unsigned 8 -> Bool
bitAt 0 $(bitPattern "1.......") = True
bitAt 1 $(bitPattern ".1......") = True
bitAt 2 $(bitPattern "..1.....") = True
bitAt 3 $(bitPattern "...1....") = True
bitAt 4 $(bitPattern "....1...") = True
bitAt 5 $(bitPattern ".....1..") = True
bitAt 6 $(bitPattern "......1.") = True
bitAt 7 $(bitPattern ".......1") = True
bitAt _ _ = False

bitOfByte :: (HiddenClockResetEnable dom) => Signal dom (Unsigned 3, Unsigned 8) -> Signal dom Bool
bitOfByte =
  moore
    (\_ inp -> inp)
    (uncurry bitAt)
    (0, 0)
