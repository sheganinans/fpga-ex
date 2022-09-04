{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.MMul (Mat64, topEntity, test) where

import Clash.Prelude
import Data.Function ((&))
import qualified Data.List as L

type Vec8 = Vec 8 Int

type Mat64 = Vec 8 Vec8

initRow :: Vec8
initRow = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> Nil

initState :: Mat64
initState =
  initRow
    :> initRow
    :> initRow
    :> initRow
    :> initRow
    :> initRow
    :> initRow
    :> initRow
    :> Nil

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Mat64, Mat64) ->
  Signal System Mat64
topEntity = exposeClockResetEnable mmul

mmul ::
  HiddenClockResetEnable dom =>
  Signal dom (Mat64, Mat64) ->
  Signal dom Mat64
mmul =
  moore
    (\_ (a, b) -> mmult a b)
    id
    initState

mmult ::
  -- from Clash examples
  na ~ mb =>
  1 <= mb =>
  KnownNat mb =>
  KnownNat nb =>
  Vec ma (Vec na Int) ->
  Vec mb (Vec nb Int) ->
  Vec ma (Vec nb Int)
mmult mA mB = result
  where
    mBT = transpose mB
    dot a b = sum $ zipWith (*) a b
    result = map (\ar -> dot ar <$> mBT) mA

test =
  print
    ( L.repeat (initState, initState)
        & simulate @System mmul
        & L.take 24
    )
