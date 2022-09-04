{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example.Project (Example.Project.topEntity) where

import Clash.Prelude
import Example.MMul (Mat64, topEntity)

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Mat64, Mat64) ->
  Signal System Mat64
topEntity = Example.MMul.topEntity
