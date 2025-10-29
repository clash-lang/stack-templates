{-|
Module      : Blink
Copyright   : Copyright © 2024 QBayLogic B.V.
License     : MIT
Maintainer  : QBayLogic B.V.
Stability   : experimental
Portability : POSIX

Blinking RGB led.
-}
module Blink where

import Clash.Annotations.TH
import Clash.Prelude

import Orangecrab.Domain
import RGB (RGB, red, green, blue, driveRGB)

topEntity ::
  -- | Orangecrab clock pin
  "CLK" ::: Clock Dom48 ->
  -- | Builtin orangecrab button
  "BTN" ::: Reset Dom48 ->
  -- | Orangecrab pin L4, as defined in the `orangecrab.pcf` file
  "PMOD3_0" ::: Signal Dom48 Bool ->
  -- | Builtin orangecrab LED
  "rgb_led0" ::: Signal Dom48 RGB
topEntity clk rst btn = withClockResetEnable clk rst enableGen (blink btn)


blink ::
  -- | Constraint hiding the Clock, Reset, and Enable signals
  HiddenClockResetEnable dom =>
  -- | Whether to reset counter
  Signal dom Bool ->
  -- | Output color for LED (as RGB)
  Signal dom RGB
blink btn = driveRGB (mealy blinkStep initState btn)
 where
  initState = (0 :: Unsigned 32, 0 :: Index 3)

  blinkStep (counter, colorIndex) _restart =
    ( ( counter + 1
      , if counter == 0 then satSucc SatWrap colorIndex else colorIndex
      )
    , blinkColors !! colorIndex
    )

  blinkColors = red :> green :> blue :> Nil


-- | A simple accumulator that works on unsigned numbers of any size.
-- It has hidden clock, reset, and enable signals.
accum ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Signal dom (Unsigned n) ->
  Signal dom (Unsigned n)
accum = mealy accumT 0
 where
  accumT s i = (s + i, s)

makeTopEntity 'topEntity
