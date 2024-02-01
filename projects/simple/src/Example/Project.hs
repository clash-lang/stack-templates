-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module Example.Project where

import Clash.Prelude

-- Create a domain with the frequency of your input clock. For this example we used
-- 50 MHz.
createDomain vSystem{vName="MyDomain", vPeriod=hzToPeriod 50e6}

-- | 'topEntity' is Clash's equivalent of 'main' in other programming languages.
-- Clash will look for it when compiling 'Example.Project' and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a 'topEntity'
-- must be monomorphic and must use non- recursive types. Or, to put it
-- hand-wavily, a 'topEntity' must be translatable to a static number of wires.
--
-- The inputs need to be declared as parameters in order to be able to give them
-- custom names. In other words, @topEntity = exposeClockResetEnable acc@ would
-- prevent naming the inputs.
topEntity ::
  Clock MyDomain ->
  Reset MyDomain ->
  Enable MyDomain ->
  Signal MyDomain (Unsigned 8) ->
  Signal MyDomain (Unsigned 8)
topEntity clk rst en inp = (exposeClockResetEnable accum) clk rst en inp
-- Give names to the top entity parts
{-# ANN topEntity
  (Synthesize
    { t_name = "accum"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}

-- | A simple accumulator: it accumulates the inputs, resets to 0, and outputs
-- the current state
accum ::
  forall dom .
  HiddenClockResetEnable dom =>
  Signal dom (Unsigned 8) ->
  Signal dom (Unsigned 8)
accum inp = mux (fmap (== maxBound) count) 0 $ mealy accumT 0 inp
 where
  count :: Signal dom (Index 5)
  count = register 0 $ satSucc SatBound <$> count
  accumT s i = (s + i, s)
