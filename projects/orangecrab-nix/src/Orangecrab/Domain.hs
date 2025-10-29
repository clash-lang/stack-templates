{-|
Module      : Domain
Copyright   : Copyright © 2024 QBayLogic B.V.
License     : MIT
Maintainer  : QBayLogic B.V.
Stability   : experimental
Portability : POSIX

OrangeCrab / Lattice ECP5-85F specific clock domains.
-}

{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Orangecrab.Domain where

import Clash.Prelude

-- | 48 MHz oscillator clock of the OrangeCrab board.
createDomain vSystem
  { vName = "Dom48"
  , vResetPolarity = ActiveLow
  , vPeriod = hzToPeriod 48_000_000
  }
