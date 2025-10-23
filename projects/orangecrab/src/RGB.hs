{-|
Module      : RGB
Copyright   : Copyright © 2024 QBayLogic B.V.
License     : MIT
Maintainer  : QBayLogic B.V.
Stability   : experimental
Portability : POSIX

RGB led color mixing.
-}
{-# LANGUAGE RecordWildCards #-}
module RGB where

import Clash.Prelude

-- | Color values.
data Color =
  Color
    { r :: Unsigned 8
    , g :: Unsigned 8
    , b :: Unsigned 8
    }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

black, white, red, green, blue, yellow, orange, cyan, violet :: Color
black  = Color   0   0   0
white  = Color 255 255 255
red    = Color 255   0   0
green  = Color   0 255   0
blue   = Color   0   0 255
yellow = Color 255 255   0
orange = Color 255 165   0
cyan   = Color   0 255 255
violet = Color 190  70 242

-- | RBG led interface
data RGB =
  RGB
    { rLed :: "r" ::: Bool
    , gLed :: "g" ::: Bool
    , bLed :: "b" ::: Bool
    }
  deriving (Generic, NFDataX, BitPack)

-- | Mixes color to an RGB led via pulse width modulation.
driveRGB ::
  (KnownDomain dom, HiddenClockResetEnable dom) =>
  Signal dom Color ->
  Signal dom RGB
driveRGB = mealy (~~>) 0
 where
  c ~~> Color{..} =
    ( satSucc SatWrap c
    , RGB (c >= r) (c >= g) (c >= b)
    )
