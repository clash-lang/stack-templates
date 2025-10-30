module Test.Blink where

import Prelude

import Clash.Hedgehog.Sized.Unsigned
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.List as List

-- Import the module containing the @accum@ function
import Blink (blink)
import RGB (RGB(..))


-- Test the property that blink at most drives one of the (r, g, b) leds within
-- the orangecrab multicolor led.
prop_blink :: H.Property
prop_blink = H.property $ do
  numTestCycles <- H.forAll (Gen.integral (Range.linear 3 500))

  let
    input :: [Bool]
    input = List.repeat False

    numberRgbProperty :: RGB -> Bool
    numberRgbProperty (RGB r g b) = 1 >= (fromEnum r + fromEnum g + fromEnum b)

    output :: [RGB]
    output = C.sample @C.System (blink (C.fromList input))

    -- Drop the 1st clock cycle, which corresponds to RESET, and then take
    -- the next `numTestCycles` cycles to test (so that we don't try to
    -- compare two infinite lists, causing the test to hang).
    outputFinite :: [RGB]
    outputFinite = List.take numTestCycles (List.drop 1 output)

    propertyHolds :: [Bool]
    propertyHolds = List.map numberRgbProperty outputFinite

  propertyHolds H.=== List.replicate numTestCycles True


blinkTests :: TestTree
blinkTests = $(testGroupGenerator)


main :: IO ()
main = defaultMain blinkTests
