module Tests.Example.Project where

import Clash.Prelude
import Clash.Prelude.Testbench

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector

import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range

-- Import the module containing the `accum` function
import Example.Project (accum)

-- Define a Hedgehog property to test the `accum` function
prop_accum :: HH.Property
prop_accum = HH.property $ do
  -- Generate vector of random unsigned numbers of at least length 1.
  someVec <- HH.forAll $ genSomeVec @_ @1 (Range.linear 0 100)
    (genUnsigned Range.linearBounded)
  -- Check if the output matches the expected result
  case someVec of
    SomeVec SNat vec -> do
      let
        tb :: HiddenClockResetEnable System => Signal System Bool
        tb = outputVerifier' expected actual
         where
          expected = scanl (\acc x -> acc + fromIntegral x) 0 vec
          actual = accum @System (stimuliGenerator vec)
      HH.assert $ or $ withClockResetEnable clockGen resetGen enableGen tb

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
