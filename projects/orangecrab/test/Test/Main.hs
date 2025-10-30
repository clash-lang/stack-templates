import Prelude

import Test.Tasty

import qualified Test.Blink

main :: IO ()
main = defaultMain $ testGroup "."
  [ Test.Blink.blinkTests
  ]
