import Prelude
import System.Environment (getArgs)
import Clash.Main (defaultMain)

main :: IO ()
main = defaultMain =<< getArgs
