module Main where

import Prelude
import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)

main :: IO ()
main = doctest (flags ++ pkgs ++ module_sources)
