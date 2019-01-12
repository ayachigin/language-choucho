module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-iexample/src", "src/Language/Choucho/Parser.hs"]

