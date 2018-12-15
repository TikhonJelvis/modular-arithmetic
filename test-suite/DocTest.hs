module Main (main) where

import           Test.DocTest         (doctest)

main :: IO ()
main = doctest ["src/Data/Modular.hs"]
