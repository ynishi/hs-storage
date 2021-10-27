module Main where

import           Lib
import           Opt

main :: IO ()
main = runApp =<< parse
