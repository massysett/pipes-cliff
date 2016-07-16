module Main where

import Pipes.Cliff.Examples

main :: IO ()
main = limitedAlphaNumbers >>= print
