module Main where

import Pipes.Cliff.Examples
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = standardOutputAndError >>= BS8.putStr
