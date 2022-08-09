module Main where

import Lib
import Data.Array.ST

main :: IO ()
main = undefined

testSelectionSort = runSTArray $ do
  s <- newArray (1,8) (10 ::Int)
  writeArray s 1 6
  writeArray s 2 5
  writeArray s 3 3
  writeArray s 4 1
  writeArray s 5 8
  writeArray s 6 7
  writeArray s 7 2
  writeArray s 8 4
  o <- selectionSort s
  return o
