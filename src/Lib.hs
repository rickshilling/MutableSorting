module Lib
    (
      selectionSort
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

selectionSort :: STArray s Int Int -> ST s (STArray s Int Int)
selectionSort l = do
  (s, e) <- getBounds l
  minRefI <- newSTRef 0
  minRefValue <- newSTRef 0
  forM_ [s..(e-1)] $ \i -> do
    writeSTRef minRefI i
    minValue <- readArray l i
    writeSTRef minRefValue minValue
    forM_ [(i+1)..e] $ \j -> do
      currentValue <- readArray l j
      minValue <- readSTRef minRefValue
      when (currentValue < minValue) $ do
        writeSTRef minRefValue currentValue
        writeSTRef minRefI j
    minI <- readSTRef minRefI
    when (minI /= i) $ do
      swap l i minI
  return l

swap :: STArray s Int a -> Int -> Int -> ST s ()
swap arr i j = do
  elem1 <- readArray arr i
  elem2 <- readArray arr j
  writeArray arr i elem2
  writeArray arr j elem1

