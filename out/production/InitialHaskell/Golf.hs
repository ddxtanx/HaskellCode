{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

skips :: [a] -> [[a]]
skips [] = []
skips x = [[x !! (i-1) | i<-[b,2*b..length x]] | b<-[1..length x]]
--

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (a:b:c:rest) = if (b > a) && (b > c) --if b is local max
                           then b:localMaxima (b:c:rest) --append b to the front of whatever the local maxes are for the array w/o a
                           else localMaxima (b:c:rest) -- just the local maxes of the array w/o a
histogram :: [Integer] -> String
histogram x = show x10
          where x10 = filter (<10) x