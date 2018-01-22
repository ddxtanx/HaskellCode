{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

skips :: [a] -> [[a]]
skips [] = []
skips x = [[x !! (i-1) | i<-[b,2*b..length x]] | b<-[1..length x]]
--

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:rest)
  | a<b && c<b = b:localMaxima(b:c:rest)
  | otherwise = localMaxima(b:c:rest)
  --If b is a local max, append it to the list of other local maxes for the rest of the array
  --If it isn't just return the local maxes of the rest of the array
localMaxima _ = []


histogram :: [Integer] -> String
histogram x = show x10
          where x10 = filter (<10) x