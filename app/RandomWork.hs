module RandomWork where

average :: [Int] -> Int
average xs = sum xs `div` length xs

evenCount :: [Int] -> Int
evenCount xs = length (filter even xs)

squareArray :: [Int] -> [Int]
squareArray = map (\x -> x * x)

