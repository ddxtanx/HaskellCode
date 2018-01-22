module Hanoi where

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDiscs peg1 peg2 peg3
      | numDiscs == 1 = [(peg1, peg2)]
      | otherwise = hanoi (numDiscs-1) peg1 peg3 peg2 ++ [(peg1, peg2)] ++ hanoi (numDiscs-1) peg3 peg2 peg1


test = print (length (hanoi 5 "a" "b" "c"))