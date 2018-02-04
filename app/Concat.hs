module Concat where
import CreditCard

concatNums :: [Integer] -> String
concatNums = foldr ((++) . show) ""

concatNumsInt :: [Integer] -> Integer
concatNumsInt x = read (concatNums x) :: Integer
limit = 1000
satisfyingSet = [(a,b,c) | a<-[0..limit], b<-[0..limit], c<-[0..limit], a^3+b^3+c^3==concatNumsInt [a,b,c]]
mainConcat :: IO()
mainConcat = print satisfyingSet

