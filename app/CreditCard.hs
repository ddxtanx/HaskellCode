module CreditCard where


toDigits :: Integer -> [Integer]
toDigits x
  | x<0 = []
  | x<10 = [x]
  | otherwise = toDigits(x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer] --Starts at right of array
doubleEveryOther xs = [a*b | (a,b)<-zip xs (cycle [1, 2])]


sumDigits :: [Integer] -> Integer
sumDigits xs = sum arr
               where arr = [sum (toDigits x) | x<-xs]

validate :: Integer -> Bool
validate card = sumDigits(doubleEveryOther (toDigits card)) `mod` 10 == 0
main :: IO()
main = print (validate 4012888888881881)