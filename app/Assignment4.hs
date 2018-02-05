module Assignment4 where
import TreeTranslator
import Data.Tree hiding (Tree, Node, foldTree)
import Data.Tree.View
import Trees
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1rewrite :: [Integer] -> Integer
fun1rewrite xs =foldr (\x y -> (x-2)*y) 1 (filter even xs)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2rewrite :: Integer -> Integer
fun2rewrite = sum . filter even . takeWhile (/= 1) . iterate hailstone
{- Iterates hailstone because fun2 is repeatedly applies
   Takewhile because fun1 1 = 0, and 0 is identity for +
   filters out the odd numbers because only evens are summed abd 3*odd + 1 is always even
   sums because even n is n + rest of series
-}

hailstone :: Integer -> Integer
hailstone x
  | even x = x `div` 2
  | odd x = 3*x + 1

xor :: [Bool] -> Bool
xor xs = foldr (/=) False (filter (== True) xs)

map' :: (a -> b) -> [a] -> [b]
map' f xs= foldr (\x y -> f(x):y) [] xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram x = map (\x -> 2 * x + 1) (filter (`notElem` arr) ltX)
  where
    arr = [i + j + 2 * i * j | i <- [1 .. x], j <- [1 .. x], i <= j, i + j + 2 * i * j <= x]
    ltX = [1 .. x - 1]

isPrime :: Integer -> Bool
isPrime x = 0 `notElem` divArr
  where
    divArr = [x `mod` k | k <- [2 .. x - 1]]


run :: IO()
run = do
          let tree = generateNElementTree 100000
          print tree
          print (isComplete tree)
          print (validHeight tree)
    {--}
--(foldTree [1..13])
--(isComplete (Node 3 (Node 1 (Node 1 Leaf 0 Leaf) 0 Leaf) 0 (Node 1 Leaf 0 Leaf)))
