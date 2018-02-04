module Assignment4 where
import TreeTranslator
import Data.Tree hiding (Tree, Node, drawTree)
import Data.Tree.View
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

myFoldTree :: [a] -> Tree a
myFoldTree (x:xs) = foldr binaryInsert (Leaf) xs

getHeight :: Tree a -> Integer
getHeight (Node a _ _ _) = a
getHeight Leaf = 0

sameHeight :: Tree a -> Tree a -> Bool
sameHeight x y = getHeight x == getHeight y

almostSameHeight :: Tree a -> Tree a -> Bool
almostSameHeight x y = abs (getHeight x - getHeight y) <= 1

degreesOfBalanced :: Tree a -> (Tree a -> Tree a -> Bool) -> Bool
degreesOfBalanced Leaf _ = True
degreesOfBalanced (Node _ lChild _ rChild) comparator = comparator lChild rChild && subtreeComparison
  where
    subtreeComparison = degreesOfBalanced lChild comparator && degreesOfBalanced rChild comparator

isBalanced :: Tree a -> Bool
isBalanced tree = degreesOfBalanced tree sameHeight

isAlmostBalanced :: Tree a -> Bool
isAlmostBalanced tree = degreesOfBalanced tree almostSameHeight

hasLeaf :: Tree a -> Bool
hasLeaf (Node a Leaf _ _) = True
hasLeaf (Node a _ _ Leaf) = True
hasLeaf _ = False

isLeaf :: Tree a -> Bool
isLeaf Leaf = True
isLeaf _ = False

replaceLeaf :: Tree a -> a -> Tree a
replaceLeaf (Node 0 Leaf nodeVal Leaf) val = Node 1 (Node 0 Leaf val Leaf) nodeVal Leaf
replaceLeaf node@(Node height lChild nodeVal rChild) val
  | isLeaf lChild = Node height (Node 0 Leaf val Leaf) nodeVal rChild
  | isLeaf rChild = Node height lChild nodeVal (Node 0 Leaf val Leaf)
  | otherwise = node

binaryInsert :: a -> Tree a -> Tree a
binaryInsert value Leaf = Node 0 Leaf value Leaf
binaryInsert value node@(Node height lChild nodeVal rChild)
  | hasLeaf node = replaceLeaf node value
  | not (isBalanced lChild) = Node newHeightL insertedLeft nodeVal rChild
  | not (isBalanced rChild) = Node newHeightR lChild nodeVal insertedRight
  | isBalanced node = Node newHeightL insertedLeft nodeVal rChild
  | otherwise =
    if getHeight lChild < getHeight rChild
      then Node newHeightL insertedLeft nodeVal rChild
      else Node newHeightR lChild nodeVal insertedRight
  where
    insertedLeft = binaryInsert value lChild
    insertedRight = binaryInsert value rChild
    newHeightL = max (getHeight insertedLeft) (getHeight rChild) + 1
    newHeightR = max (getHeight insertedRight) (getHeight lChild) + 1

testTree =
  Node
    3
    (Node 2 (Node 0 Leaf 'F' Leaf) 'I' (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
    'J'
    (Node 2 (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf) 'H' (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))

run :: IO()
run = drawTree (myTreeToLibTree (myFoldTree (intArrayToStringArray [1 .. 100])))