module Trees where
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree (x:xs) = foldr binaryInsert Leaf xs

getHeight (Node a _ _ _) = a
getHeight Leaf = -1 --Since Leaf Node is Node 0 _ _ _, Leaf must have 1 less, so it's height is -1

sameHeight :: Tree a -> Tree a -> Bool
sameHeight x y = getHeight x == getHeight y

almostSameHeight :: Tree a -> Tree a -> Bool
almostSameHeight x y = abs (getHeight x - getHeight y) <= 1

hasLeaf :: Tree a -> Bool
hasLeaf (Node a Leaf _ _) = True
hasLeaf (Node a _ _ Leaf) = True
hasLeaf _ = False

isLeaf :: Tree a -> Bool
isLeaf Leaf = True
isLeaf _ = False

booleanTreeProperty :: (Integer -> Tree a -> Tree a -> Bool) -> Tree a -> Bool -> Bool
booleanTreeProperty _ Leaf initialTruthValue = initialTruthValue
booleanTreeProperty propertyFunction (Node height lChild _ rChild) init = nodeSatisfies && childrenSatisfy
  where nodeSatisfies = propertyFunction height lChild rChild
        childrenSatisfy = booleanTreeProperty propertyFunction lChild init && booleanTreeProperty propertyFunction rChild init
{-This is similar to a fold on an array where it takes in a function on the height of a tree and it's children, a tree, and an
  initial value for the Leaf. Examples are below
-}


degreesOfBalanced :: Tree a -> (Tree a -> Tree a -> Bool) -> Bool
degreesOfBalanced tree comparator = booleanTreeProperty (\_ lChild rChild -> comparator lChild rChild) tree True
--Determines the degree of a tree's 'balanced-ness'. A Leaf is always seen as balanced, no matter what the definition

validHeight :: Tree a -> Bool
validHeight tree = booleanTreeProperty (\h l r -> h == max (getHeight l) (getHeight r) + 1) tree True
--Determines if a tree's height is actually valid. A Leaf always has a valid height

isComplete :: Tree a -> Bool
isComplete node = booleanTreeProperty (\h l r -> sameHeight l r && noSingleLeaves l r) node True
  where
    noSingleLeaves lChild rChild = not (isLeaf lChild) || not (isLeaf rChild) || (isLeaf lChild && isLeaf rChild)
--Determines if a tree is complete, if every node's children are either both nodes or both leaves. A Leaf is complete.

isBalanced :: Tree a -> Bool
isBalanced tree = degreesOfBalanced tree sameHeight

isAlmostBalanced :: Tree a -> Bool
isAlmostBalanced tree = degreesOfBalanced tree almostSameHeight

replaceLeaf :: Tree a -> a -> Tree a
replaceLeaf (Node 0 Leaf nodeVal Leaf) val = Node 1 (Node 0 Leaf val Leaf) nodeVal Leaf
replaceLeaf node@(Node height lChild nodeVal rChild) val
  | isLeaf lChild = Node height (Node 0 Leaf val Leaf) nodeVal rChild
  | isLeaf rChild = Node height lChild nodeVal (Node 0 Leaf val Leaf)
  | otherwise = node

binaryInsert :: a -> Tree a -> Tree a
binaryInsert value Leaf = Node 0 Leaf value Leaf
binaryInsert value node@(Node height lChild nodeVal rChild)
  | hasLeaf node = replaceLeaf node value --Value replaces leaf
  | not (isComplete lChild) = Node newHeightL insertedLeft nodeVal rChild --Value is injected into
  | not (isComplete rChild) = Node newHeightR lChild nodeVal insertedRight
  | otherwise =
    if getHeight lChild < getHeight rChild
      then Node newHeightL insertedLeft nodeVal rChild
      else Node newHeightR lChild nodeVal insertedRight
  where
    insertedLeft = binaryInsert value lChild
    insertedRight = binaryInsert value rChild
    newHeightL = max (getHeight insertedLeft) (getHeight rChild) + 1
    newHeightR = max (getHeight insertedRight) (getHeight lChild) + 1

generateNElementTree :: Integer -> Tree Integer
generateNElementTree n = foldr binaryInsert Leaf [1..n]
