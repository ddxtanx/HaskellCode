module TreeTranslator where
import Data.Tree as LibTree
data Tree a = Leaf
  | Node Integer (TreeTranslator.Tree a) a (TreeTranslator.Tree a)
  deriving (Show, Eq)

myTreeToLibTree :: TreeTranslator.Tree String -> LibTree.Tree String
myTreeToLibTree Leaf = LibTree.Node "Leaf" []
myTreeToLibTree (TreeTranslator.Node _ Leaf val Leaf) = LibTree.Node val []
myTreeToLibTree (TreeTranslator.Node _ lChild label rChild) = LibTree.Node label [myTreeToLibTree lChild, myTreeToLibTree rChild]

intArrayToStringArray :: [Integer] -> [String]
intArrayToStringArray = foldr ((:) . show) [""]
