module TreeTranslator where
import Data.Tree as LibTree
import Trees

myTreeToLibTree :: Trees.Tree String -> LibTree.Tree String
myTreeToLibTree Leaf = LibTree.Node "Leaf" []
myTreeToLibTree (Trees.Node _ Leaf val Leaf) = LibTree.Node val []
myTreeToLibTree (Trees.Node _ lChild label rChild) = LibTree.Node label [myTreeToLibTree lChild, myTreeToLibTree rChild]

intArrayToStringArray :: [Integer] -> [String]
intArrayToStringArray = foldr ((:) . show) [""]
