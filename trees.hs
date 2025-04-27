data ColorEnum = Red | Orange | Yellow | Green | Blue | Purple
data Pet = Cat String Int | Dog String Int

data Shape = Circle Float | Rectangle Float Float

data TreeInt = Leaf |
                Node Int TreeInt TreeInt
                deriving Show

treeSize :: TreeInt -> Int
treeSize Leaf = 0
treeSize (Node x izq der) = 1 + treeSize izq + treeSize der
--instancia de un tipo de dato


treeHeight:: TreeInt -> Int
treeHeight Leaf = 0
treeHeight (Node x left right) = 1 + max (treeHeight left) (treeHeight right)