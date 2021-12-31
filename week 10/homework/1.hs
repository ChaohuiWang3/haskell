data Answer = Yes | No | Unknown

wonky :: Answer -> Answer

wonky Yes = No
wonky No = Unknown
wonky Unknown = Yes

data Shape = Circle Float | Rect Float Float |  Ellipse Float Float
area :: Shape -> Float
area (Ellipse a b) = pi * a * b

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) 
    | x==a = Node x left right
    | x<a = Node a (treeInsert x left) right
    | x>a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x==a = True
    | x<a = treeElem x left
    | x>a = treeElem x right

-- let nums = [6,4,1,7,3,5]
-- let numsTree = foldr treeInsert EmptyTree nums
-- let sum nums = foldr (+) 0 nums 