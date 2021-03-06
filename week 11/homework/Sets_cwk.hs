module Coursework where

{-
  Your task is to design a datatype that represents the mathematical concept of a (finite) set of elements (of the same type).
  We have provided you with an interface (do not change this!) but you will need to design the datatype and also 
  support the required functions over sets.
  Any functions you write should maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a list
  (as in the example below). Alternatively, one could use an algebraic data type, or 
  wrap a binary search tree.
  Extra marks will be awarded for efficient implementations if appropriate.

  You are NOT allowed to import anything from the standard library or other libraries.
  Your edit of this file should be completely self-contained.

  DO NOT change the type signatures of the functions below: if you do,
  we will not be able to test them and you will get 0% for that part. While sets are unordered collections,
  we have included the Ord constraint on most signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Please include everything in this file.
-}

{-
   PART 1.
   You need to define a Set datatype. Below is an example which uses lists internally.
   It is here as a guide, but also to stop ghci complaining when you load the file.
   Free free to change it.
-}

-- you may change this to your own data type
-- newtype Set a = Set { unSet :: [a] }
data Set a = Set [a] deriving(Show)


{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Ord a => Set a -> [a]
toList (Set []) = []
toList (Set [x]) = [x]
toList (Set (x:xs)) = toList(Set(filter(<=x) xs)) ++ [x] ++ toList(Set(filter(>x) xs))


-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList [] = Set []
fromList [x] = Set [x]
fromList (x1:x2:xs) = Set(fromList1 (x1:x2:xs))
   where fromList1 [] = []
         fromList1 [x] = [x]
         fromList1 (x1:x2:xs) = if x1 == x2
            then fromList1 (x1:xs)
            else x1 : fromList1 (x2:xs)
   


{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
   Set (x:xs) == Set (y:ys) = toList(fromList(toList(Set(x:xs)))) == toList(fromList(toList(Set(y:ys))))


-- the empty set
empty :: Set a
empty = Set []


-- Set with one element
singleton :: a -> Set a
singleton x = Set [x]


-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x (Set []) = Set [x]
insert x (Set (y:ys)) = fromList(insert1 x (toList(Set (y:ys))))
   where insert1 x [] = [x]
         insert1 x (y:ys) = if x < y
            then x : y : ys
            else y : insert1 x ys   






-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union (Set[]) (Set []) = Set []
union (Set xs) (Set []) = (Set xs)
union (Set []) (Set ys) = (Set ys)
union (Set xs) (Set ys) = fromList(union1 (toList(Set xs)) (toList(Set ys)))
   where union1 [] [] = []
         union1 xs [] = xs
         union1 [] xs = xs
         union1 (x : xs) (y : ys) = if x <= y
            then x : (union1 xs (y : ys))
            else y : (union1 (x : xs) ys)




-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set[]) (Set []) = Set []
intersection (Set xs) (Set []) = Set []
intersection (Set []) (Set ys) = Set []
intersection (Set xs) (Set ys) = fromList(intersection1 (toList(Set xs)) (toList(Set ys)))
   where intesection1 [] [] = []
         intersection1 xs [] = []
         intersection1 [] xs = []
         intersection1 (x:xs) ys = if (x `elem` ys)
            then x : (intersection1 xs ys)
            else intersection1 xs ys




-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set []) (Set []) = Set []
difference (Set []) (Set ys) = Set []
difference (Set xs) (Set []) = (Set xs)
difference (Set xs) (Set ys) = fromList(difference1 (toList(Set xs)) (toList(Set ys)))
   where difference1 [] [] = []
         difference1 xs [] = xs
         difference1 [] xs = []
         difference1 (x:xs) ys = if (x `elem` ys)
            then difference1 xs ys
            else x : (difference1 xs ys)




-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member x (Set []) = False
member x (Set [y]) = if x == y
   then True
   else False
member x (Set (y:ys)) = if x == y
   then True
   else member x (Set ys)





-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality(Set []) = 0
cardinality(Set xs) = length (xs)


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap _ (Set []) = Set []
setmap f (Set xs) = Set((map f xs))


setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr _ (Set []) y = y
setfoldr f (Set xs) y = foldr f y xs



-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet (Set []) = Set [Set []]
powerSet (Set xs) = Set(map (fromList) (powerSet1 xs))
   where powerSet1 [] = [[]]
         powerSet1 (x:xs) = map (x:) (powerSet1 xs) ++ powerSet1 xs




-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian (Set []) (Set []) = Set []
cartesian (Set xs) (Set ys) = Set[(x,y) | x <- xs, y <- ys]





-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (Ord a) => (a -> Bool) -> Set a -> (Set a, Set a)
partition _ (Set []) = (Set[], Set[])
partition f (Set xs) = (Set(filter f xs), (difference (Set xs) (Set(filter f xs))))



{-
   On Marking:
   Be careful! This coursework will be marked using QuickCheck, against Haskell's own
   Data.Set implementation. Each function will be tested for multiple properties.
   Even one failing test means 0 marks for that function.

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough for a
   passing mark of 40%.

-}
