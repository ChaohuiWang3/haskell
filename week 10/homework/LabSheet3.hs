mult :: [Int] -> Int
mult xs = foldr (*) 1 xs

posList :: [Int] -> [Int]
posList xs = filter(\x -> x>0) xs

trueList :: [Bool] -> Bool
trueList xs = foldr(&&) True xs

evenList :: [Int] -> Bool
evenList xs = foldr ((&&) . even) True xs

maxList :: Ord a => [a] -> a
maxList xs = foldr max (head xs) xs

inRange :: Int -> Int -> [Int] -> [Int] 
inRange a b xs = filter (\x -> (x>=a) && (x<=b)) xs

countPositives :: [Int] -> Int
countPositives xs = foldr (+) 0 (map(\x -> 1)(filter(\x -> x>0) xs))

myLength :: [Int] -> Int
myLength xs = foldr (+) 0 (map(\x -> 1) xs)

myMap :: (a -> b) -> [a] -> [b]
myMap a = foldr ((:) . a) []

myLength' :: [Int] -> Int
myLength' xs = foldr (+) 0 (myMap(\x -> 1) xs)