merge :: Ord a => [a] -> [a] -> [a] 
merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge (x : xs) (y : ys) = if x <= y
    then x : (merge xs (y : ys))
    else y : (merge (x : xs) ys)
