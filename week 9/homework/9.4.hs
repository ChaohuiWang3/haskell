msort :: Ord a => [a] -> [a]

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x  (isort xs)
    where insert x [] = [x]
          insert x (y : ys) = if x <= y
              then x : (y : ys)
              else y : (insert x ys)

merge :: Ord a => [a] -> [a] -> [a] 
merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge (x : xs) (y : ys) = if x <= y
    then x : (merge xs (y : ys))
    else y : (merge (x : xs) ys)

msort [] = []
mosrt (x : xs) = merge (isort(filter odd (x : xs))) (isort(filter even (x : xs)))