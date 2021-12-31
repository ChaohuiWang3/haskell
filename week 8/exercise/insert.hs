insert :: Int -> [Int] -> [Int]
insert a [] = []
insert a (x : xs) = if a < x
    then a : (x : xs)
    else
        x : insert a xs

