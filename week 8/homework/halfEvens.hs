halfEvens :: [Int] -> [Int]
halfEvens [] = []
halfEvens (x : xs) = if rem x 2 /= 0
    then x : halfEvens xs
    else
        div x 2 : halfEvens xs