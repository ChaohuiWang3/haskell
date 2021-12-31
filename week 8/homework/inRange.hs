inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [x | x <- xs, a <= x, x <= b]