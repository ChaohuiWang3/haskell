countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]