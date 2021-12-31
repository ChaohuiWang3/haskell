import Data.Char
inRange :: Int -> Int ->[Int] -> [Int]
inRange a b [] = []
inRange a b (x : xs) = if x >= a && x <= b
    then x : inRange a b xs
    else
        inRange a b xs

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x : xs) = if x > 0
    then 1 + countPositives xs
    else 
        0 + countPositives xs

capitalised :: String -> String
capitalised "" = ""
capitalised (x : xs) = toUpper x : map(toLower) xs

lower (a : as) = toLower a : map(toLower) as

lengthofword z = if length z >= 4
    then capitalised z
    else lower z


title :: [String] -> [String]
title [] = []
title (y : ys) = capitalised y :  map(lengthofword) ys