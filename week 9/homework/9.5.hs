import Data.Char
rotor :: Int -> [Char] -> [Char]
rotor a [] = []
rotor 0 xs = xs
rotor a (x:xs) = if (a >= 0) && (a < length(x:xs))
    then rotor (a-1) (xs ++ [x])
    else error"offset number out of range"

makeKey :: Int -> [(Char,Char)]
makeKey b = zip upperletter (rotor b upperletter)
    where upperletter = ['A'..'Z']

lookUp :: Char -> [(Char,Char)] -> Char
lookUp c [] = c
lookUp c ((c1,c2):pairs) = if c == c1
    then c2
    else lookUp c pairs

encipher :: Int -> Char -> Char
encipher d c =lookUp c (makeKey d)

normalise :: String -> String
normalise [] = []
normalise (x:xs) = if (x `elem` ['A'..'Z'] || (x `elem` ['0'..'9']))
    then x:normalise xs
    else if (x `elem` ['a'..'z'])
        then (toUpper x) : normalise xs
    else 
        normalise xs

encipherStr :: Int -> String -> String
encipherStr e [] = []
encipherStr e (x:xs) = encipheridv e (normalise(x:xs))
    where encipheridv e [] = []
          encipheridv e (x:xs) = (encipher e x) : (encipheridv e xs)
