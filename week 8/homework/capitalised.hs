capitalised :: String -> String
capitalised [] = []
capitalised (x:xs) = toUpper x : [toLower x | x <- xs]
