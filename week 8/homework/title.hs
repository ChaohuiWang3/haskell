import Data.Char

capitaliseLong :: String -> String
capitaliseLong word = if (length word >= 4) then (capitalised word)
                       else (lowercase word)

lowercase :: String -> String
lowercase xs = [toLower x | x <- xs]

title :: [String] -> [String]
title [] = []
title (w:words) = capitalised w : [capitaliseLong w | w <- words]
