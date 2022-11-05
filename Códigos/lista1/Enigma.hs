decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] [] = []
decEnigma [] xs = []
decEnigma as [] = []
decEnigma (a:as) xs = analise a xs : decEnigma as xs 


analise :: Char -> [(Char, Char)] -> Char
analise ch [] = ' '
analise ' ' xs = ' '
analise ch ((a,b):as) | ch == a = b
                      | otherwise = analise ch as

main = do
a <- getLine
b <- getLine
let result = decEnigma a (read b)
print result

