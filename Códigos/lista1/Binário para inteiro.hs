btoi :: String -> Int
btoi [] = 0
btoi (x:xs) = ((convertInt x - 48) * (2 ^ lenght xs)) + btoi xs

lenght :: [t] -> Int
lenght [] = 0
lenght (a:as) = 1 + lenght as

convertInt :: Char -> Int
convertInt ch = fromEnum ch

main = do
    s <- getLine
    let result = btoi s
    print result