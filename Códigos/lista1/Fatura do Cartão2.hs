logMes :: String -> String -> Double
logMes mes [] = 0.0
logMes [] str = 0.0
logMes mes str = aux (compras str) mes

compras :: String -> [String]
compras str = wordsWhen (==';') str

aux :: [String] -> String -> Double
--aux [] mes = 0.0
aux str mes = soma (transDouble (analise str mes))

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

analise :: [String] -> String -> [String]
analise [] mes = []
analise (x:y:z:xs) mes | ehMes x mes testeDia = z : analise xs mes
                       | otherwise = analise xs mes

testeDia :: [Int]
testeDia = [1..100]

ehMes :: String -> String -> [Int] -> Bool
ehMes [] mes testeDia = False
ehMes str mes [100] = False
ehMes str mes (a:as)| str == "0" ++ show a ++ " " ++ mes = True
                    | str == show a ++ " " ++ mes = True
                    | otherwise = ehMes str mes (as)

soma :: [Double] -> Double
soma [] = 0.0
soma xs = sum xs

transDouble :: [String] -> [Double]
transDouble [] = []
transDouble (x:xs)  = (read x :: Double) : transDouble xs

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result