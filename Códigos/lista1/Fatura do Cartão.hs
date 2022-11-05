minMaxCartao :: String -> (Double, Double)
minMaxCartao [] = (0.0, 0.0)
minMaxCartao str = aux (compras str)

compras :: String -> [String]
compras str = wordsWhen (==';') str

aux :: [String] -> (Double, Double)
aux [] = (0.0, 0.0)
aux str = minMax (transDouble (analise str))

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

analise :: [String] -> [String]
analise [] = []
analise (x:y:z:xs) = z : analise xs

minMax :: [Double] -> (Double, Double)
minMax [] = (0.0, 0.0)
minMax ld = (minLd ld, maxLd ld)

minLd :: [Double] -> Double
minLd [] = 1000000.0 
minLd (a:as) = min a (minLd as) 

maxLd :: [Double] -> Double
maxLd [] = 0.0
maxLd (a:as) = max a (maxLd as)

transDouble :: [String] -> [Double]
transDouble [] = []
transDouble (x:xs)  = (read x :: Double) : transDouble xs

main = do
    a <- getLine
    let result = minMaxCartao a
    print result




