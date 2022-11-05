somarListas :: [Int] -> [Int] -> [Int]
somarListas [] [] = []
somarListas xs ys = intToList (listToInt xs + listToInt ys)

listToInt :: [Int] -> Int
listToInt xs = sum (zipWith (*) (reverse xs) (iterate (*10) 1))

intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]

main :: IO ()
main = do
    input1 <- getLine
    input2 <- getLine
    let result = somarListas (read input1 :: [Int])  (read input2 :: [Int])
    print result


{-
somarListas :: [Int] -> [Int] -> [Int]
somarListas [] [] = []
somarListas xs ys = intToList (listToInt xs + listToInt ys)

listToInt :: [Int] -> Int
listToInt xs = read (foldl (++) [] [show y | y <- xs]) :: Int

intToList :: Int -> [Int]
intToList 0 = [0]
intToList x = [read y :: Int | y <- strToList (show x)]

strToList :: String -> [String]
strToList [] = []
strToList (a:as) = [a] : strToList as

main :: IO ()
main = do
    input1 <- getLine
    input2 <- getLine
    let result = somarListas (read input1)  (read input2)
    print result
-}