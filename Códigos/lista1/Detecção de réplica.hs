isReplica :: String -> Int -> Char -> Bool
isReplica str n ch = str == constString ch n


constString :: Char -> Int -> String
constString ch n | n == 0 = []
                 | otherwise = ch : constString ch (n-1)

main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result