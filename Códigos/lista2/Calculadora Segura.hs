import Prelude hiding (Maybe (..))
import Data.Char (isDigit)

data Maybe a = Just a |
               Nothing
               deriving(Show)

safeCalc :: String -> IO ()
safeCalc xs = print (calc xs)

calc :: String -> Maybe Int
calc [] = Nothing
calc xs = doOp (strInt (pegaNum xs)) (pegaOp xs)


doOp :: [Int] -> String -> Maybe Int
doOp [] op = Nothing
doOp (a:[]) op | op == "sum" = Just (a+0)
               | op == "sub" = Just (a-0)
               | op == "mul" = Just (a*0)
               | otherwise = Nothing
doOp (a:b:as) op | op == "sum" = Just (a+b)
                 | op == "sub" = Just (a-b)
                 | op == "mul" = Just (a*b)
                 | b == 0 = Nothing
                 | otherwise = Just (a `div` b)

pegaNum :: String -> [String]
pegaNum [] = []
pegaNum (x:xs) | isDigit x && x /= '0' = [x] : pegaNum xs
               | otherwise = pegaNum xs

strInt :: [String] -> [Int]
strInt [] = []
strInt (a:as) = [(read a :: Int)] ++ strInt as

pegaOp :: String -> String
pegaOp [] = []
pegaOp (a:as) | isDigit a = pegaOp as
              | otherwise = a : pegaOp as


main = do
       a <- getLine
       safeCalc a