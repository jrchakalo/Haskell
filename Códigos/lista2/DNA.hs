data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

toList :: Tree Int -> [Int]
toList (Nilt) = []
toList (Node r lf rt) = map modCinc ((toList lf) ++ [r] ++ (toList rt))

toBase :: [Int] -> String
toBase [] = []
toBase (a:as) | a == 0 = 'E' : toBase as
              | a == 1 = 'M' : toBase as
              | a == 2 = 'A' : toBase as
              | a == 3 = 'C' : toBase as
              | otherwise = 'S' : toBase as

sepList :: [t]  -> [[t]]
sepList  [] = []
sepList  xs | length (xs) <= 8 = xs : []
          | otherwise = take 8 xs : sepList (takeOff 8 xs)

takeOff :: Int -> [t] -> [t]
takeOff n [] = []
takeOff n (a:as) | n == 0 = a:as
                 | otherwise = takeOff (n-1) as

dna1 :: Tree Int -> [String]
dna1 (Nilt) = []
dna1 (Node r lf rt) = sepList (toBase (toList (Node r lf rt)))

modCinc :: Int -> Int
modCinc x = x `mod` 5

main :: IO ()
main = do

  input <- getLine

  let result = dna1 (read input :: Tree Int)

  print result