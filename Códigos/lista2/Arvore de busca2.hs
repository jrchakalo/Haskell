data Tree t = Nilt |
               Node t (Tree t) (Tree t)
               deriving (Read, Show)


insertList :: Ord t => Tree t -> [t] -> Tree t
insertList Nilt [] = Nilt
insertList Nilt (a:as) = insertList (Node a Nilt Nilt) as
insertList (Node t lf rt) [] = (Node t lf rt)
insertList (Node t lf rt) (a:as) | a < t = insertList (Node t (insertElem lf a) rt) as
                                 | otherwise = insertList (Node t lf (insertElem rt a)) as


insertElem :: Ord t => Tree t -> t -> Tree t
insertElem Nilt x = (Node x Nilt Nilt)
insertElem (Node t lf rt) x | x < t = (Node t (insertElem lf x) rt)
                            | otherwise = (Node t lf (insertElem rt x))


main = do
    a <- getLine
    b <- getLine
    let result = insertList (read a::Tree Int) (read b)
    print result
