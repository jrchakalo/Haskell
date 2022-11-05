data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)


maiorDiametro :: Ord t => Tree t -> Int
maiorDiametro (Nilt) = 0
maiorDiametro (Node r Nilt Nilt) = 0
maiorDiametro (Node r lf Nilt) = maiorDiametro (lf)
maiorDiametro (Node r Nilt rt) = maiorDiametro (rt)
maiorDiametro (Node r lf rt) | calcDiam (lf) > calcDiam (Node r lf rt) = calcDiam (lf)
                             | calcDiam (rt) > calcDiam (Node r lf rt) = calcDiam (rt)
                             | otherwise = calcDiam (Node r lf rt)

calcDiam :: Ord t => Tree t -> Int
calcDiam (Node r lf rt) = 1 + (totSub lf) + (totSub rt)

totSub :: Ord t => Tree t -> Int
totSub (Nilt) = 0
totSub (Node r lf rt) | totNo lf < totNo rt = 1 + totSub rt
                      | otherwise = 1 + totSub lf

totNo :: Ord t => Tree t -> Int
totNo (Nilt) = 0
totNo (Node r lf rt) = 1 + (totNo lf) + (totNo rt)


main = do
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result
