data Ops = SUM | MUL | SUB
           deriving (Read, Eq)
           

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node cmd lf rt) | cmd == SUM = evalTree lf + evalTree rt
                          | cmd == MUL = evalTree lf * evalTree rt
                          | otherwise = evalTree lf - evalTree rt

main = do
    s <- getLine
    let result = evalTree (read s)
    print result