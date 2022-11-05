data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

vazio :: Tree t -> Bool
vazio Nilt = True
vazio _ = False

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node t Nilt Nilt) = True
isBST (Node t (Node x lf rt) Nilt) = isBST (Node x lf rt) && (vazio (Node x lf rt) || maior (Node x lf rt) < t )
isBST (Node t Nilt (Node y lf1 rt2)) = isBST (Node y lf1 rt2) && (vazio (Node y lf1 rt2) || t < menor (Node y lf1 rt2))
isBST (Node t (Node x lf rt) (Node y lf1 rt2)) = isBST (Node x lf rt) && isBST (Node y lf1 rt2) && (vazio (Node x lf rt) || maior (Node x lf rt) < t ) && (vazio (Node y lf1 rt2) || t < menor (Node y lf1 rt2))

menor :: Ord t => Tree t -> t
menor (Node t lf rt) = min t (min y z)
    where y = if vazio lf then t else menor lf
          z = if vazio rt then t else menor rt

maior :: Ord t => Tree t -> t
maior (Node t lf rt) = max t (max y z)
    where y = if vazio lf then t else maior lf
          z = if vazio rt then t else maior rt


main = do
    s <- getLine
    let result = isBST (read s::Tree Int)
    print result