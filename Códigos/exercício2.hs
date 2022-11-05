-- 16:04 --
-- 17:00 --

--1)

type Chave = [(Char, Char)]

rot13parcial :: Chave
rot13parcial = [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'), ('g','t'),('h','u'),('i','v'),('j','w'),('k','x'),('l','y'), ('m','z')]

cipher :: Chave -> String -> String
cipher key [] = []
cipher key (a:as) = verif key a : cipher key as

verif :: Chave -> Char -> Char
verif [] ch = ch
verif (a:as) ch | fst a == ch = snd a
                | otherwise = verif as ch

--2)

inverteChave :: Chave -> Chave
inverteChave [] = []
inverteChave ((x,y): as) = (y,x) : inverteChave as

--3)

type FuncaoChave = (Char -> Char)

trocaSoLetraL :: FuncaoChave
trocaSoLetraL 'l' = 'b'
trocaSoLetraL c = c


cipherf :: FuncaoChave -> String -> String
cipherf fch [] = []
cipherf fch (a:as) = fch a : cipherf fch as


--4)

-- Não, pois chave é uma lista de pares de char e o argumento da FuncaoChave é char para char, ou seja, recebe um char e devolve um char.


--5)

data KeyTree = Node Char Char KeyTree KeyTree | Empty

chaveParcial :: KeyTree
chaveParcial = Node 'h' 'u' (Node 'c' 'p' (Node 'b' 'o' (Node 'a' 'n' Empty Empty) Empty) (Node 'e' 'r' Empty Empty)) (Node 'l' 'y' Empty (Node 'm' 'z' Empty Empty))


cipherT :: KeyTree -> String -> String
cipherT keyt [] = []
cipherT keyt (a:as) = verift keyt a : cipherT keyt as


verift :: KeyTree -> Char -> Char
verift Empty ch = ch
verift (Node a b lf rt) ch | a == ch = b 
                           | ch < a = verift lf ch
                           | ch > a = verift rt ch

{-
cipher :: Chave -> String -> String
cipher key [] = []
cipher key (a:as) = verif key a : cipher key as

verif :: Chave -> Char -> Char
verif [] ch = ch
verif (a:as) ch | fst a == ch = snd a
                | otherwise = verif as ch
-}