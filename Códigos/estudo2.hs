
{-
Parte 1

Funções polimórficas e classes de tipos.

(==) :: t -> t -> Bool (polimorfica)
(<)
(show)

Funcao monomorfica tem parametros e resultado fixos.
Ex:
Int -> Int -> Bool
Int -> Double -> Double

capitalize :: Char -> Char
capitalize ch = chr (ord ch + offset)
        where offset = ord 'A' - ord 'a'

(Eh de char para char)

Uma funcao eh polimorfica quando ela lida com varios tipos de dados distintos.
Eh usado quando os tipos nao importam.

-- Funcao zip --

(Tem no Prelude)
zip :: [t] -> [u] -> [(t,u)]
zip _ _= []
zip (a:as) (b:bs) = (a,b):zip as bs

Existem 4 tipos de polimorfimos:
Coersao de tipos (nao iremos ver)
Polimorfismo de inclusao
Polimorfismo parametrico (que esta sendo visto agora)
Polimorfismo de sobrecarga

os parametros tem letra minuscula por que representam uma variavel de um tipo que ainda vai ser adicionado.
se colocar os dois parametros com a mesma letra, vai receber dois parametros de um mesmo tipo.

ex1 :: t -> t -> t
(recebe e devolve um mesmo tipo)

ex2 :: t -> t -> u
(recebe o mesmo tipo e devolve outro tipo)

ex3 :: t -> u -> y
(recebe e devolve tudo diferente)

-}

-- Exemplos --

--(conta quantos elementos tem em uma lista)
lenght :: [t] -> Int
lenght [] = 0
lenght (a:as) = 1 + lenght as

--(inverte uma lista)
rev :: [t] -> [t]
rev [] = []
rev (a:as) = rev as ++ [a]

--(repete qualquer coisa n vezes e faz uma lista)
rep :: (Eq t, Num t) => t -> a -> [a]
rep 0 x = []
rep n x = x : rep (n-1) x

{-
Observe (Eq t, Num t) => 
Isso significa que t tem limitacoes, ele tanto e um numero quanto eh equiparavel.
Ele faz parte das Classes Eq e Num.

Polimorfismo de sobrecarga:
Mesmo nome de funcao mas ela funciona com diversos tipos.

Classe: Eh uma colecao de tipos para quais uma funcao esta definida/submetida.

Int, Float, Double, Integer -> Sao da classe Num.
(==), (/=) -> Sao da classe Eq.
(<), (<=), (max), (min) -> Sao da classe Ord.

Instancia: Eh um tipo que faz parte de uma classe.

Int -> Faz parte de Eq, Num e Ord.
Bool -> Faz parte de Eq, Ord e nao faz parte de Num.
-}

-- Exemplos --

--(ve se todos os elementos digitados sao iguais)
allEqual :: Eq t => t -> t -> t -> Bool
allEqual x y z = (x == y) && (y == z)


--(ve se um qualquer coisa faz parte de uma lista de qualquer coisa)
member :: Eq t => [t] -> t -> Bool
member [] b = False
member (a:as) b = (a == b) || member as b

-- Exercicio -- 

{-Crie uma função agrupar que recebe uma lista de listas de valores
de um tipo t que podem ser comparados para saber se são iguais e
devolve uma lista de pares (t, Int) onde o primeiro elemento é um
valor do tipo t que existe em pelo menos uma das sub-listas da
entrada e o segundo é o número de ocorrências desse valor nas sub
listas.-}

agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar [] = []
agrupar (a:as) = tup (junt (a:as))

tup :: Eq t => [t] -> [(t, Int)]
tup [] = []
tup (a:as) | cont a (a:as) > 1 = (a, cont a (a:as)) : tup (dup a as)
           | otherwise = (a, cont a (a:as)) : tup as

dup :: Eq t => t -> [t] -> [t]
dup x [] = []
dup x (a:as) | x == a = dup x as
             | otherwise = a : dup x as

cont :: Eq t => t -> [t] -> Int
cont t [] = 0
cont t (a:as) | t == a = 1 + cont t as
              | otherwise = cont t as

junt :: [[t]] -> [t]
junt [] = []
junt (a:as) = a ++ junt as

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Parte 2

Funcoes de alta ordem

Funcoes de alta ordem sao funcoes que recebem funcoes como parametro.

Exemplo:
-}

applyBinOper::(t -> t -> t) -> t -> t -> t
applyBinOper f x y = f x y
{-(dado uma funcao f e dois parametros x e y, aplique esses parametros x e y a funcao f)
applyBinOper (+) 23 56 = 79
applyBinOper (==) 23 56 = *erro*

Para solucionar esse erro deixamos a funcao um pouco mais generalista.-}

applyBinOper2::(t -> t -> u) -> t -> t -> u
applyBinOper2 f x y = f x y
{-applyBinOper2 (==) 23 56 = False

Em haskell, funcoes sao VALORES, pode ser usada como chamada e resultado de uma funcao.

O tipo de operacao que recebe uma lista como argumento e produz como resultado outra lista que tem o mesmo numero de
argumentos onde cada elemento da lista resultante eh o resultado de transforma um integrante da lista recebida como
parametro eh uma funcao que faz o mapeamento, ou mapping.

Haskell tem uma funcao padrao que ja faz isso, a funcao map (de mapeamento)

map :: (t -> u) -> [t] -> [u]
map f [] = []
map f (a:as) = f a : map f as

Dado uma lista do tipo T, aplica a funcao em cada elemento dela e retorna uma lista de elementos tipo U.

Exemplo:
-}

--funcao para dobrar um valor
times2 :: Int -> Int
times2 n = 2 * n

--funcao para tirar o quadrado de um numero
sqr :: Int -> Int
sqr n = n * n

--funcao para dobrar todos os elementos de uma lista
doubleList xs = map times2 xs

--funcao para tirar o quadrado de todos elementos de uma lista
sqrList xs = map sqr xs

--funcao que pega todos segundos valores de uma lista de tuplas
seconds :: [(t, u)] -> [u]
seconds xs = map snd xs

{-
Map tambem pode ser usado com compreensao de lista, mas nao eh mto util pois quando se quer fazer um mapeamento se
usa a funcao map.

As funcoes de alta ordem facilitam o entendimento e modificacao das funcoes e no reuso de codigo.

Folding

Folding, reducao ou dobramento consiste em reduzir todos os elementos de uma lista (ou de uma estrutura de dados qualquer) a apenas um elemento.

Uma funcao que faz isso, eh a funcao fold.

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)

Dado uma funcao f e uma lista, se a lista so tiver um elemento retorna ele, mas se a lista tiver 
cabeca e calda aplica a funcao na cabeca junto com o valor recursivo da calda.
Nao funciona com lista vazia.

fold = foldr1 no GHC

Exemplos:
-}

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)

sumList xs = fold (+) xs

multList xs = fold (*) xs

{-
Mas essa funcao nao eh muito generalista, entao normalmente utilizaremos a funcao foldr, que tem uma definicao padrao em Haskell.

foldr :: (t -> u -> u) -> u -> [t] -> u
foldr f s [] = s
foldr f s (a:as) = f a (foldr f s as)

Dado uma funcao, um caso base U e uma lista, retorna um valor U como resultado, a diferenca dessa funcao eh justamente
incluir um caso base, para funcionar com lista vazia.

Exemplos:
-}

sumList2 xs = foldr (+) 0 xs

multList2 xs = foldr (*) 1 xs

{-
Filter

Filter ou filtragem filtra os elementos que satisfazem uma condicao estabelecida, ou seja, de um predicado

filter :: (t -> Bool) -> [t] -> [t]
filter p [] = []
filter p (a:as) | p a = a : filter p as
                |otherwise = filter p as

Dado uma lista, se ela for vazia retorna uma lista vazia, mas se ela tiver cabeca e calda aplica o predicado p
a cabeca, se for verdadeiro constroi uma lista com a e procede recursivamente com a calda, se for falso apenas 
procede recursivamente com calda, ignorando a cabeca.

Exemplo:

digits st = filter isDigit st

letters st = filter isLetter st
-}

pares xs = filter isEven xs
    where isEven n = (n `mod` 2 == 0)

{-
Assim como map, filter pode ser utilizado com compreensao de lista.

filter p l = [a | a <- l, p a]

Dado uma lista l e um predicado p, a lista resultando vai ser composta por todo a pertencente a l que satisfaz
o preficado aplicado a A ser verdade.
-}

-- Exercicios --

{-
 Defina as seguintes funções sobre listas
    – eleva os itens de uma lista ao quadrado
        -> mapping
    – retorna a soma dos quadrados dos itens
        -> folding
    – manter na lista todos os itens maiores que zero.
        -> filtering
-}

quad xs = map sqr xs

sumQuad xs = foldr (+) 0 (quad xs)

maiorZero xs = filter mZero xs
    where mZero n = n > 0

{-
Construa uma nova definição para a função map em termos da função foldr. Faça o mesmo para filter.
-}

{-Pegando a funcao map:

map :: (t -> u) -> [t] -> [u]
map f [] = []
map f (a:as) = f a : map f as
-}

map2 :: (t -> u) -> [t] -> [u]
map2 f xs = foldr (\y ys -> f y : ys) [] xs

{-Dado um lambda y de uma lista ys, aplica a funcao f a esse lambda y e repete com ys, tendo como o
caso base [] e passando a lista xs.-}

{-Pegando a funcao filter:

filter :: (t -> Bool) -> [t] -> [t]
filter p [] = []
filter p (a:as) | p a = a : filter p as
                |otherwise = filter p as
-}

filter2 :: (t -> Bool) -> [t] -> [t]
filter2 p xs = foldr (\y ys -> if p y then y:ys else ys) [] xs

{-Dado um elemento lambda y de uma lista ys, usa a funcao if para determinar se a cabeca lambda y eh
igual a o predicado p, se for retorna a lista se nao so a calda, tendo como o caso base [] e passando
a lista xs.-}

--para testar

pares2 xs = filter isEven xs
    where isEven n = (n `mod` 2 == 0)

{-
O que a função
–> naosei l = foldr (++) [] (map sing l), onde sing a = [a], faz?
-}

sing :: t -> [t]
sing x = [x]

singl xs = map sing xs

naosei xs = foldr (++) [] (singl xs)

--Desmembra a lista em uma lista de listas, depois junta tudo de novo.

{-
Defina uma função:
–> maiores :: [[Int]] -> [Int]
Que, dada uma lista de listas de inteiros, devolve uma lista contendo o maior elemento de cada sub-lista da entrada
-}

maiores :: [[Int]] -> [Int]
maiores [] = []
maiores (a:as) = foldr max 0 a : maiores as


{-
Implemente funções takeWhile e dropWhile:

-> takeWhile (/= ' ') "The Office"
"The"
-> dropWhile (< 42) [1,5,6,25,64,13]
[64,13]
-}

takeWhile' :: (t -> Bool) -> [t] -> [t]
takeWhile' f [] = []
takeWhile' p (a:as)| p a = a : takeWhile' p as
                   | otherwise = []

--Semelhante ao filter, porem quando nao acha o elemento cria uma lista, e quando acha encerra a lista com lista vazia.

dropWhile' :: (t -> Bool) -> [t] -> [t]
dropWhile' f [] = []
dropWhile' p (a:as)| p a = dropWhile' p as
                   | otherwise = a:as

--Semelhando ao takeWhile e o filter, mas quando acha o predicado retorna a calda como lista resultante.

{-
Baseado nas definições de takeWhile e dropWhile defina as seguintes funções

getWord :: String -> String
dropWord :: String -> String
dropSpace :: String -> String
-}

getWord :: String -> String
getWord [] = []
getWord xs = takeWhile (/= ' ') xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace xs = dropWhile (> ' ') xs

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Parte 3

Expressoes Lambda

Forma que fazemos normalmente:
f :: Int -> Int -> Int
f x y = x + y*2

O que acontece por tras:
(((Lx . Ly . x + y*2)5)6) : onde L e o lambda, 5 o parametro para x e 6 o parametro para y
(Ly . 5 + y*2) 6
(5 + 6*2)
= 17 

Escrevendo expressoes lambda no interpretador:
f = \x -> \y -> x + y*2 : onde \ representa lambda

map (\x -> x*2) [1..10] = multiplica a lista por 2
(\x -> \y -> x + y*2) 5 6 = 17
let g = (\x -> \y -> x + y*2) 5 in g 6 = 17
let f = (\x -> \y -> x + y*2) in let g = f 5 in g 6 = 17

funcao composicao
(f . g) x = f (g x)
aplica f depois g em um valor x. da pra aplicar um monte de funcao de uma vez

let maisUm x = x + 1 in (maisUm . maisUm) 40 = 42
((+1) . (*5)) 40 = 201
((+1) . (*5) . (*100)) 40 = 20001

-}

-- Funcoes como valores e resultados --

maisUm :: Int -> Int
maisUm x = x + 1

twice :: (t -> t) -> (t -> t) -- Vai ter 2 parametros, nao 1
twice f = f . f

iter :: Int -> (t -> t) -> (t -> t) -- Vai ter 3 parametros, nao 2
iter 0 f = id
iter n f = (iter (n-1) f) . f

-- Exercicio --
{- Uma funcao que recebe os parametros de uma funcao invertido
ex: div 10 2 
    inv div 2 10
-}

inv :: (t -> u -> v) -> u -> t -> v
inv f x y = f y x

{- Defina uma funcao que insira um elemento em uma lista ordenada, na posicao correta, mantendo a lista ordenada.
    exemplo: insere 99 [1,2,15,72,101,102] ---> [1,2,15,72,99,101,102]

    insere :: Ord t => t -> [t] -> [t]
-}

insere :: Ord t => t -> [t] -> [t]
insere t [] = [t]
insere t (a:as) | a <= t = a : insere t as
                  | otherwise = t : (a:as)

{- Defina uma funcao que ordene uma lista de numeros inteiros (utilizando insertion sort) -}

insertionSort :: Ord t => [t] -> [t]
insertionSort [] = []
insertionSort (a:as) = insere a (insertionSort as)

-- ou --

insSort :: Ord t => [t] -> [t]
insSort ls = foldr insere [] ls

