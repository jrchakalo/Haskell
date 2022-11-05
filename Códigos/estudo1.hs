double :: [Int] -> [Int]
double [] = []
double (a:as) = (a*2) :double as
-------------------------------------------------------------------------------------------

member :: [Int] -> Int -> Bool
member [] n = False
member (a:as) n | a == n = True
                | otherwise = member as n
------------------------------------------------------------------------------------------

digits :: String -> String
digits [] = []
digits (a:as) | (a == '0') || (a == '1') || (a == '2') || (a == '3') || (a == '4') || (a == '5') || (a == '6') || (a == '7') || (a == '8') || (a == '9')  = a : digits as
              | otherwise = digits as
--------------------------------------------------------------------------------------------

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] [] = []
sumPairs a [] = a
sumPairs [] b = b
sumPairs (a:as) (b:bs) = (a+b) : sumPairs as bs
------------------------------------------------------------------------------------------------

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:as) = (quickSort mn) ++ [a] ++ (quickSort ma)
                where mn = filter (< a) as
                      ma = filter (>=a) as
-------------------------------------------------------------------------------------------------

fibo :: Int -> [Int]
fibo 0 = [0]
fibo 1 = [1]
fibo n | par n = [calc n] ++ fibo (n-1)
       | otherwise = fibo (n-1)


par :: Int -> Bool
par n | mod (calc n) 2 == 0 = True
      | otherwise = False

calc :: Int -> Int
calc 0 = 0
calc 1 = 1
calc n = calc (n-1) + calc (n-2)

------------------------------------------------------------------------------------------------
{-
-- Compreensao de Lista --

Serve para construir lista de uma forma mais otimizada.
Uma compreensao de lista eh uma expressao que tem como resultado uma lista.

Normalmente possui a o seguinte formato:

[expressao que usa variavel | variavel <- lista, condicao]

uma expressao que usa uma variavel, a variavel que testa cada elemento de uma lista e condicao que pode dizer
que so vai avaliar a expressao se a condicao for verdadeira.

Exemplo especifico:
-}

quadl xs = [x*x | x <- xs]
--Tira o quadado de todos os elementos de uma lista.

{-A expressao nao precisa, necessariamente, usar os elementos da lista para criar uma nova lista.-}

zerof xs = [0 | x <- xs]
--Repete 0 tantas vezes quanto o tamanho da lista passada.

{-Com compreensao de lista voce consegue colocar o condicao para filtrar os elementos que entram em uma lista.-}

paresl xs = [x | x <- xs, ehPar x]
      where ehPar x = x `mod` 2 == 0
--Constroi uma nova lista apenas com os pares da lista orginal.

doistresl xs = [x | x <- xs, ehPar x || multTre x] 
      where ehPar x = x `mod` 2 == 0 
            multTre x = x `mod` 3 == 0  
--Constroi uma nova lista com os multiplos de dois e tres.

-- Exercicio --

{-
Suponha um "banco de dados" de uma biblioteca, onde possui uma tupla com o nome de uma pessoa e o livro que ela
pegou emprestado. escreve as funcoes para: saber quais livros uma pessoa tem, emprestar livros, ver se um livro
esta emprestado, saber quantos emprestimos uma pessoa tem, emprestar e devolver.

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseEx :: BancoDados
baseEx = [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr.Norrell"), ("Fernando","A Game of Thrones")]

livros :: BancoDados -> Pessoa -> [Livro]

emprestimos :: BancoDados -> Livro -> [Pessoa]

emprestado :: BancoDados -> Livro -> Bool

qtdEmprestimos :: BancoDados -> Pessoa -> Int

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
-}

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseEx :: BancoDados
baseEx = [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr.Norrell"), ("Fernando","A Game of Thrones")]

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [lb | (pb, lb) <- bd, pb == p]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [pb | (pb, lb) <- bd, lb == l]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l | emprestimos bd l /= [] = True
                | otherwise = False

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length (livros bd p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p l = [(p, l)]
emprestar (b:bs) p l | (p, l) == b = (b:bs)
                     | otherwise = b : emprestar bs p l

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver (b:bs) p l | (p, l) == b = bs
                    | otherwise = b : devolver bs p l

{-
Defina uma função que ordena uma lista de
inteiros utilizando o algoritmo quick sort,
lançando mão de compreensões de listas
-}

quickSortl :: [Int] -> [Int]
quickSortl [] = []
quickSortl (x:xs) = [y | y <- xs, y < x] ++ x : quickSortl [y | y <- xs, y >= x]
{-Faz a compreensao de lista para achar os numeros maiores que a cabeca depois concatena (++) com
uma lista formada pela cabeca e o quick sort da compreensao dos numeros maiores ou igual a cabeca,
fazendo assim eles serem ordenados recursivamente.-}