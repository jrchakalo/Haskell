-- 17:17 --

--1)

data Time = Australia | Dinamarca | Franca | Peru 
          | Argentina | Croacia | Islandia | Nigeria
          deriving (Eq, Show)

type Jogo = (Time, Int, Time, Int) 

jogos1 :: [Jogo]
jogos1 = [(Australia, 1, Dinamarca, 3), (Franca, 2, Peru, 0), 
          (Australia, 0, Franca, 2),(Dinamarca, 0, Peru, 0), 
          (Dinamarca, 0, Franca, 1), (Australia, 0, Peru, 0), 
          (Argentina, 1, Croacia, 0), (Islandia, 0, Nigeria, 1), 
          (Argentina, 1, Islandia, 0), (Argentina, 1, Nigeria, 1), 
          (Croacia, 0, Islandia, 0), (Croacia, 1, Nigeria, 2)]


gols :: Time -> [Jogo] -> Int
gols tm [] = 0
gols tm ((x,y,z,w):xs) | tm == x = y + gols tm xs
                       | tm == z = w + gols tm xs
                       | otherwise = gols tm xs

saldo :: Time -> [Jogo] -> Int
saldo tm [] = 0
saldo tm ((x,y,z,w):xs) | tm == x = (y - w) + saldo tm xs
                        | tm == z = (w - y) + saldo tm xs
                        | otherwise = saldo tm xs

pontos :: Time -> [Jogo] -> Int
pontos tm [] = 0
pontos tm ((x,y,z,w):xs) | tm == x = if y > w then 3 + pontos tm xs else if y == w then 1 + pontos tm xs else 0 + pontos tm xs
                         | tm == z = if w > y then 3 + pontos tm xs else if w == y then 1 + pontos tm xs else 0 + pontos tm xs
                         | otherwise = pontos tm xs

data Grupo = Grup Char Time Time Time Time
            deriving (Eq, Show)

--grupoC :: Grupo 
grupoC = Grup 'C' Australia Dinamarca Franca Peru
--grupoD :: Grupo
grupoD = Grup 'D' Argentina Croacia Islandia Nigeria

s