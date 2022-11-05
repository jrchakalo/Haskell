type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa commands = foldl calc 0 commands

calc :: Int -> (Comando, Valor) -> Int
calc x (cmd, y) | cmd == "Multiplica" = (x * y)   
                | cmd == "Soma" = (x + y)
                | cmd == "Subtrai" = (x - y)
                | y == 0 = -666
                | otherwise = (div x y)


main = do
    a <- getLine
    let result = executa (read a)
    print result