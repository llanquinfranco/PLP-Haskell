{- Dar una definicion de la funcion maxOcurr el cual retorna el maximo de
dos numeros, junto con el numero de veces que aparece -}
maxOcurr :: Int -> Int -> (Int, Int)
maxOcurr x y
    | (x > y) = (x, 1)
    | (y > x) = (y, 1)
    | (x == y) = (x, 2)

{- Usando esta u otra funcion defina maxOcurr que haga lo mismo pero con tres argumentos -}
maxOcurr2 :: Int -> Int -> Int -> (Int, Int)
maxOcurr2 x y z
    | (x >= y) && (x >= z) = (x, contar x x y z)
    | (y >= x) && (y >= z) = (y, contar y x y z)
    | (z >= x) && (z >= y) = (z, contar z x y z)

contar :: Int -> Int -> Int -> Int -> Int
contar maximo x y z
    | (maximo == x) && (maximo == y) && (maximo == z) = 3
    | ((maximo == x) && (maximo == y)) || ((maximo == y) && (maximo == z)) || ((maximo == x) && (maximo == z)) = 2
    | otherwise = 1