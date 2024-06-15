{- Dados tres pares de enteros, debe devolver en un cuarto par el menor y el mayor elemento de
los pares dados. Por ejemplo para la entrada (2,4) (3,5) (4,1) la salida debe ser (1,5) -}
menorMayor :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
menorMayor (a, b) (c, d) (e, f) = (minimo, maximo)
    where
        minimo = mini a (mini b (mini c (mini d (mini e f))))
        maximo = maxi a (maxi b (maxi c (maxi d (maxi e f))))

mini :: Int -> Int -> Int
mini x y
    | (x < y) = x
    | otherwise = y

maxi :: Int -> Int -> Int
maxi x y
    | (x > y) = x
    | otherwise = y