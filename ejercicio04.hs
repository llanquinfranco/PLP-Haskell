cuantosIguales :: Int -> Int -> Int -> Int
cuantosIguales x y z
    | (x == y) && (y == z) = 3
    | (x == y) || (y == z) || (x == z) = 2
    | otherwise = 0

cuantosIgualesDeDos :: Int -> Int -> Int
cuantosIgualesDeDos x y
    | (x == y) = 2
    | otherwise = 0