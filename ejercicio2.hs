losCuatroIguales :: Int -> Int -> Int -> Int -> Bool
losCuatroIguales x y z w
    | ((x == y) && (y == z) && (z == w)) = True
    | otherwise = False