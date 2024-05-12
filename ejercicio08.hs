-- Funcion allDiferent con la propiedad que m /= n es True si m y n no son iguales --
allDiferent :: Int -> Int -> Int -> Bool
allDiferent x y z
    | ((x /= y) && (y /= z) && (x /= z)) = True
    | otherwise = False