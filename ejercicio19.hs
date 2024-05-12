-- Definir una funcion minMax la cual retorne el mınimo y el maximo de una tupla --
minMax :: (Int, Int) -> String
minMax (x, y)
    | (x > y) = "El maximo es " ++ show x ++ " y el minimo es " ++ show y
    | (y > x) = "El maximo es " ++ show y ++ " y el minimo es " ++ show x