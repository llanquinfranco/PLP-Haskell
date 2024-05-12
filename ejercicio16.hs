-- Defina una funcion tal que hacerEspacios n devuelva un String de n espacios --
hacerEspacios :: Int -> String
hacerEspacios n
    | (n == 0) = " "
    | (n > 0) = " " ++ hacerEspacios (n - 1)