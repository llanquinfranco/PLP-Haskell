{- Defina una funcion la cual tome un String y un numero natural n. El resultado 
son n copias de un String concatenado. (Si n=0 debe devolver un String vacıo) -}
duplicar :: String -> Int -> String
duplicar cadena n
    | (n == 0) = ""
    | (n > 0) = (duplicar cadena (n - 1)) ++ cadena