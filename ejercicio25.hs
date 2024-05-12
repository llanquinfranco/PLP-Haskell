{- Definir en Haskell la funcion alinear tal que, a partir de tres palabras y un total de caracteres 
para una lınea, debe dar como salida las palabras de manera que una quede justificada a la izquierda,
otra centrada y la tercera justificada a la derecha dentro de la misma lınea -}
alinear :: String -> String -> String -> Int -> String
alinear a b c n = a ++ concatenar longitud ++ b ++ concatenar longitud ++ c
    where
        longitud = (n - length a - length b - length c) `div` 2

concatenar :: Int -> String
concatenar longitud
    | (longitud > 0) = " " ++ concatenar (longitud - 1)
    | otherwise = ""