{- Definir una funcion que dada una tripla (a,b,c), genere una cadena con los numeros
comprendidos entre a y b que cumplan con la propiedad de ser multiplos de c -}
multiplos :: (Int, Int, Int) -> String
multiplos (a, b, c)
    | (a <= b) && (a `mod` c == 0) = show a ++ " " ++ multiplos (a + 1, b, c)
    | (a <= b) = "" ++ multiplos (a + 1, b, c)
    | otherwise = ""