{- Definir una funcion tal que, dado un intervalo de años representado por n m pueda mostrar en una
tabla los años y la leyenda “es bisiesto” o “no bisiesto”. Un año es bisiesto si es multiplo de 4
(por ej. 1984), pero si el año es multiplo de 100 solo son bisiestos cuando a su vez son multiplos
 de 400. Por ej 1800 no es bisiesto, mientras que 2000 si -}
bisiestos :: Int -> Int -> String
bisiestos n m
    | (n <= m) && (n `mod` 400 == 0) = show n ++ "\t Bisiesto\n" ++ bisiestos (n + 1) m
    | (n <= m) && (n `mod` 100 == 0) = show n ++ "\t No Bisiesto\n" ++ bisiestos (n + 1) m
    | (n <= m) && (n `mod` 4 == 0) = show n ++ "\t Bisiesto\n" ++ bisiestos (n + 1) m
    | (n <= m) = show n ++ "\t No Bisiesto\n" ++ bisiestos (n + 1) m
    | otherwise = ""

main :: IO ()
main = do
    putStrLn "Ingrese el valor inicial (n): "
    n <- readLn
    putStrLn "Ingrese el valor final (m): "
    m <- readLn
    putStrLn (bisiestos n m)