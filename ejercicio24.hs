{- Definir una funcion tal que, dado un numero entero, genere la siguiente salida 
repitiendo la cantidad de dıgitos con una lınea para cada dıgito -}
cantDigitos :: Int -> String
cantDigitos n
    | (n < 10) = linea n n 
    | (n > 10) = cantDigitos (n `div` 10) ++ "\n" ++ linea m m
    | otherwise = ""
  where 
    m = (n `mod` 10)

linea :: Int -> Int -> String
linea n m
    | (m == 0) = ""
    | (m == 1) = show n
    | otherwise = show n ++ linea n (m - 1)

main :: IO ()
main = do
    putStrLn "Ingrese el digito"
    m <- readLn
    putStrLn (cantDigitos m)