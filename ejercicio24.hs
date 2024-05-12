{- Definir una funcion tal que, dado un numero entero, genere la siguiente salida 
repitiendo la cantidad de dıgitos con una lınea para cada dıgito -}
linea :: Int -> Int -> String
linea n m
    | (m==0) = ""
    | (m==1) = show n
    | otherwise = show n ++ linea n (m-1)

ej24 :: Int -> String
ej24 n
    | (n < 10) = linea n n 
    | (n > 10) = ej24 (n `div` 10) ++ "\n" ++ linea m m
    | otherwise = ""
  where 
    m = (n `mod` 10)

main :: IO ()
main = do
    putStrLn "Ingrese el digito"
    m <- readLn
    putStrLn (ej24 m)