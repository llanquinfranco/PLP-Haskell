{- Dar una definicion para la funcion factorialTable de forma que factorialTable m n tabule
los valores de los factoriales desde m hasta n inclusive. Validar los datos de entrada -}
factorialTable :: Int -> Int -> String
factorialTable m n
    | (m > n) = "Entrada no Valida"
    | (m == n) = show m ++ "!\t" ++ show (factorial m)
    | otherwise = show m ++ "!\t" ++ show (factorial m) ++ "\n" ++ (factorialTable (m + 1) n)

factorial :: Int -> Int
factorial n
    | (n == 0) = 1
    | (n == 1) = 1
    | (n > 1) = n * factorial(n - 1)

main :: IO ()
main = do
    putStrLn "Ingrese el valor inicial (m): "
    m <- readLn
    putStrLn "Ingrese el valor final (n): "
    n <- readLn
    putStrLn (factorialTable m n)