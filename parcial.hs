devolverIMC :: Float -> Float -> String
devolverIMC peso altura
    | (calculo peso altura <= 18.5) = "Masa Corporal Baja"
    | (calculo peso altura <= 25) = "Masa Corporal Normal"
    | (calculo peso altura <= 30) = "Masa Corporal Alta"
    | (calculo peso altura > 30) = "Masa Corporal Muy Alta"

calculo :: Float -> Float -> Float
calculo peso altura = peso / (altura * altura)

devolverIMC2 :: Float -> Float -> String
devolverIMC2 peso altura
    | (calculo <= 18.5) = "Masa Corporal Baja"
    | (calculo <= 25) = "Masa Corporal Normal"
    | (calculo <= 30) = "Masa Corporal Alta"
    | (calculo > 30) = "Masa Corporal Muy Alta"
    where
        calculo = peso / (altura * altura)

{- Un número narcisista, también conocido como número de Armstrong o número pleno, es un número natural
 que es igual a la suma de sus propios dígitos elevados a la potencia del número total de dígitos -}
narciso :: Int -> Bool
narciso n = (sumarDigitos n x == n)
    where x = longitud n

longitud :: Int -> Int
longitud n = length (show n)

sumarDigitos :: Int -> Int -> Int
sumarDigitos n longitud
    | (n > 0) = (n `mod` (10)) ^ longitud + sumarDigitos (n `div` 10) longitud
    | (n <= 0) = 0

{- Parcial 2024 -}
tParcial :: (Int, Int) -> Int -> String
tParcial (n, m) c
    | (n < m) && (n >= 1) && (c > 0) = show n ++ "\t" ++ show (sumatoria n c) ++ "\n" ++ tParcial (n + 1, m) c
    | (n == m) && (n >= 1) && (c > 0) = show n ++ "\t" ++ show (sumatoria n c)
    | otherwise = "Error al ingresar los datos"

{- /,% en float, div, mod en int -}
sumatoria :: Int -> Int -> Float
sumatoria k c
    | (k > 1) = (fromIntegral(arriba) / fromIntegral(abajo)) + sumatoria (k - 1) c
    | (k == 1) = (fromIntegral(arriba) / fromIntegral(abajo))
    | otherwise = 0
    where
        arriba = (k ^ 3) + (5 * c)
        abajo = (k * 2) + c - 1 

main :: IO ()
main = do
    putStrLn (tParcial (2, 4) 3)