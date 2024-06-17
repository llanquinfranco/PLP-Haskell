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

{- 6) Definir una funcion que calcule la suma de los cuadrados de dos numeros
que recibe como argumentos, usando definiciones locales mediante where -}
sumaCuadrados :: Int -> Int -> Int
sumaCuadrados x y = cuadradoX + cuadradoY
    where
        cuadradoX = x * x
        cuadradoY = y * y

{- 12) Implementar una funcion que reciba un numero real y devuelva una tupla
con su parte entera y sus dos primeros decimales (com numeros enteros) -}
destrozaReal :: Float -> (Int, Int)
destrozaReal numero = (entera, decimal)
    where
        entera = floor(numero)
        decimal = floor((numero - fromIntegral(entera)) * 100)
        {- floor: Redondea hacia abajo al entero más cercano -}

{- 13) Diseñar un funcion que reciba una tupla de numeros enteros y 
devuelva el cociente y el resto de la division entera, junto con un 
mensaje de que la operacion se realizo de manera exitosa -}
division :: (Int, Int) -> String
division (x, y)
    | (x /= 0 && y == 0) = "Infinito (x/0)"
    | (x == 0 && y == 0) = "Indeterminado (0/0)"
    | otherwise = show (cociente, resto)
    where
        cociente = x `div` y
        resto = x `mod` y

{- 14) Diseñar una funcion que reciba el radio de una circunferencia y 
devuelva una tupla con la longitud de la circunferencia y el area del circulo -}
circunferencia :: Float -> (Float, Float)
circunferencia radio = (longitud, area)
    where
        longitud = 2 * pi * radio
        area = pi * (radio ^ 2)