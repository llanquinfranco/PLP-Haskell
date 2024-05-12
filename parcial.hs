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