-- Devuelve True excepto cuando sus dos argumentos son ambos True --
nAnd :: Bool -> Bool -> Bool
nAnd exp1 exp2
    | ((exp1 == True) && (exp2 == True)) = False
    | otherwise = True