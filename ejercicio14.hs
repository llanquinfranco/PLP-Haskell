{- Defina una funcion entreLıneas la cual tome tres Strings y retorne un 
String que cuando se imprima muestre los tres Strings en lıneas separadas -}
entreLineas :: String -> String -> String -> String
entreLineas string1 string2 string3 = string1 ++ "\n" ++ string2 ++ "\n" ++ string3

main :: IO ()
main = do
    putStrLn "Ingrese la primer palabra"
    l <- readLn
    putStrLn "Ingrese la segunda palabra"
    m <- readLn
    putStrLn "Ingrese la tercer palabra"
    n <- readLn
    putStrLn (entreLineas l m n)