-- Dar una definicion para la siguiente funcion ordenTriple la cual ordena los tres elementos en orden ascendente --
ordenTriple :: (Int,Int,Int) -> (Int,Int,Int)
ordenTriple (x, y, z)
    | (x >= y) && (y >= z) = (x, y, z)
    | (x >= z) && (z >= y) = (x, z, y)
    | (y >= x) && (x >= z) = (y, x, z)
    | (y >= z) && (z >= x) = (y, z, x)
    | (z >= x) && (x >= y) = (z, x, y)
    | (z >= y) && (y >= x) = (z, y, x)