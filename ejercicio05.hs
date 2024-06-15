fibonacci :: Int -> Int
fibonacci n
    | (n == 0) = 0
    | (n == 1) = 1
    | (n > 1) = fibonacci(n - 1) + fibonacci(n - 2)

factorial :: Int -> Int
factorial n
    | (n == 0) = 1
    | (n == 1) = 1
    | (n > 1) = n * factorial(n - 1)

sucesion :: Int -> Int -> Float
sucesion x n = fromIntegral(sumatoria n n) / fromIntegral(factorial x)

sumatoria :: Int -> Int -> Int
sumatoria i n
    | (i >= 1) = (i ^ n) + sumatoria (i - 1) n
    | otherwise = 0