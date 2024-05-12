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

sucesion :: Int -> Int -> Int
sucesion x n = (sumatoria n n) / (factorial x)

sumatoria :: Int -> Int -> Int
sumatoria i n



