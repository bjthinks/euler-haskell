p1 :: Integer -> Integer
p1 0 = 1
p1 n = sum [(-1)^(k-1) * p1 (n - div (k*(3*k+1)) 2) | k <- takeWhile (\x -> div (x*(3*x+1)) 2 <= n) [1..]] +
       sum [(-1)^(k-1) * p1 (n - div (k*(3*k-1)) 2) | k <- takeWhile (\x -> div (x*(3*x-1)) 2 <= n) [1..]]

-- p2 :: Integer -> Integer
