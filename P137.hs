-- consider AF(x) = x + x^2 + 2x^3 + 3x^4 + 5x^5 + 8x^6 + ...
-- coefficients are fibonacci numbers
-- Solving AF(x) = n, for n integer, yields these solutions:
-- AF(x) x
-- 1     sqrt(2)-1
-- 2     1/2
-- 3     (sqrt(13)-2)/3
-- 4     (sqrt(89)-5)/8
-- 5     (sqrt(34)-3)/5
-- Call an integer AF(x) a golden nugget if x is rational
-- the first golden nugget is 2 and the tenth is 74049690
-- Find the 15th golden nugget

import Euler

-- I don't know why this is correct, but evidently the golden nuggets
-- are sums of every 4th Fibonacci number
-- 2, 15, 104, 714, 4895...
-- I noted this by decomposing 74049690 as a sum of Fibonaccis...

answer :: Integer
answer = sum [fibonacci !! k | k <- [3,7..4*15]]

main :: IO ()
main = print answer
