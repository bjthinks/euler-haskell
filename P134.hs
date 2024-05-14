-- p1,p2 are consecutive primes
-- for each such pair with 5 <= p1 < 10^6
-- find the smallest n
-- such that the final digits of n are p1
-- and n is divisible by p2
-- sum all these n to get the answer

import Euler

candidates :: [(Integer,Integer)]
candidates =
  dropWhile ((<5) . fst) $
  takeWhile ((<10^6) . fst) $
  scanl (\(_,p1) p2 -> (p1,p2)) (0,0) primes

n :: (Integer,Integer) -> Integer
n (p1,p2) = increment * multiple + p1
  where
    increment = head $ filter (>p1) [10^n | n <- [1..]]
    multiple = (modularInverse p2 increment * (-p1)) `mod` p2

answer :: Integer
answer = sum $ map n candidates

main :: IO ()
main = print answer
