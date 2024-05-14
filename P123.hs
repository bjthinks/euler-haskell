import Euler

f :: (Integer,Integer) -> Integer
f (n,p) = mod ((-1)^n + n*p*(-1)^(n-1) + 1 + n*p) (p^2)

answer :: Integer
answer = fst . head . filter (\x -> f x > 10^10) . zip [1..] $ primes

main :: IO ()
main = print answer
