import Euler

-- The number in question is (10^(10^9) - 1) / 9
-- We need to calculate it mod p
bignumModP :: Integer -> Integer
bignumModP p = ((powmod 10 (10^9) (9*p) - 1) `mod` (9*p)) `div` 9

isFactorOfBignum :: Integer -> Bool
isFactorOfBignum p = bignumModP p == 0

factorsOfBignum :: [Integer]
factorsOfBignum = filter isFactorOfBignum primes

answer :: Integer
answer = sum $ take 40 factorsOfBignum

main :: IO ()
main = print answer
