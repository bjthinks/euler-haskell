import Euler

-- The number in question is (10^(10^n) - 1) / 9
-- We need to calculate it mod p
bignumModP :: Int -> Integer -> Integer
bignumModP n p = ((powmod 10 (10^n) (9*p) - 1) `mod` (9*p)) `div` 9

isFactorOfBignum :: Int -> Integer -> Bool
isFactorOfBignum n p = bignumModP n p == 0

factorsOfBignum :: Int -> [Integer]
factorsOfBignum n = takeWhile (<100000) $ filter (isFactorOfBignum n) primes

-- Trial and error: above function stabilizes at 16
answer :: Integer
answer = sum (takeWhile (<100000) primes) - sum (factorsOfBignum 16)

main :: IO ()
main = print answer
