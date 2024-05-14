import Data.List (sort)
import Euler

-- Uses the fact that n and p are coprime
-- otherwise, p divides n, and n divides k^3
--   where n^2 (n+p) = (n+k)^3  (just look mod n to see)
-- so p would divide k; let k = mp
-- but then (n+k)^3 = (n+mp)^3 > (n+p)^3 > n^3 + n^2 p, contradiction

-- since n and p are coprime, so are n and n+p
-- and since n^2(n+p) is a cube, both n and n+p must be cubes.
-- So p is a difference of cubes.
-- Conversely, for any p a difference of cubes, there is an n so that
-- n and n+p are both cubes.

-- So we just want primes under a million that are a difference of cubes

cubes :: [Integer]
cubes = [n^3 | n <- [1..600]]

cubediffs :: [Integer]
cubediffs = [x-y | x <- cubes, y <- cubes, x>y]

answer :: Int
answer = length $ filter isPrime $ filter (<1000000) $ uniq $ sort cubediffs

main :: IO ()
main = print answer
