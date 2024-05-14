-- x, y, z consecutive positive members of an arithmetic progression
-- the smallest n so that x^2 - y^2 - z^2 = n has exactly two solutions is 27
-- 34 27 20 and 12 9 6
-- the least n with ten solutions is n = 1155
-- How many n less than a million have exactly 10 solutions?

-- Let's get some bounds...
-- x = a, y = a - b, z = a - 2b
-- then x^2 - y^2 - z^2
-- = a^2 - (a^2 - 2ab + b^2) - (a^2 - 4ab + 4b^2)
-- = -a^2 + 6ab - 5b^2
-- = - (a^2 - 6ab + 5b^2)
-- = - (a - b) (a - 5b)
-- So if b = a/5 or a, then we are at zero
-- So b must be between a/5 and a
-- and positive --> b < a/2
-- What if b is a/5+1/5 ?
-- = a^2 - (4/5 a - 1/5)^2 - (3/5 a - 2/5)^2
-- = a^2 - (16/25 a^2 - 8/25 a + 1/25) - (9/25 a^2 - 12/25 a + 4/25)
-- = 4/5 a - 1/5
-- this is out of range if a > 1,250,000
-- so a (or x) is in [1..1250001]
-- and b is in [a/5..(a-1)/2]
-- and usually b will be near the lower end of that range...

import Data.List (sort)

blet :: Integer -> [Integer]
blet a = [a^2 - (a-b)^2 - (a-2*b)^2 | b <- [(a `div` 5)+1..(a-1) `div` 2]]

ns :: [Integer]
ns = concat $ map (takeWhile (<10^6)) $ map blet [1..1250000]

-- Now the answer is integers that appear exactly 10 times in ns
countup :: Eq a => [a] -> [(Int,a)]
countup [] = []
countup (x:xs) = countup' x 1 xs
  where
    countup' x n [] = [(n,x)]
    countup' x n (y:ys)
      | x == y = countup' x (n+1) ys
      | otherwise = (n,x) : countup' y 1 ys

goodns :: [Integer]
goodns = map snd $ filter ((==10) . fst) $ countup $ sort ns

answer :: Int
answer = length goodns

main :: IO ()
main = print answer
