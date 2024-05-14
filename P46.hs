import Euler

oddComposites :: [Int]
oddComposites = complement [3,5..] (tail primes)
  where
    complement :: Eq a => [a] -> [a] -> [a]
    complement (x:xs) yy@(y:ys)
      | x == y = complement xs ys
      | otherwise = x : complement xs yy

-- True if a number is a sum of a prime and twice a square
isGood :: Int -> Bool
isGood n = or [isPrime (n - 2*k^2) | k <- [1..iSqrt (div n 2)]]

answer :: Int
answer = head $ filter (not . isGood) oddComposites

main :: IO ()
main = print answer
