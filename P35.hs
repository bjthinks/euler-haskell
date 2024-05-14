import Euler

isCircularPrime n
  | elem n [2,3,5,7] = True
  | n < 10 = False
  | elem 2 d || elem 4 d || elem 5 d || elem 6 d || elem 8 d = False
  | otherwise = and . map (isPrime . undigits) . rotations $ d
    where
      d = digits n

rotations :: [a] -> [[a]]
rotations xs = r xs []
  where
    r [] ys = []
    r xx@(x:xs) ys = (xx ++ reverse ys) : r xs (x:ys)

answer :: Int
answer = length $ filter isCircularPrime $ takeWhile (<10^6) primes

main :: IO ()
main = print answer
