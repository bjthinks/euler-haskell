import Euler

countProdUpTo :: Int -> [Int] -> Int
countProdUpTo lim nn@(n:ns)
  | n*n > lim = 0
  | otherwise = length (takeWhile (<=div lim n) nn) + countProdUpTo lim ns

answer :: Int
answer = countProdUpTo (10^8) primes

main :: IO ()
main = print answer
