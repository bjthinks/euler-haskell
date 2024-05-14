import Euler

smoothNumbers :: Integer -> [Integer] -> Integer -> [Integer]
smoothNumbers lim [] acc = [acc]
smoothNumbers lim (p:ps) acc
  | p*acc > lim = [acc]
  | otherwise = concat $ map (smoothNumbers lim ps)
                (takeWhile (<=lim) $ iterate (*p) acc)

answer :: Int
answer = length $ smoothNumbers (10^9) (takeWhile (<100) primes) 1

main :: IO ()
main = print answer
