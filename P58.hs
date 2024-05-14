import Euler

numbersArePrime :: [[Bool]]
numbersArePrime = map (map isPrime) [[k^2-3*k+3,k^2-2*k+2,k^2-k+1,k^2] | k <- ([3,5..] :: [Integer])]

answer :: Int
answer = 1 + 2 * answer' numbersArePrime 1 0
  where
    answer' :: [[Bool]] -> Int -> Int -> Int
    answer' (b:bs) n k
      | n > 10*k && k /= 0 = 0
      | otherwise = 1 + answer' bs (n+4) (k+length (filter id b))

main :: IO ()
main = putStrLn $ show answer
