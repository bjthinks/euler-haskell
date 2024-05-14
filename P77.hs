import Euler

ways :: Int -> Int
ways n = waysUsing primes n
  where
    waysUsing pp@(p:ps) n
      | n == 0 = 1
      | p > n = 0
      | otherwise = waysUsing pp (n-p) + waysUsing ps n

answer :: Int
answer = head $ filter ((>5000) . ways) $ [1..]

main :: IO ()
main = putStrLn $ show answer
