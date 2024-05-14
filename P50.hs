import Euler

-- The sum of the first 546 primes is < 1,000,000.  The 547th puts the sum
-- over the one million limit.

answer :: Int
answer = head $ filter isPrime $ concat [takeWhile (<1000000) $ map sum $ substrs a primes | a <- [546,545..1]]

main :: IO ()
main = putStrLn $ show answer
