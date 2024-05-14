import Euler

answer :: Integer
answer = sum $ map toInteger $ takeWhile (< 2000000) (primes :: [Int])

main :: IO ()
main = putStrLn $ show answer
