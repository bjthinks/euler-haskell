import Euler

answer :: Int
answer = primes !! 10000

main :: IO ()
main = putStrLn $ show answer
