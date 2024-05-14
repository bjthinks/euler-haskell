import Euler

answer :: Int
answer = sum $ filter (divisible 2) $ takeWhile (<= 4000000) fibonacci

main :: IO ()
main = putStrLn $ show answer
