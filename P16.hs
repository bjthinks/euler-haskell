import Euler

answer :: Int
answer = sum $ digits (2^1000)

main :: IO ()
main = putStrLn $ show answer
