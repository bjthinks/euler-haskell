import Euler

answer :: Int
answer = sum $ digits $ factorial 100

main :: IO ()
main = putStrLn $ show answer
