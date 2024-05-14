import Euler

answer :: Int
answer = (sumUpTo 100)^2 - sumSquaresUpTo 100

main :: IO ()
main = putStrLn $ show answer
