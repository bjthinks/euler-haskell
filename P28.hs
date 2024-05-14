import Euler

answer :: Int
answer = 16 * sumSquaresUpTo 500 + 4 * sumUpTo 500 + 4 * 500 + 1

main :: IO ()
main = putStrLn $ show answer
