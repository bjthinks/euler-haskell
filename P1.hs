import Euler

condition :: Int -> Bool
condition = paste (||) (divisible 3) (divisible 5)

answer :: Int
answer = sum $ filter condition [1..999]

main :: IO ()
main = putStrLn $ show answer
