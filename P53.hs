import Euler

numbers :: [Integer]
numbers = [choose n r | n <- [1..100], r <- [0..n]]

answer :: Int
answer = length $ filter (>10^6) numbers

main :: IO ()
main = putStrLn . show $ answer
