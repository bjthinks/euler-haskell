import Euler

products :: [Int]
products = [a*b | a <- [100..999], b <- [100..999]]

answer :: Int
answer = maximum $ filter isPalindrome products

main :: IO ()
main = putStrLn $ show answer
