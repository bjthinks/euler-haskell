import Euler

condition :: Int -> Bool
condition = both isPalindrome $ isPalindromeBase 2

answer :: Int
answer = sum $ filter condition [1..999999]

main :: IO ()
main = putStrLn $ show answer
