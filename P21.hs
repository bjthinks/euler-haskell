import Euler

answer :: Int
answer = sum $ filter isAmicable [2..9999]

main :: IO ()
main = putStrLn $ show answer
