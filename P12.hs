import Euler

answer :: Integer
answer = head $ filter ((>500) . numberOfDivisors) $ tail triangularNumbers

main :: IO ()
main = putStrLn $ show answer
