import Euler

f :: Integer -> Integer
f n
  | isEven n  = div n 2
  | otherwise = 3*n + 1

chainLength :: Integer -> Int
chainLength 1 = 1
chainLength n = 1 + chainLength (f n)

answer :: Integer
answer = maximumVia chainLength [1..999999]

main :: IO ()
main = putStrLn $ show answer
