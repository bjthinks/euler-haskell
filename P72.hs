import Euler

answer :: Integer
answer = sum $ map phi [2..1000000]

main :: IO ()
main = putStrLn $ show answer
