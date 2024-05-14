import Euler

answer :: Integer
answer = last $ factor 600851475143

main :: IO ()
main = putStrLn $ show answer
