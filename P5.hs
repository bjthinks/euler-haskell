answer :: Integer
answer = foldl lcm 1 [1..20]

main :: IO ()
main = putStrLn $ show answer
