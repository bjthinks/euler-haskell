isAnswer :: Integer -> Bool
isAnswer n = length s == 19 &&
             s !! 0 == '1' &&
             s !! 2 == '2' &&
             s !! 4 == '3' &&
             s !! 6 == '4' &&
             s !! 8 == '5' &&
             s !! 10 == '6' &&
             s !! 12 == '7' &&
             s !! 14 == '8' &&
             s !! 16 == '9' &&
             s !! 18 == '0'
               where
                 s = show (n*n)

answer :: Integer
answer = head $ filter isAnswer [1000000000,1000000010..10000000000]

main :: IO ()
main = putStrLn $ show answer
