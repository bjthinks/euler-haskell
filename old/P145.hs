isReversible :: Int -> Bool
isReversible x
  | x `mod` 10 == 0 = False
  | otherwise       = and $ map isOddDigit $ show $ x + reverseNumber x

reverseNumber :: Int -> Int
reverseNumber = read . reverse . show

isOddDigit :: Char -> Bool
isOddDigit '1' = True
isOddDigit '3' = True
isOddDigit '5' = True
isOddDigit '7' = True
isOddDigit '9' = True
isOddDigit _ = False

answer :: Int
answer = length $ filter isReversible [1..1000000000]

main :: IO ()
main = putStrLn $ show answer
