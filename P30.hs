import Euler

isSumOfFifthPowersOfDigits :: Int -> Bool
isSumOfFifthPowersOfDigits = equal id $ sum . map (\n -> n^5) . digits

-- A solution cannot exceed 999999, because 7*9^5 < 10^6.  In fact, the
-- sum of the fifth powers of the digits of 999999 is 6*9^5, so a solution
-- cannot exceed 6*9^5 = 354294.

answer :: Int
answer = sum $ filter isSumOfFifthPowersOfDigits [2..6*9^5]

main :: IO ()
main = putStrLn $ show answer
