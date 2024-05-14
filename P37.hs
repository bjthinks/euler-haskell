import Euler

leftTruncations :: Int -> [Int]
leftTruncations = tail . takeWhile (/=0) . iterate removeLastDigit

removeLastDigit :: Int -> Int
removeLastDigit = flip div 10

rightTruncations :: Int -> [Int]
rightTruncations = tail . takeWhile (/=0) . iterate removeFirstDigit

removeFirstDigit :: Int -> Int
removeFirstDigit n = mod n (10 ^ (numberOfDigits n - 1))

numberOfDigits :: Int -> Int
numberOfDigits 0 = 0
numberOfDigits n = 1 + numberOfDigits (div n 10)

isListPrime :: [Int] -> Bool
isListPrime = and . map isPrime . reverse

isTruncatablePrime :: Int -> Bool
isTruncatablePrime = both (isListPrime . leftTruncations) (isListPrime . rightTruncations)

answer :: Int
answer = sum $ take 11 $ filter isTruncatablePrime $ drop 4 primes

main :: IO ()
main = putStrLn $ show answer
