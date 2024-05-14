import Euler

hasFourPrimeFactors :: Int -> Bool
hasFourPrimeFactors = (==4) . length . primeFactors

findFourConsecutive :: [Int] -> Int
findFourConsecutive (a:bs@(b:c:d:_))
  | b == a+1 && c == a+2 && d == a+3 = a
  | otherwise = findFourConsecutive bs

answer :: Int
answer = findFourConsecutive $ filter hasFourPrimeFactors [2..]

main :: IO ()
main = putStrLn $ show answer
