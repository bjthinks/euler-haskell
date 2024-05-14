import Euler
import Data.List

lim :: Int
lim = 10^8

squareSumsFrom :: Int -> [Int]
squareSumsFrom x = ssf (x^2+(x+1)^2) (x+2)
  where
    ssf t x = t : ssf (t+x^2) (x+1)

squareSums :: [Int]
squareSums = concat $ map (takeWhile (<lim) . squareSumsFrom) [1..iSqrt lim]

answer :: Integer
answer = sum . map toInteger . uniq . sort . filter isPalindrome $ squareSums

main :: IO ()
main = print answer
