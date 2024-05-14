import Euler
import Data.List

isSquareFree :: Integer -> Bool
isSquareFree n = f == uniq f where f = factor n

answer :: Integer
answer = sum . uniq . sort . filter isSquareFree $
         [choose n k | n <- [0..50], k <- [0..n]]

main :: IO ()
main = print answer
