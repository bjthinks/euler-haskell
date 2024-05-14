import Euler
import Data.List

sublists :: [a] -> [([a],[a])]
sublists [] = [([],[])]
sublists (x:xs) = map (\(a,b) -> (x:a,b)) ss ++
                  map (\(a,b) -> (a,x:b)) ss
                    where ss = sublists xs

numPrimesUsing :: [Int] -> Int
numPrimesUsing = length . filter isPrime . map undigits . permutations

minuslists :: Eq a => [a] -> [a] -> [a]
minuslists x [] = x
minuslists [] _ = error "list subtraction"
minuslists (x:xs) yy@(y:ys)
  | x == y = minuslists xs ys
  | otherwise = x : minuslists xs yy

numPrimeSetsUsing :: [Int] -> Int
numPrimeSetsUsing [] = 1
numPrimeSetsUsing dd@(d:ds) =
  sum . map (\(a,b) -> a * numPrimeSetsUsing b) .
  filter ((/=0) . fst) .
  map (\(a,b) -> (numPrimesUsing a,b)) .
  map (\(a,b) -> (d:a,b)) . sublists $ ds

answer :: Int
answer = numPrimeSetsUsing [1..9]

main :: IO ()
main = print answer
