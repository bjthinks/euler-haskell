import Euler
import Data.List

inputs :: [(Int,Int)]
inputs = [(a,b) | a <- [1..9], b <- [1000..9999]] ++
         [(a,b) | a <- [10..99], b <- [100..999]]

isGood :: Int -> Int -> Bool
isGood a b = [1,2,3,4,5,6,7,8,9] == sort (concat . map digits $ [a,b,a*b])

answer :: Int
answer = sum . uniq . sort . map (uncurry (*)) . filter (uncurry isGood) $
         inputs

main :: IO ()
main = print answer
