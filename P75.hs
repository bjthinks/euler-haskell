import Euler
import Control.Monad
import Data.List

perimeters :: [Int]
perimeters = sort . map (\(a,b,c) -> a+b+c) .
             pythagoreanTriplesWithPerimeterUpTo $ 1500000

answer :: Int
answer = length . filter (\x -> length x == 1) . group $ perimeters

main :: IO ()
main = print answer
