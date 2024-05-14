import Euler
import Data.List

perimeters :: [Int]
perimeters = sort . map (\(a,b,c) -> a+b+c) .
             pythagoreanTriplesWithPerimeterUpTo $ 999

answer :: Int
answer = head . maximumVia length . group . sort $ perimeters

main :: IO ()
main = print answer
