-- for pythagorean right triangles with perimeter up to 100 million,
-- how many such (a,b,c), a<b<c, have the property that b-a divides c?

import Euler

isGood :: (Integer,Integer,Integer) -> Bool
isGood (a,b,c) = divides c (b-a)

prim :: [(Integer,Integer,Integer)]
prim = primitivePythagoreanTriplesWithPerimeterUpTo (10^8)

goodPrim :: [(Integer,Integer,Integer)]
goodPrim = filter isGood prim

perimeter :: (Integer,Integer,Integer) -> Integer
perimeter (a,b,c) = a+b+c

countsFor :: (Integer,Integer,Integer) -> Integer
countsFor t = (10^8) `div` perimeter t

answer :: Integer
answer = sum $ map countsFor goodPrim

main :: IO ()
main = print answer
