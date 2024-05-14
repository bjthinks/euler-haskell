-- consider a 16-17-17 isosceles triangle
-- pythagoras: height from base is 15 (sqrt(17^2-(16/2)^2))
-- height is 1 less than base!
-- for 272-305-305, the height is 273, one more than the base!
-- this is the second smallest isosceles triangle with h = b+1
-- find sum of the diagonal sides for the 12 smallest such triangles with
-- h = b +/- 1
-- (all sides integers)

-- we seek pythagorean triples (x,y,z) where
-- x = b/2
-- y = b +/- 1
-- z = anything
-- in such a case, y = 2x +/- 1 ==> y = +/- 1 mod x ==> primitive
-- brute force is looking too slow, so...
-- (b/2)^2 + (b +- 1)^2 must be a square
-- = b^2/4 + b^2 +- 2b + 1
-- = 5/4 b^2 +- 2b + 1 must be a square
-- b must be even
-- Still too slow!

-- Since b is even, let c = b/2
-- So 5c^2 +- 4c + 1 must be a square
-- 5(c^2 +- 4/5 c + 1/5) must be a square
-- Note: (c +- 2/5)^2 = c^2 +- 4/5 c + 4/25
-- So 5((c +- 2/5)^2 + 1/25) must be a square
-- So, new strategy: look for integer squares s for which
-- (s/5 - 1/25) is a rational square
-- equivalently, for which 5s-1 is an integer square

{-
1 or 2 + 10 + 13
that + 22 + 25
that + 34 + 37
-}
import Control.Monad (guard)
import Euler

isSquare :: Integer -> Bool
isSquare x = (iSqrt x)^2 == x

hypotenuse :: Integer -> [Integer]
hypotenuse b = do h <- [b-1,b+1]
                  let diagsqrd = (b `div` 2)^2 + h^2
                  guard (isSquare diagsqrd)
                  return (iSqrt diagsqrd)

happy :: [Integer]
happy = concat (map hypotenuse [2,4..])

squares :: [Integer]
squares = [n^2 | n <- [1..]]

isGoodSquare :: Integer -> Bool
isGoodSquare n = isSquare (5*n-1)

main :: IO ()
--main = mapM_ print happy
main = mapM_ print $ filter isGoodSquare $ drop 2 squares
