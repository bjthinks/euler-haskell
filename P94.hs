import Euler
import Control.Monad
import Data.Ratio

-- Suppose the triangle with side lengths (a,a,a+-1) has integral area.
-- Then (3a+-1)/2 ((3a+-1)/2 - a)^2 ((3a+-1)/2 - (a+-1)) is a square
--      (3a+-1) ((3a+-1) - 2a)^2 ((3a+-1) - (2a+-2)) / 16 is a square
--      (3a+-1) (a+-1)^2 (a-+1) / 16 is a square
--      (3a+-1) (a+-1) (a^2-1) / 16 is a square
-- a must be odd
-- so (3a+-1) is even, (a+-1) is even, and 24 divides (a^2-1)
-- so numerator is a multiple of 96, so dividing by 16 is not a problem
-- so we need all a such that (3a+-1) (a-+1) is a square
-- or 3a^2 -+ 2a - 1 is a square
-- a=5, sign=first -> 75-10-1=64 CHECK

{- Correct but MUCH TOO SLOW
isSquare :: Integer -> Bool
isSquare n = n >= 0 && (iSqrt n)^2 == n

answer :: [(Integer,(Integer,Integer))]
answer = filter (isSquare . fst) $ [(3*a^2-s*2*a-1,(a,s)) | a <- [2..333333], s <- [-1,1]]
-}

-- From experimentation, it appears that the left and right halves of
-- each solution triangle are pythagorean right triangles.  Height is
-- rational, since base (a+-1) is integer, and
-- integer = area = height * base/2 .
-- And since (base/2)^2 + height^2 = side^2, and (base/2) and side are
-- integer, (height rational) => (height integer).

-- So we want pythagorean triples (b,c,a) where 2b = a +- 1

-- Here's another take on the problem.  We know that a solution
-- (b,c,a) must be integral, and that for such a solution,
-- b^2 + c^2 = a^2 and 2b = a +- 1
-- b^2 + c^2 = (2b -+ 1)^2
-- b^2 + c^2 = 4b^2 -+ 4b + 1
-- c^2 = 3b^2 -+ 4b + 1
-- c^2 = (3b^2 -+ 4b + 4/3) - 1/3
-- c^2 = (sqrt(3)b -+ 2/sqrt(3))^2 - 1/3
-- 3c^2 = (3b -+ 2)^2 - 1
-- Let y = 3b -+ 2, so y can be any integer not a multiple of 3, and
-- y mod 3 determines the sign.
-- 3c^2 = y^2 - 1
-- y^2 - 3c^2 = 1
-- This is Pell's Equation :)

-- import Data.List

-- list of solutions to x^2-dy^2=1
-- d -> [(x,y)]
solvePell :: Integer -> [(Integer,Integer)]
solvePell d = allSolutions (x1,y1)
  where
    allSolutions (xk,yk) = (xk,yk) : allSolutions (x1*xk+d*y1*yk,x1*yk+y1*xk)
    (x1,y1) = (\x -> (numerator x, denominator x)) .
              head . filter isSolution . convergents . cfracSqrtFull $ d
    isSolution r = (numerator r)^2-d*(denominator r)^2==1

answer :: Integer
answer = sum $ do
  (y,c) <- takeWhile ((<= 500000000) . fst) $ solvePell 3
  guard $ mod y 3 /= 0
  let s = if mod y 3 == 1 then 1 else (-1)
      b = div (y + 2*s) 3
      a = 2*b - s
  guard $ a > 1
  return $ 3*a+s

main :: IO ()
main = print answer
