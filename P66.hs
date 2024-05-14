import Euler
import Data.Ratio
import Data.List

-- list of solutions to x^2-Dy^2=1
-- D -> [(x,y)]
solvePell :: Integer -> [(Integer,Integer)]
solvePell d = allSolutions (x1,y1)
  where
    allSolutions (xk,yk) = (xk,yk) : allSolutions (x1*xk+d*y1*yk,x1*yk+y1*xk)
    (x1,y1) = (\x -> (numerator x, denominator x)) .
              head . filter isSolution . convergents . cfracSqrtFull $ d
    isSolution r = (numerator r)^2-d*(denominator r)^2==1

nonSquares :: [Integer]
nonSquares = [d | d <- [2..1000], d /= (iSqrt d)^2]

values :: [(Integer,Integer)]
values = [(fst . head . solvePell $ d, d) | d <- nonSquares]

answer :: Integer
answer = snd . last . sort $ values

main :: IO ()
main = print answer
