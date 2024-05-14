import Euler

-- Given the factors of n, compute the number of factors of n^2
numDivSqrFromFact :: (Eq a) => [a] -> Int
numDivSqrFromFact [] = 1
numDivSqrFromFact xx@(x:_) = (2 * length xfacs + 1) * numDivSqrFromFact rest
  where
    (xfacs,rest) = span (==x) xx

answer :: Integer
answer = head $ filter ((>=2000) . numDivSqrFromFact . factor) [1..]

main :: IO ()
main = print answer
