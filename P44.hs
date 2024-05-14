import Euler

-- c = n(3n-1)/2
-- iff c = 3/2 n^2 - 1/2 n
-- iff 3/2 n^2 - 1/2 n - c = 0
-- iff n = (1/2 +/- sqrt(1/4 + 6c)) / 3
-- iff n = (1 +/- sqrt(1 + 24c)) / 6
-- iff 24c + 1 is a square and its square root is 5 mod 6
isPentagonal :: Int -> Bool
isPentagonal c = k == r^2 && mod r 6 == 5
  where
    k = 24*c + 1
    r = iSqrt k

pentagonalNumbers :: [Int]
pentagonalNumbers = [div (n*(3*n-1)) 2 | n <- [1..]]

inputs :: [(Int,Int)]
inputs = [(p,q) | p <- pentagonalNumbers, q <- takeWhile (<p) pentagonalNumbers]

candidates :: [(Int,Int)]
candidates = filter (\(p,q) -> isPentagonal (p+q) && isPentagonal (p-q)) inputs

-- Technically this may give us a solution that is not minimal.
answer :: Int
answer = uncurry (-) . head $ candidates

main :: IO ()
main = print answer
