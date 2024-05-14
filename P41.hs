import Euler
import Data.List

isPandigital :: Int -> Bool
isPandigital n = sort (digits n) == take (length $ digits n) [1..]

-- answer must be either 1-4 pandigital or 1-7 pandigital
-- to avoid being divisible by 3
answer :: Int
answer = maximum $ filter isPrime $ map undigits $ concat $ map permutations $
         [[1..7],[1..4]]

main :: IO ()
main = print answer
