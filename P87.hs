import Euler
import Control.Monad
import Data.List

limit :: Int
limit = 50 * 10^6

numbers :: [Int]
numbers = do c <- takeWhile (<=84) primes
             b <- takeWhile (<=368) primes
             guard (b^3 + c^4 < limit)
             a <- takeWhile (<=7071) primes
             guard (a^2 + b^3 + c^4 < limit)
             return (a^2 + b^3 + c^4)

-- Sorting is the most time consuming part
answer :: Int
answer = length $ uniq $ sort numbers

main :: IO ()
main = putStrLn $ show answer
