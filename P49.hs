import Euler
import Data.List

spork :: [(Int,Int)]
spork = [(a,b) | a <- [1001..9997], b <- [a+2..9999]]

sporkFilter :: (Int,Int) -> Bool
sporkFilter (x,y) =
  z < 10000 &&
  isPrime x &&
  isPrime y &&
  isPrime z &&
  sort (digits x) == sort (digits y) &&
  sort (digits x) == sort (digits z)
    where
      z = 2*y-x

answer :: String
answer = show x ++ show y ++ show z
  where
    (x,y) = head $ filter (/=(1487,4817)) $ filter sporkFilter spork
    z = 2*y-x

main :: IO ()
main = putStrLn answer
