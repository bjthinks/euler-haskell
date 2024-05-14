import Euler
import Data.List

isKooky :: [Int] -> Bool
isKooky n =
  d1 /= 0 &&
  divides (100*d8+10*d9+d10) 17 &&
  divides (9*d7+10*d8+d9) 13 &&
  divides (d6-d7+d8) 11 &&
  divides (2*d5+3*d6+d7) 7 &&
  divides d6 5 &&
  divides (d3+d4+d5) 3 &&
  divides d4 2
    where
      (d1:d2:d3:d4:d5:d6:d7:d8:d9:d10:[]) = n

answer :: Integer
answer = sum $ map undigits $ filter isKooky $ permutations [0,1,2,3,4,5,6,7,8,9]

main :: IO ()
main = putStrLn $ show answer
