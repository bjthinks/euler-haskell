import Euler
import Data.List

indicesWith :: (a -> Bool) -> [a] -> [Int]
indicesWith p xs = indicesWith' 0 p xs
  where
    indicesWith' n p (x:xs)
      | p x       = n : indicesWith' (n+1) p xs
      | otherwise =     indicesWith' (n+1) p xs

fibonacciMod :: [Int]
fibonacciMod = fibModWith 0 1
  where
    fibModWith a b = a : fibModWith b (mod (a+b) (10^9))

panEndIndices :: [Int]
panEndIndices = indicesWith isGood fibonacciMod

isGood :: (Integral a) => a -> Bool
isGood = (==[1,2,3,4,5,6,7,8,9]) . sort . digits

fibonacciBeginnings :: [Int]
fibonacciBeginnings = fb (1.0 / sqrt 5.0)
  where
    fb x
      | x < 1.0e9 = (round x) : fb (x * gamma)
      | otherwise = fb (x / 10.0)
    gamma = (1.0 + sqrt 5.0) / 2.0

panBeginIndices :: [Int]
panBeginIndices = indicesWith isGood fibonacciBeginnings

inBoth :: (Ord a) => [a] -> [a] -> [a]
inBoth (x:xs) (y:ys)
  | x == y = x : inBoth xs ys
  | x < y = inBoth xs (y:ys)
  | x > y = inBoth (x:xs) ys

answer :: Int
answer = head $ inBoth panEndIndices panBeginIndices

main :: IO ()
main = putStrLn $ show answer
