import Data.List (sort)
import Data.Bits (Bits, xor)
import System.IO

mex :: (Integral a) => [a] -> a
mex = mex' 0 . sort
mex' :: (Integral a) => a -> [a] -> a
mex' v [] = v
mex' v (x:xs)
  | v == x = mex' (v+1) xs
  | v > x  = mex' v xs
  | v < x  = v


nimVals :: [Int]
nimVals = map nimVal [0..]

nimVal :: Int -> Int
nimVal n = mex $ map (nimVals !!) $ takeWhile (>=0) $ map (\x -> n - x) $
           [i*i | i <- [1..]]

answer = length $ filter (==0) $
         [(nimVals !! a) `xor` (nimVals !! b) `xor` (nimVals !! c) |
          a <- [0..300], b <- [a..300], c <- [b..300]]

main :: IO ()
main = putStrLn $ show answer
