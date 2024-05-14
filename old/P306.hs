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
nimVal n = mex $ zipWith xor nimVals (reverse $ take (n-1) nimVals)

answers :: [Int]
answers = map (length . filter (/=0) . tail) [f nimVals | f <- map take ([6,51] ++ [1001,2001..1000001])]

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn $ show answers
