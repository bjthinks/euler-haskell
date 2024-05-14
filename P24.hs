import Euler

inOrderPermutations :: [a] -> [[a]]
inOrderPermutations [] = [[]]
inOrderPermutations ys = iop' [] ys
  where
    iop' xs [] = []
    iop' xs (y:ys) = map (y:) (inOrderPermutations (reverse xs ++ ys)) ++
                     iop' (y:xs) ys

answer :: Integer
answer = undigits $ inOrderPermutations [0,1,2,3,4,5,6,7,8,9] !! 999999

main :: IO ()
main = print answer
