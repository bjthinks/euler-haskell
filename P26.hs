import Data.List

-- Successive (q,r) when computing a reciprocal's decimal expansion
-- e.g.
-- 2 -> [(5,0),(0,0)..]
-- 3 -> [(3,1)..]
-- 4 -> [(2,2),(5,0),(0,0)..]
-- 7 -> [(1,3),(4,2),(2,6),(8,4),(5,5),(7,1)]
decimalExpansionStates :: Int -> [(Int,Int)]
decimalExpansionStates n = des' n 1
  where
    des' n k = (div k' n,k'mn) : des' n k'mn
      where
        k'mn = mod k' n
        k' = 10*k

cycleLength :: (Eq a) => [a] -> Int
cycleLength xs = cl [] xs
  where
    cl js (i:is)
      | elem i js = k + 1
      | otherwise = cl (i:js) is
        where
           Just k = elemIndex i js

answer :: Int
answer = snd . maximum $
         zip (map (cycleLength . decimalExpansionStates) [2..999]) [2..999]

main :: IO ()
main = print answer
