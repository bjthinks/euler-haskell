import Euler

intersect :: Ord a => [a] -> [a] -> [a]
intersect xx@(x:xs) yy@(y:ys)
  | x < y = intersect xs yy
  | x > y = intersect xx ys
  | otherwise = x : intersect xs ys

answer :: Integer
answer = (foldl1 intersect $ map polygonalNumbers [3,5,6]) !! 3

main :: IO ()
main = print answer
