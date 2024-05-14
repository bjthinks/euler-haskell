import IsPrime

{-
2 * blue * (blue - 1) = total * (total - 1)
x = total
y = blue
2 (y^2 - y) - (x^2 - x) = 0
2 (y^2 - y) + 1/2 = (x^2 - x) + 1/2
2 (y^2 - y + 1/4) = (x^2 - x + 1/4) + 1/4
2 (y - 1/2)^2 = (x - 1/2)^2 + (1/2)^2
Scale x and y by 2.  Now x and y have to be even.
2 (y/2 - 1/2)^2 = (x/2 - 1/2)^2 + (1/2)^2
2 (y - 1)^2 / 4 = (x - 1)^2 / 4 + 1 / 4
2 (y - 1)^2 = (x - 1)^2 + 1
If x is even, then RHS is even.  Can divide by 2.
(y - 1)^2 = ((x - 1)^2 + 1) / 2
So, need to find first even x > 2 * 10^12
such that ((x - 1)^2 + 1) / 2 is a square.
Then the answer is given by (sqrt(((x - 1)^2 + 1) / 2) + 1) / 2
Answer will be an integer if x is even.
Proof:
(x-1) is odd
(x-1)^2 is 1 mod 4
(x-1)^2+1 is 2 mod 4
((x-1)^2+1)/2 is odd
sqrt(((x-1)^2+1)/2) is odd
sqrt(((x-1)^2+1)/2)+1 is even
(sqrt(((x-1)^2+1)/2)+1) is an integer
-}

{-
values :: [(Integer, Integer)]
values = [(x, ((x-1)^2+1) `div` 2) | x <- [2*10^12,2*10^12+2..]]

goodValues :: [(Integer, Integer)]
goodValues = filter (isSquare . snd) values

isSquare :: Integer -> Bool
isSquare x = x == s*s where s = iSqrt x

The preceding code finds values of (x,((x-1)^2+1)/2) for which the
snd is a square.
Instead, we can look for collisions in a pair of sparse lists.
The first list is (x - 1)^2
The second list is (2 z^2 - 1)
This gets rid of the iSqrt calls...
-}

collisions :: (Ord a) => [a] -> [a] -> [a]
collisions (x:xs) (y:ys)
  | x < y = collisions xs (y:ys)
  | x > y = collisions (x:xs) ys
  | x == y = x : collisions xs ys

goodValues :: [Integer]
goodValues = collisions
             [(x^2+1) `div` 2 | x <- [firstx,firstx+2..]]
             [ y^2            | y <- [firsty,firsty+2..]]

firstx :: Integer
firsty :: Integer
firstx = 2*10^12-1
firsty = 1414213562373
--firstx = 1
--firsty = 1

answer :: Integer
answer = ((iSqrt $ head goodValues) + 1) `div` 2

main :: IO ()
main = putStrLn $ show answer
