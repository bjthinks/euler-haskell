import Data.Ratio

{-
ab / bc = a / c
(10a + b) / (10b + c) = a / c
c (10a + b) = a (10b + c)

ab / ca = b / c
(10a + b) / (10c + a) = b / c
c (10a + b) = b (10c + a)
-}

inputs :: [Ratio Int]
inputs = [(10*a+b) % (10*b+c) | a <- [1..9], b <- [1..9], c <- [0..9],
          c * (10*a+b) == a * (10*b+c),
          10*a+b < 10*b+c] ++
         [(10*a+b) % (10*c+a) | a <- [1..9], b <- [0..9], c <- [1..9],
          c * (10*a+b) == b * (10*c+a),
          10*a+b < 10*c+a]

answer :: Int
answer = denominator . product $ inputs

main :: IO ()
main = print answer
