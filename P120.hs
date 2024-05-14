import Euler

-- Some algebra gives us this simplification
remainder :: Integer -> Integer -> Integer
remainder a n
  | isEven n = 2
  | otherwise = a * mod (2*n) a

rmax :: Integer -> Integer
rmax a = maximum . map (remainder a) $ [0..2*a]

answer :: Integer
answer = sum . map rmax $ [3..1000]

main :: IO ()
main = print answer
