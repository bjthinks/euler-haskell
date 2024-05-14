import Euler

powers :: [(Integer,Integer)]
powers = generateInOrder powSuccs (uncurry (^)) (2,2)

-- a = sum of digits of a^b < 9 * (1 + log10 a^b)
-- so we can stop searching for b-th powers that are solutions when
-- a > 9 * (1 + log10 a^b)
-- a-9 > 9b log10 a
-- (a-9)/(9b) > log10 a
-- 10^((a-9)/(9b)) > a
-- a >= 9 and 10^(div (a-9) (9b)) > a

powSuccs :: (Integer,Integer) -> [(Integer,Integer)]
powSuccs (2,b) = [(3,b),(2,b+1)]
powSuccs (a,b)
  | a >= 9 && 10^(div (a-9) (9*b)) > a = []
  | otherwise = [(a+1,b)]

isGood :: Integer -> Integer -> Bool
isGood a b = sum ((map toInteger . digits) (a^b)) == a

answer :: Integer
answer = (uniq . map (uncurry (^)) . filter (uncurry isGood) $ powers) !! 29

main :: IO ()
main = print answer
