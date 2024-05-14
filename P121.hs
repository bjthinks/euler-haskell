import Data.Ratio

type Rat = Ratio Integer

-- bork a b = probability of winning the game with a turns
-- in which at least b blue discs must be drawn
bork :: Integer -> Integer -> Rat
bork 0 b
  | b <= 0 = 1%1
  | otherwise = 0%1
bork a b = (1%(a+1)) * bork (a-1) (b-1) + (a%(a+1)) * bork (a-1) b

answer :: Integer
answer = div (denominator p) (numerator p)
  where p = bork 15 8

main :: IO ()
main = print answer
