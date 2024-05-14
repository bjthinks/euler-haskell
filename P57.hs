import Euler
import Data.Ratio

expansions :: [Ratio Integer]
expansions = tail . convergents . cfracSqrtFull $ 2

isGood :: Ratio Integer -> Bool
isGood = paste (>) (length . digits . numerator)
         (length . digits . denominator)

answer :: Int
answer = length . filter isGood . take 1000 $ expansions

main :: IO ()
main = print answer
