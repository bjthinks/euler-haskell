import Data.Ratio
import Control.Monad
import Data.List
import Euler

-- numbersFrom [1,2,3,4] -> [1,2..28,x,y,36]
numbersFrom :: [Int] -> [Ratio Int]
numbersFrom [] = []
numbersFrom [x] = [x%1]
numbersFrom xs = uniq . sort $ do
  (as,bs) <- splits xs
  guard $ bs /= []
  a <- numbersFrom as
  b <- numbersFrom bs
  op <- [(+), (-), (*)] ++ if b == (0%1) then [] else [(/)]
  return $ op a b

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = concat . map (\(ys,zs) -> [(x:ys,zs),(ys,x:zs)]) . splits $ xs

intsFrom :: [Int] -> [Int]
intsFrom = map numerator . filter (\x -> denominator x == 1 && x > 0) . numbersFrom

maxConsec :: [Int] -> Int
maxConsec = length . takeWhile (uncurry (==)) . zip [1..]

subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize 0 _ = [[]]
subsetsOfSize _ [] = []
subsetsOfSize n (x:xs) = map (x:) (subsetsOfSize (n-1) xs) ++ subsetsOfSize n xs

answer :: Int
answer = undigits . snd . maximum $ [(maxConsec (intsFrom xs),xs) |
                                     xs <- subsetsOfSize 4 $ [0..9]]

main :: IO ()
main = print answer
