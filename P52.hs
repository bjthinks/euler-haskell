import Euler
import Data.List

isGood :: Int -> Bool
isGood x = (==1) . length . uniq . map (sort . digits) $ [n*x | n <- [1..6]]

answer :: Int
answer = head . filter isGood $ [1..]

main :: IO ()
main = print answer
