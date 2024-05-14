import Euler
import Data.List
import Data.Ratio

isPhiPermutation :: Int -> Bool
isPhiPermutation = equal (sort . digits) (sort . digits . phi)

numbers :: [Int]
numbers = filter isPhiPermutation [2..10^7-1]

answer :: Int
answer = maximumVia (\x -> (toInteger $ phi x) % (toInteger x)) numbers

main :: IO ()
main = do putStrLn $ show answer
