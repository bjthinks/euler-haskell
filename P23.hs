import Euler
import qualified Data.Set as Set
import Data.Array

abundantTable :: Array Int Bool
abundantTable = listArray (1,28123) $ map isAbundant [1..28123]

isAbundantSum :: Int -> Bool
isAbundantSum n = or [abundantTable ! (n-a) | a <- takeWhile (<n) abundant]

answer :: Int
answer = sum $ filter (not . isAbundantSum) [1..28123]

main :: IO ()
main = putStrLn $ show answer
