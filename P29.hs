import Euler
import Data.List

answer :: Int
answer = length . uniq $ sort [a^b | a <- [2..100], b <- [2..100]]

main :: IO ()
main = print answer
